# _targets.R - reproducible pipeline -------------------------------------------
# `targets::tar_make()` executes this DAG. Each target is cached on disk, so a
# rerun only recomputes what changed.
#
#   make all      # full pipeline
#   make test     # unit tests
#   make serve    # launch the dashboard
#
# STRUCTURAL NOTE: every tar_target() below lives inside a single list(). With
# targets 1.12 under R 4.6, bare top-level tar_target() calls register nothing
# and the pipeline reports success having run zero targets. Verified against a
# minimal reproduction before writing this file.

library(targets)

source(file.path("R", "00_config.R"), local = FALSE)
source(file.path("R", "utils.R"),    local = FALSE)
source(file.path("R", "01_data.R"),   local = FALSE)
source(file.path("R", "02_features.R"), local = FALSE)
source(file.path("R", "03_rfm.R"),    local = FALSE)
source(file.path("R", "04_cluster.R"), local = FALSE)
source(file.path("R", "05_clv.R"),    local = FALSE)
source(file.path("R", "06_report.R"), local = FALSE)

set.seed(CFG$seed)

# Targets run in fresh R subprocesses, so each one needs its packages attached
# explicitly. The source() calls above define functions in the _targets.R
# session only; without this the workers see the functions but not dplyr etc.
tar_option_set(packages = c("dplyr", "tidyr", "lubridate", "ggplot2",
                            "readxl", "cluster"))

list(

  # ---- Stage 1: data ------------------------------------------------------
  tar_target(raw, load_raw()),

  tar_target(clean, {
    res <- clean_transactions(raw)
    log_head("Cleaning audit")
    print(unlist(res$audit))
    res$data
  }),

  tar_target(audit, clean_transactions(raw)$audit),

  tar_target(audit_rows, data.frame(
    Stage = c("Raw rows",
              "Missing CustomerID (dropped)",
              "Missing Description (dropped)",
              "Cancellations (dropped)",
              "Non-positive UnitPrice (dropped)",
              "Quantity outliers (invoice dropped)",
              "Final rows"),
    Rows = c(audit$raw_rows, audit$raw_missing_customerid,
             audit$raw_missing_description, audit$cancel_rows,
             audit$nonpos_price, audit$qty_outlier_rows, audit$clean_rows)
  )),

  # ---- Stage 2: features --------------------------------------------------
  tar_target(features, build_features(clean)),
  tar_target(lines,     features$lines),
  tar_target(customers, features$customers),

  tar_target(by_country, aggregate_by(lines, "Country")),
  tar_target(by_weekday, aggregate_by(lines, "weekday")),
  tar_target(by_hour,    aggregate_by(lines, "hour")),
  tar_target(by_month,   monthly_revenue(lines)),

  # ---- Stage 3: RFM --------------------------------------------------------
  tar_target(rfm,             build_rfm(lines)),
  tar_target(rfm_scored,      score_rfm(rfm)),
  tar_target(snapshot_date,   max(lines$date)),
  tar_target(cluster_features_raw, rfm_cluster_features(rfm)),

  # Log-transformed AND standardised. Every clustering stage consumes this
  # matrix, so the diagnostics and the shipped fit cannot diverge.
  tar_target(cluster_features, cluster_matrix(cluster_features_raw)),

  # ---- Stage 4: clustering -------------------------------------------------
  tar_target(k_selection, select_k(cluster_features)),

  tar_target(K, choose_k(k_selection)),

  # Segment 1 is always the lowest-value group.
  tar_target(fit, fit_segments(cluster_features, k = K,
                               order_by = "monetary_log")),

  tar_target(eval_partition, evaluate_partition(cluster_features, k = K)),

  tar_target(segment_summary, summarise_segments(rfm, fit$labels)),

  # ---- Stage 5: CLV --------------------------------------------------------
  #
  # lifetime_days is measured from first_order to LAST purchase, not to the
  # snapshot: time spent inactive after a customer churns is not lifetime.
  tar_target(lifetimes, {
    rfm2 <- rfm %>% mutate(first_order = snapshot_date - tenure_days)
    lt <- build_lifetimes(rfm2, snapshot_date = snapshot_date)

    # Attach the day-level lifetime so the CLV purchase rate uses the same
    # denominator as the survival fit.
    lt$lifetime_days <- as.integer(difftime(lt$last_order, lt$first_order,
                                             units = "days"))
    lt
  }),

  # Lifetime fits use repeat customers only. A single-purchase customer has a
  # first and last purchase on the same day, which is a degenerate duration.
  # See build_lifetimes().
  tar_target(lifetime_cohort, lifetimes %>% filter(eligible_for_lifetime)),

  tar_target(km_survival, kaplan_meier(lifetime_cohort$lifetime_weeks,
                                       !lifetime_cohort$censored)),

  tar_target(weib, {
    w <- fit_weibull(lifetime_cohort$lifetime_weeks, !lifetime_cohort$censored)
    log_head("Weibull fit")
    cat(sprintf("  cohort = %d repeat customers (%.0f%% of base)\n",
                nrow(lifetime_cohort), 100 * nrow(lifetime_cohort) / nrow(lifetimes)))
    cat(sprintf("  shape = %.3f   scale = %.1f weeks\n", w$shape, w$scale))
    cat(sprintf("  churn observed = %d, right-censored = %d\n",
                w$n_event, w$n_censored))
    w
  }),

  tar_target(survival_check, {
    ag <- survival_agreement(lifetime_cohort$lifetime_weeks,
                             !lifetime_cohort$censored, weib)
    log_head("Parametric vs non-parametric survival")
    cat(sprintf("  Weibull vs Kaplan-Meier mean absolute gap = %.4f\n", ag))
    cat("  CLV uses the Kaplan-Meier curve, which assumes nothing about the\n")
    cat("  shape of the lifetime distribution. The Weibull is reported as a\n")
    if (ag > CFG$max_survival_gap) {
      cat(sprintf("  REJECTED fit: gap %.3f exceeds %.2f. The retention curve is a\n",
                  ag, CFG$max_survival_gap))
      cat("  mixture (fast churners plus a long-lived tail) that no single\n")
      cat("  Weibull can represent.\n")
      message(sprintf("Weibull rejected (gap %.3f > %.2f); CLV uses Kaplan-Meier.",
                      ag, CFG$max_survival_gap))
    } else {
      cat(sprintf("  acceptable fit (gap %.3f <= %.2f).\n",
                  ag, CFG$max_survival_gap))
    }
    validate_survival(lifetime_cohort$lifetime_weeks,
                      !lifetime_cohort$censored, weib)
  }),

  # CLV uses the purchase rate measured over OBSERVED lifetime, not over the
  # calendar time since first order. Using the latter would inflate the rate
  # for exactly the churned customers whose post-churn inactivity is counted.
  tar_target(clv_tbl, {
    # age_weeks     = weeks since FIRST purchase. This is the argument the
    #                 Kaplan-Meier curve is indexed by.
    # still_active  = last purchase within the censoring window. A customer
    #                 older than that has churned and is given zero expected
    #                 remaining activity.
    # rate          = orders per week of OBSERVED active lifetime (first to
    #                 last purchase), not per week of calendar tenure.
    d <- compute_clv(
      lifetimes %>%
        select(CustomerID, lifetime_days, lifetime_weeks, since_last_weeks,
               censored, frequency, monetary, avg_order_value) %>%
        transmute(CustomerID,
                  age_weeks        = pmax(1, lifetime_weeks),
                  since_last_weeks = since_last_weeks,
                  # `censored` = TRUE means "still active when the window closed", i.e. the
  # GOOD case. So still_active is `censored`, not its negation.
  still_active     = censored,
                  rate             = frequency / pmax(1, lifetime_weeks),
                  frequency        = frequency,
                  monetary         = monetary,
                  avg_order_value  = avg_order_value) %>%
        # recency/tenure_days are reporting columns; they do not enter the CLV
        # calculation, which is indexed by age_weeks.
        left_join(rfm %>% select(CustomerID, recency, tenure_days,
                                 observation_weeks),
                  by = "CustomerID"),
      km_survival)
    d$segment <- fit$labels
    assign_rfm_segments(d, rfm_scored)
    d$segment <- fit$labels
    assign_rfm_segments(d, rfm_scored)
  }),

  tar_target(clv_summary, clv_by_segment(clv_tbl, "segment")),

  tar_target(clv_rfm_summary, clv_by_segment(clv_tbl, "rfm_segment")),

  tar_target(cluster_sizes, as.data.frame(table(fit$labels))),

  # ---- Stage 6: outputs ----------------------------------------------------
  tar_target(results, list(
    audit           = audit,
    audit_rows      = audit_rows,
    k_selection     = k_selection,
    K               = K,
    cluster_sizes   = cluster_sizes,
    eval_partition  = eval_partition,
    segment_summary = segment_summary,
    weib            = weib,
    survival_check  = survival_check,
    clv_summary     = clv_summary,
    clv_rfm_summary = clv_rfm_summary,
    clv_tbl         = clv_tbl
  )),

  tar_target(tables, write_report_tables(results, customers, by_country,
                                         by_month, rfm_scored, clv_tbl,
                                         audit_rows)),

  tar_target(figures, write_figures(lines, results, clv_tbl)),

  tar_target(report, render_report(results, tables))

)
