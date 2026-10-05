# 06 - Report generation -------------------------------------------------------
# Everything here produces artefacts (markdown tables, PNG figures, a rendered
# HTML report). Numbers are computed, never transcribed by hand -- the
# contradictions in the original deck came from manual re-entry.

suppressPackageStartupMessages({
  library(ggplot2)
})

theme_clv <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", colour = PALETTE[["neutral"]]),
      plot.subtitle = element_text(colour = "grey35"),
      panel.grid.minor = element_blank(),
      axis.text  = element_text(colour = "grey25"),
      plot.margin = margin(12, 16, 12, 12)
    )
}

#' Attach the RFM segment name onto a CLV table for segment-level reporting
#' Attach the classical RFM segment to a CLV table
#'
#' The CLV table already carries `segment` (the k-means label), so the RFM
#' label is renamed to `rfm_segment` rather than silently overwriting it.
assign_rfm_segments <- function(clv_tbl, rfm_scored) {
  clv_tbl %>%
    left_join(
      rfm_scored %>%
        select(CustomerID, r_score, f_score, m_score, rfm_score,
               rfm_segment = segment),
      by = "CustomerID"
    )
}

# ---- Figures -----------------------------------------------------------------

#' Revenue and orders by month
#'
#' Revenue as columns, orders as a line on a secondary axis. Side-by-side
#' dodged columns need a bar width that fits the month spacing, and the natural
#' choice overlaps at this date resolution.
fig_monthly <- function(by_month) {
  # Rescale orders onto the revenue axis so both fit one panel. Bound outside
  # aes() so ggplot2 does not treat the derived columns as columns to plot.
  scale <- max(by_month$revenue) / max(by_month$orders)

  ggplot(by_month, aes(month_start)) +
    geom_col(aes(y = revenue, fill = "Revenue"), width = 25, colour = NA) +
    geom_line(aes(y = orders * scale, colour = "Orders"), linewidth = 0.8) +
    geom_point(aes(y = orders * scale, colour = "Orders"), size = 1.8) +
    scale_fill_manual(values = c(Revenue = PALETTE[["muted"]]), guide = "none") +
    scale_colour_manual(values = c(Orders = PALETTE[["secondary"]]), name = NULL) +
    scale_y_continuous(labels = function(x) fmt_money(x, 0),
                       expand = expansion(mult = c(0, 0.05))) +
    scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
    labs(title = "Revenue and orders by month",
         subtitle = "The November-December gift season dominates the year",
         x = NULL, y = "Revenue") +
    theme_clv() +
    theme(legend.position = "top",
          axis.text.x = element_text(angle = 45, hjust = 1))
}

#' Revenue by country, UK excluded
fig_country <- function(by_country, top_n = 15) {
  d <- by_country %>%
    filter(Country != "United Kingdom") %>%
    arrange(desc(revenue)) %>%
    slice_head(n = top_n) %>%
    mutate(Country = factor(Country, levels = rev(Country)))

  ggplot(d, aes(revenue, Country)) +
    geom_col(fill = PALETTE[["accent"]], width = 0.7) +
    geom_text(aes(label = format(round(revenue / 1000), big.mark = ",")),
              hjust = -0.15, size = 3, colour = "grey30") +
    scale_x_continuous(labels = function(x) paste0(x / 1000, "k"),
                       expand = expansion(mult = c(0, 0.12))) +
    labs(title = paste0("Top ", top_n, " countries by revenue (UK excluded)"),
         subtitle = "The UK dominates total revenue; these are the international markets",
         x = "Revenue", y = NULL) +
    theme_clv()
}

#' The elbow/silhouette diagnostics, two panels
fig_k_selection <- function(k_sel, K) {
  ks <- k_sel

  wss_df <- ks$wss
  wss_df$k <- as.numeric(wss_df$k)
  sil_df <- ks$silhouette
  sil_df$k <- as.numeric(sil_df$k)

  p1 <- ggplot(wss_df, aes(k, wss)) +
    geom_line(linewidth = 0.9, colour = PALETTE[["muted"]]) +
    geom_point(colour = PALETTE[["muted"]], size = 1.6) +
    geom_vline(xintercept = ks$elbow_k, linetype = "dashed",
               colour = PALETTE[["secondary"]]) +
    geom_point(data = wss_df[wss_df$k == ks$elbow_k, ], size = 3.5,
               colour = PALETTE[["secondary"]]) +
    annotate("text", x = ks$elbow_k, y = max(wss_df$wss) * 0.9,
             label = paste("elbow k =", ks$elbow_k), hjust = 1.15,
             colour = PALETTE[["secondary"]], size = 3.5) +
    scale_x_continuous(breaks = wss_df$k) +
    scale_y_continuous(labels = function(x) format(round(x / 1000), big.mark = ",")) +
    labs(title = "Elbow method",
         subtitle = paste("The elbow prefers k =", ks$elbow_k,
                          "- the curve is already flattening there"),
         x = "k", y = "Within-cluster sum of squares (thousands)") +
    theme_clv()

  p2 <- ggplot(sil_df, aes(k, silhouette)) +
    geom_line(linewidth = 0.9, colour = PALETTE[["accent"]]) +
    geom_point(colour = PALETTE[["accent"]], size = 1.6) +
    geom_vline(xintercept = K, linetype = "dashed",
               colour = PALETTE[["secondary"]]) +
    geom_point(data = sil_df[sil_df$k == K, ], size = 3.5,
               colour = PALETTE[["secondary"]]) +
    labs(title = "Silhouette width",
         subtitle = paste("Peaks at k =", K, "- a clear, decisive maximum"),
         x = "k", y = "Mean silhouette") +
    theme_clv()

  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(p1, p2, ncol = 2) +
      patchwork::plot_annotation(
        title = "Choosing the number of clusters",
        theme = theme(plot.title = element_text(face = "bold", size = 14))
      )
  } else {
    p1 + p2
  }
}

#' Cluster stability across bootstrap resamples
fig_stability <- function(k_sel) {
  s <- k_sel$stability
  s$k <- as.numeric(s$k)
  ggplot(s, aes(k, ari_mean)) +
    geom_linerange(aes(ymin = ari_p05, ymax = ari_mean),
                   colour = PALETTE[["muted"]], linewidth = 3.5) +
    geom_point(size = 2.4, colour = PALETTE[["primary"]]) +
    scale_x_continuous(breaks = s$k) +
    scale_y_continuous(limits = c(0, 1.02), expand = expansion(mult = c(0.01, 0.02))) +
    labs(title = "Cluster stability under bootstrap resampling",
         subtitle = "Adjusted Rand Index between the full fit and each refit; higher is more stable",
         x = "k", y = "Mean ARI (5th pct to mean)") +
    theme_clv()
}

#' Customer profile by segment
fig_segment_scatter <- function(clv_tbl) {
  d <- clv_tbl
  if (!"segment" %in% names(d)) d$segment <- factor(d$rfm_segment)

  ggplot(d, aes(recency, frequency, colour = segment, size = clv)) +
    geom_point(alpha = 0.35) +
    facet_wrap(~ segment, nrow = 1) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ","),
                       trans = "log1p") +
    scale_size_continuous(range = c(0.4, 4), labels = function(x)
      paste0(CFG$currency_symbol, format(round(x), big.mark = ","))) +
    scale_colour_manual(values = setNames(
      c(PALETTE[["primary"]], PALETTE[["secondary"]], PALETTE[["accent"]],
        PALETTE[["warn"]], PALETTE[["neutral"]]),
      levels(d$segment))) +
    guides(colour = "none", size = guide_legend(title = "Predicted CLV")) +
    labs(title = "Customer profile by segment",
         subtitle = "Recency vs purchase frequency (log scale); point size is predicted lifetime value",
         x = "Days since last purchase", y = "Orders") +
    theme_clv() +
    theme(legend.position = "bottom")
}

#' CLV by segment
fig_clv_segments <- function(clv_summary) {
  d <- clv_summary %>%
    mutate(segment = factor(segment, levels = rev(segment)))

  ggplot(d, aes(clv_total, segment)) +
    geom_col(fill = PALETTE[["primary"]], width = 0.65) +
    geom_text(aes(label = paste0(CFG$currency_symbol,
                                 format(round(clv_total / 1e6, 2), big.mark = ","),
                                 "M  (", round(100 * clv_share), "%)")),
                  hjust = -0.1, size = 3.2, colour = "grey25",
                  show.legend = FALSE) +
    scale_x_continuous(labels = function(x) paste0(x / 1e6, "M"),
                       expand = expansion(mult = c(0, 0.34))) +
    labs(title = "Predicted 12-month lifetime value by segment",
         subtitle = paste0("Sum of predicted CLV across all customers; ",
                           CFG$gross_margin * 100, "% assumed gross margin"),
         x = "Total predicted CLV", y = NULL) +
    theme_clv()
}

#' Survival curve: Weibull fit against Kaplan-Meier
fig_survival <- function(survival_check) {
  d <- survival_check
  gap <- mean(d$abs_gap)

  ggplot(d, aes(time)) +
    geom_line(aes(y = km_survival), colour = PALETTE[["neutral"]],
              linewidth = 1.4) +
    geom_line(aes(y = weibull_survival), colour = PALETTE[["secondary"]],
               linewidth = 1.1, linetype = "dashed") +
    scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"),
                       limits = c(0, 1)) +
    labs(
      title = "Customer retention, and a rejected parametric fit",
      subtitle = paste0(
        "Solid = Kaplan-Meier, the model behind every CLV figure here (no ",
        "distributional assumption).\n",
        "Dashed = Weibull MLE, fitted and rejected (mean gap ", round(gap, 2),
        "): retention drops fast then flattens,\n",
        "a mixture that no single Weibull can represent."),
      x = "Weeks since first purchase", y = "Share still active",
      caption = paste0("Repeat customers only. Single-purchase customers are ",
                       "excluded: their first and last purchase are the same ",
                       "day, so\nthey have no measurable duration.")) +
    theme_clv() +
    theme(plot.subtitle = element_text(lineheight = 1.25),
          plot.caption  = element_text(lineheight = 1.25, colour = "grey45"))
}

#' Write all figures to disk
write_figures <- function(lines, results, clv_tbl, dir = "figures") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  by_country <- aggregate_by(lines, "Country")
  by_month   <- monthly_revenue(lines)

  jobs <- list(
    "monthly-revenue"   = fig_monthly(by_month),
    "revenue-by-country" = fig_country(by_country),
    "k-selection"       = fig_k_selection(results$k_selection, results$K),
    "cluster-stability" = fig_stability(results$k_selection),
    "segment-profiles"  = fig_segment_scatter(clv_tbl),
    "clv-by-segment"    = fig_clv_segments(results$clv_summary),
    "retention-curve"   = fig_survival(results$survival_check)
  )

  paths <- character(length(jobs))
  names(paths) <- names(jobs)

  for (nm in names(jobs)) {
    p <- file.path(dir, paste0(nm, ".png"))
    ggsave(p, jobs[[nm]], width = 10, height = 6, dpi = 150, bg = "white")
    log_step("wrote %s", p)
    paths[[nm]] <- p
  }
  paths
}

# ---- Tables ------------------------------------------------------------------

#' Emit every report table as markdown
write_report_tables <- function(results, customers, by_country, by_month,
                                rfm_scored, clv_tbl, audit_rows) {
  dir.create("reports/tables", showWarnings = FALSE, recursive = TRUE)
  tbls <- list()

  a <- results$audit
  tbls$audit <- audit_rows

  # Headline figures
  money <- function(x, dp = 0) paste0(CFG$currency_symbol,
                                       format(round(x, dp), big.mark = ","))
  tbls$headline <- data.frame(
    Metric = c("Customers", "Invoices", "Products", "Countries",
               "Revenue (12 months)", "Average order value"),
    Value = c(length(unique(customers$CustomerID)),
              a$clean_invoices, a$clean_products, a$clean_countries,
              money(a$revenue),
              money(a$revenue / a$clean_invoices, 2))
  )

  # Segment summary with CLV joined in
  seg <- results$segment_summary %>%
    select(segment, customers, share_of_base, recency_median,
           frequency_median, monetary_median, aov_median, revenue_share) %>%
    left_join(
      results$clv_summary %>%
        select(segment, clv_total, clv_median, clv_share) %>%
        rename(clv_share_of_total = clv_share),
      by = "segment"
    )
  tbls$segments <- seg

  # Classical RFM segments with CLV
  rfm_seg <- clv_tbl %>%
    group_by(r_score, f_score, m_score) %>%
    summarise(customers = n(), median_recency = median(recency),
              median_frequency = median(frequency), median_monetary = median(monetary),
              total_clv = sum(clv), .groups = "drop") %>%
    arrange(desc(median_monetary))
  tbls$rfm_segments <- rfm_seg

  # Model diagnostics
  tbls$diagnostics <- data.frame(
    Check = c("Chosen k (silhouette)", "Elbow method",
              "Gap statistic", "Within-cluster SS (in-sample)",
              "Within-cluster SS (30% holdout)",
              "Silhouette (full data)", "Silhouette (held out)",
              "ARI train vs full fit",
              "Bootstrap stability (mean ARI)",
              "Weibull shape", "Weibull scale (weeks)",
              "KM vs Weibull mean gap"),
    Value = c(results$K,
              results$k_selection$elbow_k, results$k_selection$gap_k,
              round(results$eval_partition$wss_ratio_full, 3),
              round(results$eval_partition$wss_ratio_train, 3),
              round(results$eval_partition$silhouette_full, 3),
              round(results$eval_partition$silhouette_holdout, 3),
              round(results$eval_partition$ari_train_vs_full, 3),
              round(results$k_selection$stability$ari_mean[
                results$k_selection$stability$k == results$K], 3),
              round(results$weib$shape, 3), round(results$weib$scale, 1),
              round(mean(results$survival_check$abs_gap), 4))
  )

  for (nm in names(tbls)) {
    path <- file.path("reports/tables", paste0(nm, ".md"))
    writeLines(to_md_table(tbls[[nm]]), path)
    log_step("wrote %s", path)
  }

  tbls
}

# ---- Report ------------------------------------------------------------------

#' Render the analysis report to HTML
render_report <- function(results, tables, rmd = "reports/analysis.Rmd") {
  # These are errors, not skips. The previous version logged "skipping render"
  # and returned NA, which targets reported as a completed target producing no
  # output -- the same silent-success failure as the empty code chunks in the
  # original notebook. A missing or unrenderable report must fail the pipeline.
  if (!file.exists(rmd)) {
    stop("report source missing: ", rmd,
         ". The report target must either render or fail loudly.")
  }
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("rmarkdown is not installed; cannot render the report.")
  }

  # The report cannot call tar_load() itself: targets forbids reading the data
  # store from inside a running target. Objects are injected here instead, via
  # render()'s envir argument.
  env <- new.env(parent = globalenv())
  env$results      <- results
  env$tables       <- tables
  # rmarkdown knits with the working directory set to the .Rmd's own folder, so
  # any path in the report must be absolute. Inject the project root rather
  # than letting the report guess.
  env$project_root <- normalizePath(".")

  log_step("rendering %s", rmd)
  out <- rmarkdown::render(
    rmd,
    output_dir = "reports",
    quiet = TRUE,
    envir = env
  )

  if (!file.exists(out)) {
    stop("rmarkdown::render() returned without producing ", out)
  }
  log_step("wrote %s (%.0f KB)", out, file.size(out) / 1024)
  out
}
