# 03 - RFM feature engineering -------------------------------------------------
#
# NOTE ON A CORRECTED BUG
# The original notebook computed frequency as:
#
#   amount_products <- d %>% select(CustomerID, InvoiceDate) %>%
#     group_by(CustomerID, InvoiceDate) %>% summarize(n_prod = n())
#   frequency <- amount_products %>% group_by(CustomerID) %>% summarize(frequency = n())
#
# `InvoiceDate` is a full timestamp, so two orders on the same day at
# different times counted as two separate purchases. The correct unit of
# "frequency" is a distinct invoice. The helper below uses n_distinct(InvoiceNo),
# and test-rfm.R asserts this behaviour on a fixture where the two differ.

#' Build the RFM table
#'
#' @param lines cleaned line-level data
#' @param snapshot_date reference date for recency. Derived from the data
#'   rather than hardcoded -- the original used a literal "2011-12-09".
build_rfm <- function(lines, snapshot_date = NULL) {
  if (is.null(snapshot_date)) snapshot_date <- max(lines$date)
  log_step("RFM snapshot date: %s", snapshot_date)

  # ---- Frequency: distinct invoices ----------------------------------------
  freq <- lines %>%
    group_by(CustomerID) %>%
    summarise(frequency = n_distinct(InvoiceNo), .groups = "drop")

  # ---- Recency and monetary, with order-level granularity -----------------
  orders <- lines %>%
    group_by(CustomerID, InvoiceNo) %>%
    summarise(order_date = max(date), order_value = sum(Spent), .groups = "drop")

  recency <- orders %>%
    group_by(CustomerID) %>%
    summarise(
      recency = as.integer(snapshot_date - max(order_date)),
      monetary = sum(order_value),
      first_order = min(order_date),
      .groups = "drop"
    )

  rfm <- recency %>%
    inner_join(freq, by = "CustomerID") %>%
    mutate(
      tenure_days = as.integer(snapshot_date - first_order),
      avg_order_value = monetary / frequency,
      # Calendar weeks of observed activity, floor at 1 so the BG/NBD
      # calibration window for a same-day customer is well defined.
      observation_weeks = pmax(1, ceiling(tenure_days / 7))
    ) %>%
    select(-first_order)

  log_step("RFM table: %d customers", nrow(rfm))
  rfm
}

#' Classical 5-bin RFM scoring and segment labelling
#'
#' Scores are 1 (worst) to 5 (best). Segment names follow the standard
#' RFM taxonomy so the output is interpretable by a marketing audience.
score_rfm <- function(rfm, n_bins = 5) {
  # qcut can fail when a feature has too few distinct values; fall back to
  # a rank-based split which is monotone-equivalent.
  bin <- function(x) {
    q <- tryCatch(
      ntile(qcut(x, n_bins, labels = FALSE), n_bins),
      error = function(e) dplyr::ntile(x, n_bins)
    )
    as.integer(q)
  }

  scored <- rfm %>%
    mutate(
      r_score = bin(recency),
      f_score = bin(frequency),
      m_score = bin(monetary)
    ) %>%
    mutate(r_score = 6L - r_score)   # recent = better, so invert

  segment <- function(r, f, m) {
    out <- rep("Unclassified", length(r))
    # Champions: recent, frequent, high value
    out[r >= 4 & f >= 4 & m >= 4] <- "Champions"
    # Loyal: frequent buyers with good value
    out[r >= 3 & f >= 4] <- "Loyal Customers"
    out[r >= 4 & f <= 3 & m >= 3] <- "Recent Customers"
    out[r == 3 & f == 3] <- "Potential Loyalists"
    out[r <= 2 & f >= 3] <- "At Risk"
    out[r <= 2 & f <= 2 & m >= 3] <- "Cannot Lose Them"
    out[r <= 2 & f <= 2 & m <= 2] <- "Hibernating"
    out
  }

  scored %>%
    mutate(rfm_score = paste0(r_score, f_score, m_score),
           segment = segment(r_score, f_score, m_score))
}

#' Customer-level model features for clustering
#'
#' Cluster in log space. The original clustered raw recency/frequency/monetary
#' after scaling, which left one cluster ~9 SD out on monetary and made the
#' centres degenerate. log1p compresses the heavy right tail of frequency and
#' monetary while leaving recency's scale intact.
rfm_cluster_features <- function(rfm) {
  rfm %>%
    mutate(
      recency_log   = log1p(recency),
      frequency_log = log1p(frequency),
      monetary_log  = log1p(monetary),
      aov_log       = log1p(avg_order_value)
    )
}

#' Standardise the clustering features
#'
#' Returns a matrix, not a data frame, so that every downstream consumer is
#' forced to acknowledge the features are on a common scale. Previously
#' `select_k()` scaled internally while `fit_segments()` and
#' `evaluate_partition()` received raw columns, so the reported diagnostics
#' described a different model from the one that shipped -- which showed up as
#' degenerate clusters of 6 and 26 customers in the segment table.
cluster_matrix <- function(features, features_used = CFG$cluster_features) {
  x <- as.matrix(features[, features_used, drop = FALSE])
  storage.mode(x) <- "double"
  scale(x)
}
