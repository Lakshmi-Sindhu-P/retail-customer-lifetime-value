# plumber API -----------------------------------------------------------------
# Minimal scoring service. Reads the fitted model artefacts written by
# `make all`; does not retrain.
#
#   Rscript -e 'pr <- plumber::plumb("api/plumber.R"); pr$run(port = 8000)'
#
# Endpoints
#   GET  /health          liveness + model metadata
#   GET  /segments        segment-level CLV summary
#   POST /predict-clv     score one customer from raw RFM inputs
#   GET  /customers/:id   look up a scored customer

library(plumber)

source(file.path("..", "R", "00_config.R"))
source(file.path("..", "R", "utils.R"))
source(file.path("..", "R", "05_clv.R"))

targets::tar_load(c(clv_tbl, results, km_survival))

#* Return model metadata and health status
#* @get /health
function() {
  list(
    status = "ok",
    model = "Kaplan-Meier lifetime x observed purchase rate",
    lifetime_cohort = "repeat customers only (2+ orders)",
    weibull_shape = round(results$weib$shape, 4),
    weibull_scale_weeks = round(results$weib$scale, 2),
    weibull_accepted = mean(results$survival_check$abs_gap) <= CFG$max_survival_gap,
    weibull_mean_gap = round(mean(results$survival_check$abs_gap), 4),
    horizon_weeks = CFG$clv_horizon_weeks,
    gross_margin = CFG$gross_margin,
    margin_is_assumption = TRUE,
    segments = levels(factor(clv_tbl$segment)),
    customers_modelled = nrow(clv_tbl)
  )
}

#* Segment-level CLV summary
#* @get /segments
function() {
  as.list(results$clv_summary)
}

#* Score a single customer
#*
#* @param age_weeks Numeric weeks since the customer's FIRST purchase. This is
#*   what the lifetime model is indexed by.
#* @param frequency Numeric number of distinct orders.
#* @param avg_order_value Numeric mean order value.
#* @param still_active Logical. FALSE means churn was observed, in which case
#*   predicted CLV is 0.
#* @post /predict-clv
function(age_weeks = NULL, frequency = NULL, avg_order_value = NULL,
         still_active = TRUE, horizon_weeks = NULL) {
  if (is.null(age_weeks) || is.null(frequency) || is.null(avg_order_value)) {
    stop("age_weeks, frequency and avg_order_value are required", status = 400)
  }
  if (any(c(age_weeks, frequency, avg_order_value) < 0)) {
    stop("numeric inputs must be non-negative", status = 400)
  }
  if (frequency < 1) {
    stop("frequency must be at least 1", status = 400)
  }
  if (is.null(horizon_weeks)) horizon_weeks <- CFG$clv_horizon_weeks

  d <- data.frame(
    CustomerID = 1L,
    age_weeks = max(1, age_weeks),
    since_last_weeks = 0,
    still_active = isTRUE(still_active),
    rate = frequency / max(1, age_weeks),
    frequency = frequency,
    monetary = frequency * avg_order_value,
    avg_order_value = avg_order_value
  )

  out <- compute_clv(d, km_survival, horizon_weeks, CFG$gross_margin)

  list(
    expected_remaining_weeks = round(out$expected_remaining_weeks[1], 3),
    expected_transactions = round(out$clv_expected_txn[1], 4),
    value_per_transaction = round(out$clv_per_txn[1], 4),
    predicted_clv = round(out$clv[1], 2),
    currency = CFG$currency,
    horizon_weeks = horizon_weeks,
    gross_margin = CFG$gross_margin,
    note = paste("CLV scales linearly with the assumed gross margin; the",
                 "dataset contains no cost data. A churned customer",
                 "(still_active = FALSE) is worth zero.")
  )
}

#* Look up a scored customer by ID
#* @param id Customer ID (integer)
#* @get /customers/:id
function(id) {
  row <- clv_tbl %>% filter(CustomerID == as.integer(id))
  if (nrow(row) == 0) stop("customer not found", status = 404)
  list(
    CustomerID = row$CustomerID[1],
    segment = as.character(row$segment[1]),
    recency_days = row$recency[1],
    age_weeks = row$age_weeks[1],
    still_active = row$still_active[1],
    frequency = row$frequency[1],
    monetary = row$monetary[1],
    predicted_clv = round(row$clv[1], 2),
    currency = CFG$currency
  )
}
