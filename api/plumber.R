# plumber API -----------------------------------------------------------------
# Minimal scoring service. Reads the fitted model artefacts written by
# `make all`; does not retrain.
#
#   Rscript -e 'pr <- plumber::plumb("api/plumber.R"); pr$run(port = 8000)'
#
# Endpoints
#   GET  /health               liveness + model metadata
#   GET  /segments             segment-level CLV summary
#   POST /predict-clv          score one customer from raw RFM inputs
#   GET  /customers?id=<id>    look up a scored customer

library(plumber)

# Run `make all` before starting the service: this reads the fitted artefacts
# rather than refitting.
#
# Locate the helper by trying both layouts, because `make api` and
# `plumb("api/plumber.R")` leave the working directory in different places.
# set_project_root() then normalises it, which is what targets::tar_load() needs.
for (p in c(file.path("R", "root.R"), file.path("..", "R", "root.R"))) {
  if (file.exists(p)) { source(p); break }
}
if (!exists("set_project_root")) {
  stop("Could not locate R/root.R. Run `make api` from the repo root.")
}
set_project_root()

source("R/00_config.R")
source("R/utils.R")
source("R/05_clv.R")

if (!dir.exists(file.path("_targets", "objects"))) {
  stop("No targets data store found. Run `make all` first.")
}
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
#* @param age_weeks:number Weeks since the customer's FIRST purchase (the lifetime model is indexed by this).
#* @param frequency:number Number of distinct orders.
#* @param avg_order_value:number Mean order value.
#* @param still_active:boolean FALSE means churn was observed, so predicted CLV is 0.
#* @param horizon_weeks:number Forecast horizon; defaults to the fitted horizon.
#* @post /predict-clv
function(req, res, age_weeks = NULL, frequency = NULL, avg_order_value = NULL,
         still_active = TRUE, horizon_weeks = NULL) {

  # Validation failures set the status on `res` and return a body rather than
  # throwing. plumber maps an uncaught condition to 500 regardless of the
  # status attached to it, so this is the supported way to return a 4xx.
  bad <- function(msg) {
    res$status <- 400L
    list(error = msg, valid = FALSE)
  }

  if (is.null(age_weeks) || is.null(frequency) || is.null(avg_order_value)) {
    return(bad("age_weeks, frequency and avg_order_value are required"))
  }

  # POST form fields arrive as character strings, so coerce before any
  # arithmetic. max(1, "40") would otherwise fail on a character comparison.
  num <- function(x, name) {
    v <- suppressWarnings(as.numeric(x))
    if (length(v) != 1 || is.na(v)) NULL else v
  }
  age_weeks <- num(age_weeks, "age_weeks")
  frequency <- num(frequency, "frequency")
  avg_order_value <- num(avg_order_value, "avg_order_value")
  if (is.null(age_weeks))  return(bad("age_weeks must be a single number"))
  if (is.null(frequency))  return(bad("frequency must be a single number"))
  if (is.null(avg_order_value)) return(bad("avg_order_value must be a single number"))

  if (is.null(horizon_weeks)) horizon_weeks <- CFG$clv_horizon_weeks
  horizon_weeks <- num(horizon_weeks, "horizon_weeks")
  if (is.null(horizon_weeks)) return(bad("horizon_weeks must be a single number"))

  # still_active may arrive as the string "false", which is truthy in R.
  still_active <- if (is.logical(still_active)) still_active else
    !(tolower(as.character(still_active)) %in% c("false", "0", "no", ""))

  if (any(c(age_weeks, frequency, avg_order_value, horizon_weeks) < 0)) {
    return(bad("numeric inputs must be non-negative"))
  }
  if (frequency < 1) return(bad("frequency must be at least 1"))

  d <- data.frame(
    CustomerID = 1L,
    age_weeks = max(1, age_weeks),
    since_last_weeks = 0,
    still_active = still_active,
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
#*
#' Note: this is a query-string endpoint (`/customers?id=12347`), not the more
#' idiomatic `/customers/:id`. Path parameters do not bind at all in plumber
#' 1.2.3 under R 4.6 -- a route declared as `/customers/:id` is registered but
#' always 404s, verified against a minimal reproduction. Query parameters bind
#' correctly, so this uses them until that is fixed upstream.
#' @param id:string Customer ID (integer).
#* @get /customers
function(req, res, id = NULL) {
  if (is.null(id)) {
    res$status <- 400L
    return(list(error = "id is required", valid = FALSE))
  }
  row <- clv_tbl %>% filter(CustomerID == suppressWarnings(as.integer(id)))
  if (nrow(row) == 0) {
    res$status <- 404L
    return(list(error = "customer not found", valid = FALSE))
  }
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
