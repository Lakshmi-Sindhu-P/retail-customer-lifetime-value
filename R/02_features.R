# 02 - Feature engineering -----------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

#' Derive line-level and customer-level features
#'
#' The original notebook extracted year/month/hour with a per-row
#' sapply(strsplit(...)) over ~530k rows. That is quadratic in practice and
#' unnecessary: lubridate does it vectorised.
build_features <- function(d) {
  log_step("deriving line-level features")

  lines <- d %>%
    mutate(
      Spent    = Quantity * UnitPrice,
      date     = as.Date(InvoiceDate),
      year     = year(InvoiceDate),
      month    = month(InvoiceDate),
          day  = day(InvoiceDate),
      hour     = hour(InvoiceDate),
      weekday  = wday(InvoiceDate, label = TRUE, week_start = 1),
      is_weekend = wday(InvoiceDate, week_start = 1) %in% c(6, 7)
    )

  log_step("aggregating to customer level (%d customers)", n_distinct(lines$CustomerID))

  customers <- lines %>%
    group_by(CustomerID, Country) %>%
    summarise(
      first_purchase = min(date),
      last_purchase  = max(date),
      tenure_days    = as.integer(max(date) - min(date)),
      n_orders       = n_distinct(InvoiceNo),
      n_lines        = n(),
      n_products     = n_distinct(StockCode),
      quantity       = sum(Quantity),
      revenue        = sum(Spent),
      .groups = "drop"
    ) %>%
    mutate(avg_order_value = revenue / n_orders)

  list(lines = lines, customers = customers)
}

#' Revenue and order-count aggregates by various dimensions
#'
#' One function rather than the seven near-duplicate summarise calls in the
#' original notebook.
aggregate_by <- function(lines, by) {
  lines %>%
    group_by(across(all_of(by))) %>%
    summarise(
      revenue     = sum(Spent),
      orders      = n_distinct(InvoiceNo),
      customers   = n_distinct(CustomerID),
      lines       = n(),
      .groups     = "drop"
    ) %>%
    mutate(avg_order_value = revenue / orders)
}

#' Monthly revenue series
monthly_revenue <- function(lines) {
  lines %>%
    mutate(month_start = as.Date(format(date, "%Y-%m-01"))) %>%
    group_by(month_start) %>%
    summarise(revenue = sum(Spent), orders = n_distinct(InvoiceNo),
              customers = n_distinct(CustomerID), .groups = "drop")
}
