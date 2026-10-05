#' Tests for the data loading and cleaning stage
#'
# The cleaning rules are where an off-by-one silently biases every downstream
# number, so the row accounting is checked exactly rather than approximately.

mk_lines <- function() {
  data.frame(
    # NOTE: every surviving invoice is distinct. The 99.9% quantile rule for
    # quantity outliers only bites on much larger data; on a 6-row fixture it
    # would otherwise delete whichever row happened to be the maximum, which
    # makes these counts confusing rather than instructive.
    InvoiceNo    = c("100001", "100001", "100002", "C100003", "100004", "100005"),
    StockCode    = c("85123A", "71053", "84406B", "22752", "21730", "22633"),
    # The NA Description is on a row that also has an NA CustomerID, so it is
    # removed by rule 1 before rule 2 ever sees it. To exercise rule 2 on its
    # own, use test_null_description_is_dropped_separately().
    Description  = c("WHITE HANGING", "SET 7 BABUSHKA HANGERS", "RED HANGING",
                     "BLUE HANGING", "GREEN HANGING", "BLACK HANGING"),
    Quantity     = c(2, 6, 4, -4, 1, 3),
    InvoiceDate  = as.POSIXct(c("2011-03-01 10:00:00", "2011-03-01 11:00:00",
                                "2011-03-02 09:30:00", "2011-03-03 12:00:00",
                                "2011-03-04 08:00:00", "2011-03-05 14:00:00")),
    UnitPrice    = c(2.55, 3.39, 2.75, 2.75, 3.39, 0.85),
    CustomerID   = c(12347, 12347, 12347, 12348, 12349, NA),
    Country      = c("United Kingdom", "United Kingdom", "United Kingdom",
                    "United Kingdom", "France", "Germany"),
    stringsAsFactors = FALSE
  )
}

test_that("clean_transactions removes cancellations and unattributable rows", {
  d <- clean_transactions(mk_lines())$data

  # Dropped: the NA CustomerID row, the NA Description row, the C-prefixed
  # cancellation, and its negative quantity.
  expect_true(!any(is.na(d$CustomerID)))
  expect_true(!any(is.na(d$Description)))
  expect_false(any(grepl("^C", d$InvoiceNo)))
  expect_true(all(d$Quantity > 0))
  expect_true(all(d$UnitPrice > 0))
  expect_equal(nrow(d), 4)
})

test_that("null descriptions are dropped on rows that have a CustomerID", {
  # Rule 2 in isolation: the NA must sit on an otherwise-valid row, otherwise
  # rule 1 has already removed it and the rule is never exercised.
  d <- mk_lines()
  d$CustomerID[6] <- 12350          # was NA
  d$Description[3] <- NA            # was "RED HANGING"
  d$InvoiceNo[3] <- "100003"

  a <- clean_transactions(d)$audit
  expect_equal(a$raw_missing_customerid, 0)
  expect_equal(a$raw_missing_description, 1)
  expect_equal(a$after_drop_description, a$after_drop_customerid - 1)
})

test_that("clean_transactions reports an exact row audit", {
  a <- clean_transactions(mk_lines())$audit

  expect_equal(a$raw_rows, 6)
  expect_equal(a$raw_missing_customerid, 1)
  expect_equal(a$cancel_rows, 1)
  expect_equal(a$clean_rows, 4)

  # The audit must reconcile: rows removed by each rule, in order.
  after_cust   <- a$after_drop_customerid
  after_desc   <- a$after_drop_description
  after_cancel <- a$after_drop_cancellations
  expect_equal(a$raw_rows - a$raw_missing_customerid, after_cust)
  expect_equal(after_desc, after_cust - a$raw_missing_description)
  expect_equal(after_cancel, after_desc - a$cancel_rows - a$nonpos_price)
  expect_equal(a$clean_rows, after_cancel - a$qty_outlier_rows)
})

test_that("cancellations are removed even without the C prefix", {
  # A negative quantity with a normal invoice number is still a cancellation
  # and must go; dropping only C-prefixed rows would leave it in.
  d <- mk_lines()
  d$InvoiceNo[4] <- "100999"
  out <- clean_transactions(d)$data
  expect_false(any(out$InvoiceNo == "100999"))
})

test_that("non-positive unit prices are removed", {
  d <- mk_lines()
  d$UnitPrice[2] <- 0
  d$UnitPrice[3] <- -11.06
  out <- clean_transactions(d)$data
  expect_true(all(out$UnitPrice > 0))
  expect_equal(nrow(out), 2)
})

test_that("quantity outliers are removed by dropping the whole invoice", {
  # The original notebook deleted two hardcoded invoice numbers to handle an
  # 80,995-unit cancellation. The rule here is a quantile instead, and it must
  # remove BOTH the oversized line and any sibling lines on that invoice.
  set.seed(1)
  n_normal_invoices <- 400
  big_invoice <- paste0("B", sprintf("%04d", 1:10))

  n_rows <- n_normal_invoices + 2 * length(big_invoice)
  d <- data.frame(
    InvoiceNo   = c(paste0("N", sprintf("%05d", seq_len(n_normal_invoices))),
                    big_invoice, big_invoice),
    StockCode   = "X",
    Description = "item",
    # Each big invoice has one absurd line and one ordinary sibling line.
    Quantity    = c(rep(2, n_normal_invoices), rep(50000, 10), rep(1, 10)),
    InvoiceDate = as.POSIXct("2011-03-01 10:00:00", tz = "UTC"),
    UnitPrice   = 1,
    CustomerID  = 1L,
    Country     = "United Kingdom",
    stringsAsFactors = FALSE
  )
  stopifnot(nrow(d) == n_rows)

  res <- clean_transactions(d)
  expect_equal(res$audit$qty_outlier_invoices, 10)
  # The sibling line on the same invoice goes too, which is the whole point.
  expect_equal(res$audit$qty_outlier_rows, 20)
  expect_false(any(res$data$InvoiceNo %in% big_invoice))
})

test_that("clean_transactions coerces the schema to the documented types", {
  d <- clean_transactions(mk_lines())$data
  expect_type(d$CustomerID, "integer")
  expect_type(d$InvoiceNo, "character")
  expect_type(d$StockCode, "character")
  expect_type(d$Country, "character")
})

test_that("cleaning is deterministic", {
  a <- clean_transactions(mk_lines())$data
  b <- clean_transactions(mk_lines())$data
  expect_equal(a, b)
})

test_that("build_features adds the expected derived columns", {
  d <- clean_transactions(mk_lines())$data
  f <- build_features(d)

  expect_true(all(c("Spent", "date", "year", "month", "hour", "weekday",
                    "is_weekend") %in% names(f$lines)))
  expect_equal(f$lines$Spent,
               f$lines$Quantity * f$lines$UnitPrice)
  # One row per (customer, country) pair.
  expect_true(all(!duplicated(f$customers[, c("CustomerID", "Country")])))
  expect_true(all(c("revenue", "n_orders", "avg_order_value", "tenure_days")
                  %in% names(f$customers)))
})

test_that("aggregate_by groups correctly and computes average order value", {
  d <- clean_transactions(mk_lines())$data
  f <- build_features(d)
  agg <- aggregate_by(f$lines, "Country")

  expect_true(all(c("revenue", "orders", "customers", "avg_order_value")
                  %in% names(agg)))
  expect_true(all(agg$orders > 0))
  expect_equal(agg$avg_order_value, agg$revenue / agg$orders)
  # Revenue must reconcile with the line-level total.
  expect_equal(sum(agg$revenue), sum(f$lines$Spent))
})

test_that("monthly_revenue produces a monotone month sequence", {
  d <- clean_transactions(mk_lines())$data
  f <- build_features(d)
  m <- monthly_revenue(f$lines)
  expect_true(all(diff(m$month_start) > 0))
  expect_equal(sum(m$revenue), sum(f$lines$Spent))
})

test_that("qty_outlier_threshold catches outliers a quantile threshold would miss", {
  # Regression test for the quantile-threshold flaw: a fixed 99.9th percentile
  # silently flags nothing when extreme rows exceed 0.1% of the data.
  set.seed(4)
  n <- 400
  with_outliers <- c(rep(2, n), rep(50000, 10))
  thresh <- qty_outlier_threshold(with_outliers)
  expect_equal(sum(with_outliers > thresh), 10)

  # A tail that is merely long, not absurd, must NOT be flagged. Real
  # wholesalers order in the hundreds.
  bulk <- c(rep(2, n), rep(120, 5))
  expect_equal(sum(bulk > qty_outlier_threshold(bulk)), 0)
})

test_that("the fence widens as the interquartile range grows", {
  # A wider spread in the bulk of the distribution must raise the fence, so
  # genuinely varied order sizes are not mistaken for entry errors.
  # Keep enough spread in the bulk that the IQR is non-degenerate, since the
  # fence scales with the interquartile range by construction.
  # Deterministic, fully-constructed sequences. Note the bulk must occupy under
  # half the rows, otherwise the 25th and 75th percentiles both land on the
  # same value and the IQR is zero -- which is exactly the degenerate case
  # handled by the fallback above, not the case under test here.
  narrow <- c(rep(c(2, 3, 4), 100), rep(50, 5))
  wide   <- c(rep(c(2, 6, 10), 100), rep(500, 5))
  stopifnot(all(is.finite(narrow)), all(is.finite(wide)))
  stopifnot(IQR(log1p(narrow)) > 0, IQR(log1p(wide)) > 0)

  expect_gt(IQR(log1p(wide)), IQR(log1p(narrow)))
  expect_gt(qty_outlier_threshold(wide), qty_outlier_threshold(narrow))
})

test_that("qty_outlier_threshold handles a constant column without dividing by zero", {
  # IQR is exactly 0 for constant input; the fence must degrade gracefully
  # rather than produce NaN or -Inf.
  const <- rep(7, 100)
  thresh <- qty_outlier_threshold(const)
  expect_true(is.finite(thresh))
  expect_equal(sum(const > thresh), 0)
})
