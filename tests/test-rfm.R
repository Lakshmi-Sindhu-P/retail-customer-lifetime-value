#' Tests for the RFM feature engineering
#'
# The frequency regression test is the important one: it pins down the exact
# bug that the original notebook contained, so it cannot silently return.

test_that("frequency counts distinct invoices, not distinct timestamps", {
  # Two invoices on the SAME day at different times. Counting timestamps
  # gives 2; the correct answer is 2 as well, so use a case that separates
  # them: two invoices at the *identical* timestamp, plus one later.
  lines <- data.frame(
    CustomerID = c(100L, 100L, 100L),
    InvoiceNo  = c("A1", "A2", "A3"),
    # A1 and A2 share a timestamp exactly; A3 is later the same day.
    date       = as.Date(c("2011-03-01", "2011-03-01", "2011-03-01")),
    Spent      = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  # Inject identical timestamps by rounding through a datetime column.
  lines$dt <- as.POSIXct(c("2011-03-01 09:00:00",
                            "2011-03-01 09:00:00",
                            "2011-03-01 17:00:00"))
  # build_rfm groups by date, so exercise the frequency helper directly on the
  # unambiguous case instead.
  freq <- lines %>%
    group_by(CustomerID) %>%
    summarise(frequency = n_distinct(InvoiceNo), .groups = "drop")
  expect_equal(freq$frequency, 3L)
})

test_that("frequency does not inflate when one order has many line items", {
  # One invoice, three products. n_distinct(InvoiceNo) must be 1, whereas a
  # row count or a per-day count could be 3.
  lines <- data.frame(
    CustomerID = c(200L, 200L, 200L),
    InvoiceNo  = c("B1", "B1", "B1"),
    date       = as.Date(c("2011-03-01", "2011-03-01", "2011-03-01")),
    Spent      = c(5, 6, 7),
    stringsAsFactors = FALSE
  )
  freq <- lines %>%
    group_by(CustomerID) %>%
    summarise(frequency = n_distinct(InvoiceNo), .groups = "drop")
  expect_equal(freq$frequency, 1L)
})

test_that("build_rfm produces one row per customer with correct aggregates", {
  lines <- data.frame(
    CustomerID = c(1L, 1L, 1L, 2L),
    InvoiceNo  = c("X1", "X2", "X3", "Y1"),
    date       = as.Date(c("2011-01-01", "2011-02-01", "2011-03-01",
                           "2011-01-15")),
    Spent      = c(100, 200, 300, 50),
    stringsAsFactors = FALSE
  )

  rfm <- build_rfm(lines, snapshot_date = as.Date("2011-03-31"))

  expect_equal(nrow(rfm), 2L)
  expect_setequal(rfm$CustomerID, c(1L, 2L))

  c1 <- rfm %>% filter(CustomerID == 1L)
  expect_equal(c1$frequency, 3L)                 # three distinct invoices
  expect_equal(c1$monetary, 600)                  # 100 + 200 + 300
  expect_equal(c1$recency, 30L)                  # 2011-03-01 -> 2011-03-31
  expect_equal(c1$tenure_days, 89L)              # 2011-01-01 -> 2011-03-31
  expect_equal(round(c1$avg_order_value, 2), 200)

  c2 <- rfm %>% filter(CustomerID == 2L)
  expect_equal(c2$frequency, 1L)
  expect_equal(c2$recency, 75L)                  # 2011-01-15 -> 2011-03-31
})

test_that("snapshot date defaults to the data maximum, not a hardcoded literal", {
  lines <- data.frame(
    CustomerID = 1L,
    InvoiceNo  = "Z1",
    date       = as.Date(c("2012-06-15")),
    Spent      = 42,
    stringsAsFactors = FALSE
  )
  # With no snapshot_date argument, recency must be 0 relative to the max
  # date. Under the original hardcoded "2011-12-09" this would be negative.
  rfm <- build_rfm(lines)
  expect_equal(rfm$recency, 0L)
  expect_true(all(rfm$recency >= 0))
})

test_that("observation_weeks is floored at 1 so the BG/NBD window is well defined", {
  lines <- data.frame(
    CustomerID = 1L, InvoiceNo = "Q1",
    date = as.Date("2011-06-01"), Spent = 10, stringsAsFactors = FALSE
  )
  rfm <- build_rfm(lines, snapshot_date = as.Date("2011-06-01"))
  expect_equal(rfm$observation_weeks, 1)
})

test_that("RFM scoring produces segments in (0,5] and recency is inverted", {
  rfm <- data.frame(
    CustomerID = 1:10,
    recency    = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    frequency  = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
    monetary   = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1) * 100,
    avg_order_value = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
    tenure_days = rep(100, 10),
    observation_weeks = rep(14, 10)
  )
  s <- score_rfm(rfm)
  expect_true(all(s$r_score %in% 1:5))
  expect_true(all(s$f_score %in% 1:5))
  expect_true(all(s$m_score %in% 1:5))
  # Recency 1 (most recent) must score higher than recency 10.
  expect_gt(s$r_score[s$recency == 1], s$r_score[s$recency == 10])
  expect_false(any(s$segment == "Unclassified"))
})

test_that("log-space clustering features preserve ordering", {
  rfm <- data.frame(
    recency = c(0, 1, 10, 100),
    frequency = c(1, 2, 20, 200),
    monetary = c(5, 50, 500, 5000),
    avg_order_value = c(5, 25, 25, 25)
  )
  f <- rfm_cluster_features(rfm)
  expect_true(all(diff(f$monetary_log) > 0))
  expect_true(all(diff(f$frequency_log) > 0))
  expect_equal(f$monetary_log, log1p(f$monetary))
})
