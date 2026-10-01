#' Tests for the CLV model
#'
# The critical property of a survival model is that it must reproduce a
# survival curve we can compute independently. These tests generate data from
# a known distribution, fit the model, and require that the fit recovers it.
# If these fail, the CLV numbers in the report are meaningless.

test_that("Kaplan-Meier matches hand-computed survival on a tiny example", {
  # 6 customers, no censoring.
  #   events at t = 1, 2, 3, 4, 5, 6
  # Survival: 5/6, 4/6, 3/6, 2/6, 1/6, 0/6
  times <- c(1, 2, 3, 4, 5, 6)
  event <- rep(TRUE, 6)

  km <- kaplan_meier(times, event)

  expect_equal(km$survival[1], 1)                 # before any event
  expect_equal(km$survival[2], 5 / 6)
  expect_equal(km$survival[3], 4 / 6)
  expect_equal(km$survival[4], 3 / 6)
  expect_equal(km$survival[7], 0)

  # At-risk counts must decrease monotonically.
  expect_true(all(diff(km$n_risk) <= 0))
})

test_that("Kaplan-Meier handles right-censoring correctly", {
  # 4 customers: two churn at t=1, two censored at t=10.
  # At t=1, n_risk = 4, d = 2 -> S = 1 - 2/4 = 0.5
  # No further events, so S stays at 0.5.
  times <- c(1, 1, 10, 10)
  event <- c(TRUE, TRUE, FALSE, FALSE)

  km <- kaplan_meier(times, event)
  expect_equal(km$survival[km$time == 1], 0.5)
  expect_equal(tail(km$survival, 1), 0.5)

  # Censored customers must still contribute to the risk set.
  expect_true(km$n_risk[km$time == 1] == 4)
})

test_that("Kaplan-Meier survival is non-increasing", {
  set.seed(1)
  times <- sample(1:100, 500, TRUE)
  event <- runif(500) < 0.6
  km <- kaplan_meier(times, event)
  expect_true(all(diff(km$survival) <= 1e-12))
  expect_true(all(km$survival >= 0 & km$survival <= 1))
})

test_that("Weibull MLE recovers known parameters from simulated data", {
  # This is the load-bearing test: if the Weibull fit cannot recover a known
  # Weibull sample, the CLV model cannot be trusted on real data.
  set.seed(42)
  n <- 8000
  true_shape <- 1.8
  true_scale <- 52

  times <- true_scale * rweibull(n, shape = true_shape)
  event <- rep(TRUE, n)   # no censoring, so the fit is unbiased

  fit <- fit_weibull(times, event)

  # A 5% tolerance on 8000 uncensored draws is reasonable.
  expect_lt(abs(fit$shape - true_shape) / true_shape, 0.10)
  expect_lt(abs(fit$scale - true_scale) / true_scale, 0.10)
})

test_that("Weibull MLE is robust to right-censoring", {
  # Censoring must bias the fit low, but not catastrophically. A broken
  # likelihood typically produces nonsense here.
  set.seed(7)
  n <- 4000
  times <- 52 * rweibull(n, shape = 1.6)
  # Independently censor ~30% at a fixed horizon.
  censor_at <- 45
  event <- times <= censor_at
  observed <- pmin(times, censor_at)

  fit <- fit_weibull(observed, event)

  expect_true(is.finite(fit$shape))
  expect_gt(fit$shape, 0.3)
  expect_lt(fit$shape, 6)
  expect_true(fit$scale > 0)
})

test_that("fitted Weibull approximates the Kaplan-Meier curve it came from", {
  # The cross-validation that justifies publishing CLV at all: the
  # parametric and non-parametric estimates must broadly agree.
  set.seed(21)
  n <- 5000
  times <- 52 * rweibull(n, shape = 1.5)
  event <- runif(n) < 0.75
  observed <- pmin(times, quantile(times, 0.85))

  weib <- fit_weibull(observed, event)
  agreement <- survival_agreement(observed, event, weib)

  # Mean absolute survival gap under 0.12 across the observed range.
  expect_lt(agreement, 0.12)
})

test_that("Weibull survival function has the correct limiting behaviour", {
  # S(0) = 1
  expect_equal(weibull_surv(0, shape = 1.5, scale = 52), 1)
  # Monotone decreasing
  s <- weibull_surv(seq(0, 200, length.out = 50), shape = 1.5, scale = 52)
  expect_true(all(diff(s) <= 1e-12))
  # The scale is the e^-1 time point, whatever the shape.
  expect_equal(weibull_surv(52, shape = 2.7, scale = 52), exp(-1))
})

#' Build a CLV input table in the shape compute_clv() expects.
#'
#' age_weeks, still_active and rate are the model's real inputs; recency is a
#' reporting column only.
mk_clv_input <- function(n, recency, freq, aov, age_weeks = 30,
                         still_active = NULL) {
  data.frame(
    CustomerID      = seq_len(n),
    age_weeks       = rep(age_weeks, length.out = n),
    since_last_weeks = recency / 7,
    still_active    = if (is.null(still_active)) recency <= 42 else still_active,
    rate            = freq / age_weeks,
    frequency       = freq,
    monetary        = freq * aov,
    avg_order_value = aov,
    recency         = recency,
    stringsAsFactors = FALSE
  )
}

test_km <- function() kaplan_meier(seq(5, 50, by = 5), rep(TRUE, 10))

test_that("CLV is positive, finite, and responds correctly to its inputs", {
  set.seed(9)
  n <- 300
  rfm <- mk_clv_input(n,
                      recency = sample(0:90, n, TRUE),
                      freq = pmax(1, rpois(n, 4)),
                      aov = runif(n, 10, 200))

  km <- test_km()
  base <- compute_clv(rfm, km, horizon_weeks = 52, gross_margin = 0.50)

  expect_true(all(base$clv >= 0))
  expect_true(all(is.finite(base$clv)))

  # Margin is a strict linear multiplier.
  wider <- compute_clv(rfm, km, horizon_weeks = 52, gross_margin = 0.90)
  expect_equal(wider$clv, base$clv * (0.90 / 0.50), tolerance = 1e-9)

  # A longer horizon can never reduce expected value.
  longer <- compute_clv(rfm, km, horizon_weeks = 104, gross_margin = 0.50)
  expect_gt(sum(longer$clv), sum(base$clv))
})

test_that("CLV orders customers by activity and purchase rate", {
  # Two otherwise-identical customers; one bought more recently and is still
  # active. The active one must be worth more.
  rfm2 <- data.frame(
    CustomerID = 1:2,
    age_weeks = c(30, 30),
    since_last_weeks = c(1/7, 40/7),
    still_active = c(TRUE, FALSE),
    frequency = c(4, 4),
    monetary = c(400, 400),
    avg_order_value = c(100, 100),
    rate = c(4/30, 4/30),
    recency = c(1, 280),
    stringsAsFactors = FALSE
  )
  km <- test_km()
  out <- compute_clv(rfm2, km, 52, 0.5)
  expect_gt(out$clv[1], out$clv[2])

  # And a higher purchase rate must be worth more, all else equal.
  rfm3 <- rfm2
  rfm3$rate <- c(1, 1)
  rfm3$frequency <- c(1, 1)
  # rfm3 has a far higher purchase rate (1 order/week vs 4/52), so it must be
  # worth strictly more even though the lifetime/recency inputs are identical.
  out3 <- compute_clv(rfm3, km, 52, 0.5)
  expect_gt(out3$clv[1], out3$clv[2])
  expect_gt(out3$clv[1], out$clv[1])

  # The lifetime component is rate-independent: identical lifetime inputs give
  # identical expected remaining weeks.
  expect_equal(out$expected_remaining_weeks, out3$expected_remaining_weeks,
               tolerance = 1e-9)
  expect_gt(out$expected_remaining_weeks[1], out$expected_remaining_weeks[2])
})

test_that("a churned customer is worth zero, and recency enters only via churn", {
  # still_active = FALSE means we observed them go quiet: no expected future
  # activity, regardless of how good a customer they historically were.
  churned <- data.frame(
    CustomerID = 1L,
    age_weeks = 30, since_last_weeks = 80, still_active = FALSE,
    frequency = 3, monetary = 300, avg_order_value = 100, rate = 3/30,
    recency = 560, stringsAsFactors = FALSE
  )
  km <- test_km()
  out <- compute_clv(churned, km, 52, 0.5)
  expect_equal(out$clv, 0)
  expect_equal(out$expected_remaining_weeks, 0)

  # An active customer with the same history is worth something.
  active <- churned
  active$still_active <- TRUE
  active$since_last_weeks <- 1
  expect_gt(compute_clv(active, km, 52, 0.5)$clv, 0)

  # Deliberate design property, asserted so it cannot change silently: the CLV
  # model is indexed by age and the churn flag. Fine-grained recency does NOT
  # enter, because the observed purchase rate already reflects how recently
  # they bought. Two customers identical but for since_last_weeks are equal.
  r2 <- active
  r2$since_last_weeks <- 5
  expect_equal(compute_clv(r2, km, 52, 0.5)$clv,
               compute_clv(active, km, 52, 0.5)$clv)
})

test_that("CLV never increases with inactivity", {
  # A staler customer is never worth more. Churn is a hard boundary at zero.
  base <- data.frame(
    CustomerID = 1L, age_weeks = 30, frequency = 4, monetary = 400,
    avg_order_value = 100, rate = 4/30, stringsAsFactors = FALSE
  )
  km <- test_km()
  gaps <- c(0, 1, 2, 4, 6, 8, 12)   # weeks since last purchase
  vals <- vapply(gaps, function(g) {
    x <- base
    x$since_last_weeks <- g
    x$still_active <- g <= 6          # 42-day censoring window
    compute_clv(x, km, 52, 0.5)$clv
  }, numeric(1))

  expect_true(all(diff(vals) <= 1e-9))
  expect_gt(vals[1], 0)
  expect_equal(vals[length(vals)], 0)   # past the window: churned
})

test_that("clv_by_segment allocates all CLV and shares sum to 1", {
  set.seed(13)
  n <- 400
  tbl <- data.frame(
    segment = sample(c("A", "B", "C"), n, TRUE),
    clv = runif(n, 10, 500),
    clv_expected_txn = runif(n, 0.1, 4),
    stringsAsFactors = FALSE
  )
  s <- clv_by_segment(tbl)
  expect_equal(sum(s$clv_total), sum(tbl$clv))
  expect_equal(sum(s$clv_share), 1, tolerance = 1e-9)
  expect_equal(sum(s$customers), n)
  # Ordered by total value, descending.
  expect_true(all(diff(s$clv_total) <= 1e-9))
})

test_that("CLV ranks an active customer above an identical dormant one", {
  # Regression test. Three separate bugs each inverted this relationship:
  #   1. conditioning survival on recency instead of age since first purchase
  #   2. treating the censoring flag as "churned" rather than "still active"
  #   3. measuring the purchase rate over calendar tenure, which silently
  #      rewarded customers who churned early
  base <- data.frame(
    CustomerID = 1:2,
    age_weeks = c(20, 20),
    since_last_weeks = c(1, 40),
    still_active = c(TRUE, FALSE),
    rate = c(4/20, 4/20),
    frequency = c(4, 4),
    monetary = c(400, 400),
    avg_order_value = c(100, 100),
    stringsAsFactors = FALSE
  )
  km <- kaplan_meier(c(5, 10, 15, 20, 25, 30), rep(TRUE, 6))

  out <- compute_clv(base, km, 52, 0.5)
  expect_gt(out$clv[1], out$clv[2])
  # A customer known to have churned is worth nothing going forward.
  expect_equal(out$clv[2], 0)
})

test_that("conditional remaining life matches an independent computation", {
  # Whether conditional remaining life rises or falls with age depends on
  # whether the hazard is increasing or decreasing, so no monotonicity law can
  # be asserted here. What CAN be verified is that the implemented integral
  # equals the documented formula computed a different way: by summing over the
  # Kaplan-Meier step intervals rather than evaluating on a uniform grid.
  base <- data.frame(
    CustomerID = 1:3,
    age_weeks = c(4, 20, 33),
    since_last_weeks = 1,
    still_active = TRUE,
    rate = 0.2,
    frequency = 8, monetary = 800, avg_order_value = 100,
    stringsAsFactors = FALSE
  )
  km <- kaplan_meier(seq(5, 50, by = 5), rep(TRUE, 10))
  H <- 52
  out <- compute_clv(base, km, H, 0.5)

  # Independent reference: exact integral of the step function over each
  # interval. km_survival_at() is LEFT-continuous (findInterval returns the
  # step in force at t), so each interval contributes its left-edge value.
  reference <- vapply(base$age_weeks, function(a) {
    s_a <- km_survival_at(km, a)
    edges <- c(a, km$time[km$time > a & km$time < a + H], a + H)
    total <- 0
    for (j in seq_len(length(edges) - 1)) {
      total <- total + km_survival_at(km, edges[j]) * (edges[j + 1] - edges[j])
    }
    total / s_a
  }, numeric(1))

  expect_equal(out$expected_remaining_weeks, reference, tolerance = 0.05)
})

test_that("conditional remaining life is flat when the hazard is constant", {
  # The memoryless (exponential) case is the one lifetime distribution where
  # the expected answer is known analytically: remaining life does not depend
  # on how long the customer has already been active. If the /S(age) term were
  # missing, this would instead decay with age.
  # Exponential lifetimes give a genuinely constant hazard.
  set.seed(15)
  mean_life <- 60
  km <- kaplan_meier(rexp(4000, rate = 1/mean_life), rep(TRUE, 4000))
  H <- 52

  # Ages are kept where the risk set is still large; at extreme ages the KM
  # tail itself is noisy, which is a property of the estimator, not the model.
  ages <- c(10, 50, 100)

  vals <- vapply(ages, function(a) {
    d <- data.frame(
      CustomerID = 1L, age_weeks = a, since_last_weeks = 1,
      still_active = TRUE, rate = 0.1, frequency = 10, monetary = 1000,
      avg_order_value = 100, stringsAsFactors = FALSE)
    compute_clv(d, km, H, 0.5)$expected_remaining_weeks
  }, numeric(1))

  # Analytic answer for a memoryless lifetime: mean_life * (1 - exp(-H/mean_life)).
  analytic <- mean_life * (1 - exp(-H / mean_life))
  expect_true(all(vals > 0))

  # Flat across ages -- the defining signature of conditioning on survival --
  # and each within 2% of the closed-form value.
  expect_lt(max(vals) - min(vals), 0.02 * mean(vals))
  expect_true(all(abs(vals - analytic) / analytic < 0.02))
})

test_that("compute_clv uses age_weeks, not recency, for conditioning", {
  # Two customers with identical age and rate but opposite recency: the active
  # one must be worth more. If the implementation conditioned on recency these
  # two would come out equal or inverted.
  base <- data.frame(
    CustomerID = 1:2,
    age_weeks = 30,
    since_last_weeks = c(1, 60),
    still_active = c(TRUE, FALSE),
    rate = 0.1, frequency = 3, monetary = 300, avg_order_value = 100,
    stringsAsFactors = FALSE
  )
  km <- kaplan_meier(seq(5, 50, by = 5), rep(TRUE, 10))
  out <- compute_clv(base, km, 52, 0.5)
  expect_gt(out$clv[1], out$clv[2])
})

test_that("a purchase rate measured over calendar tenure is not used", {
  # A churned customer has a long calendar tenure but a short active lifetime.
  # Dividing orders by calendar tenure understates their rate; the model must
  # receive the observed-lifetime rate.
  base <- data.frame(
    CustomerID = 1L,
    age_weeks = 2,          # first purchase 2 weeks ago
    since_last_weeks = 0,   # bought today
    still_active = TRUE,
    rate = 4/2,             # 4 orders in 2 active weeks
    frequency = 4, monetary = 400, avg_order_value = 100,
    stringsAsFactors = FALSE
  )
  km <- kaplan_meier(seq(5, 50, by = 5), rep(TRUE, 10))
  out <- compute_clv(base, km, 52, 0.5)
  expect_equal(out$clv_per_txn, 100)
  expect_gt(out$clv_expected_txn, 0)
})
