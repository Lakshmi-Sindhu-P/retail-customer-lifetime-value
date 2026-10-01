#' Tests for the k-selection policy
#'
# The choice of k is the single most consequential judgement in a segmentation
# deliverable, so the policy that makes it is tested directly rather than
# assumed.

mk_selection <- function(sil_k = 3, elbow = 4, gap = 2,
                         ari = c(`2` = 0.96, `3` = 0.93, `4` = 0.90, `5` = 0.80)) {
  list(
    wss = wss_curve(scale(matrix(rnorm(400, 0, 1), ncol = 2)), k_max = 5, nstart = 5),
    silhouette = data.frame(k = 2:5,
                            silhouette = c(0.20, 0.35, 0.30, 0.25)),
    elbow_k = elbow,
    silhouette_k = sil_k,
    gap_k = gap,
    stability = data.frame(k = as.integer(names(ari)),
                           ari_mean = as.numeric(ari),
                           ari_p05 = as.numeric(ari) - 0.05,
                           stable_share = 0)
  )
}

test_that("choose_k follows the silhouette when it is stable", {
  sel <- mk_selection(sil_k = 3, elbow = 4, gap = 2)
  expect_equal(choose_k(sel, min_stability = 0.90), 3L)
})

test_that("choose_k overrides an unstable silhouette optimum", {
  # Silhouette says 4, but k = 4 barely clears the floor and k = 5 is below
  # it. The policy must fall back to a stable, no-larger partition rather than
  # shipping an irreproducible one.
  sel <- mk_selection(sil_k = 4, elbow = 4, gap = 4,
                      ari = c(`2` = 0.96, `3` = 0.95, `4` = 0.70, `5` = 0.60))
  expect_equal(choose_k(sel, min_stability = 0.90), 3L)
})

test_that("choose_k never returns k = 1", {
  sel <- mk_selection(sil_k = 2, elbow = 4, gap = 1)
  k <- choose_k(sel, min_stability = 0.90)
  expect_gte(k, 2L)
})

test_that("choose_k handles the case where every candidate is unstable", {
  sel <- mk_selection(sil_k = 3, elbow = 3, gap = 3,
                      ari = c(`2` = 0.5, `3` = 0.4, `4` = 0.3, `5` = 0.2))
  k <- choose_k(sel, min_stability = 0.90)
  # Falls back rather than erroring; the value is the silhouette optimum.
  expect_equal(k, 3L)
})

test_that("select_k excludes k = 1 from the stability table", {
  # Regression: clusGap can return k = 1, and a k = 1 "stability" row is
  # trivially ARI = 1 against itself, which made k = 1 look like the most
  # stable partition in the table.
  set.seed(31)
  x <- scale(matrix(rnorm(300, 0, 1), ncol = 2))
  sel <- select_k(x, k_max = 6, nstart = 5, seed = 1)
  expect_true(all(sel$stability$k >= 2))
  expect_true(all(sel$stability$ari_mean <= 1))
})

test_that("cluster_matrix standardises and honours the configured feature set", {
  rfm <- data.frame(
    recency = c(1, 10, 100, 5),
    frequency = c(1, 2, 30, 3),
    monetary = c(10, 200, 9000, 50),
    avg_order_value = c(10, 100, 300, 17),
    monetary_log = log1p(c(10, 200, 9000, 50)),
    recency_log = log1p(c(1, 10, 100, 5)),
    frequency_log = log1p(c(1, 2, 30, 3)),
    aov_log = log1p(c(10, 100, 300, 17))
  )
  m <- cluster_matrix(rfm, features_used = CFG$cluster_features)

  expect_true(is.matrix(m))
  expect_equal(ncol(m), length(CFG$cluster_features))
  expect_equal(colnames(m), CFG$cluster_features)
  # Standardised: every column centred at 0 with unit SD.
  expect_true(all(abs(colMeans(m)) < 1e-9))
  expect_true(all(abs(apply(m, 2, sd) - 1) < 1e-9))
})

test_that("fit_segments labels segments in ascending order of value", {
  # k-means returns arbitrary cluster ids. Segment 1 must always be the
  # lowest-value group or any statement about "segment N" is unfalsifiable.
  set.seed(32)
  x <- rbind(
    matrix(rnorm(100, -5, 0.4), ncol = 2),
    matrix(rnorm(100,  0, 0.4), ncol = 2),
    matrix(rnorm(100,  5, 0.4), ncol = 2))
  x <- scale(x)

  x <- scale(x)
  colnames(x) <- c("V1", "V2")
  fit <- fit_segments(x, k = 3, order_by = "V1")
  expect_equal(levels(fit$labels), paste("Segment", 1:3))
  expect_equal(nrow(fit$centres), 3)
  expect_equal(rownames(fit$centres), levels(fit$labels))

  # Ordering is by the requested column only, so assert monotonicity there --
  # not on the row means, which need not be ordered.
  ord_vals <- fit$centres[, "V1"]
  expect_true(all(diff(ord_vals) >= -1e-9))

  # And each label must map back to a cluster whose centroid matches.
  for (lvl in levels(fit$labels)) {
    rows <- as.integer(fit$labels) == which(lvl == levels(fit$labels))
    expect_equal(mean(x[rows, "V1"]), fit$centres[which(lvl == levels(fit$labels)), "V1"])
  }
})

test_that("fit_segments rejects an order_by column that is not a feature", {
  x <- scale(matrix(rnorm(90, 0, 1), ncol = 3))
  expect_error(fit_segments(x, k = 2, order_by = "not_a_column"),
               "not one of the clustering columns")
})

test_that("fit_segments is reproducible under a fixed seed", {
  set.seed(33)
  x <- scale(matrix(rnorm(300, 0, 1), ncol = 3))
  a <- fit_segments(x, k = 3, seed = 42)
  b <- fit_segments(x, k = 3, seed = 42)
  expect_equal(as.character(a$labels), as.character(b$labels))
  expect_equal(a$centres, b$centres)
})

test_that("summarise_segments warns when a segment is a negligible sliver", {
  n <- 1000
  labels <- factor(c(rep("Segment 1", 2), rep("Segment 2", n - 2)))
  rfm <- data.frame(
    CustomerID = seq_len(n),
    recency = sample(1:100, n, TRUE),
    frequency = rpois(n, 3),
    monetary = runif(n, 10, 1000),
    avg_order_value = runif(n, 5, 100)
  )
  expect_warning(summarise_segments(rfm, labels), "degenerate")
})

test_that("summarise_segments does not warn on a balanced partition", {
  n <- 1000
  labels <- factor(rep(paste("Segment", 1:4), length.out = n))
  rfm <- data.frame(
    CustomerID = seq_len(n),
    recency = sample(1:100, n, TRUE),
    frequency = rpois(n, 3),
    monetary = runif(n, 10, 1000),
    avg_order_value = runif(n, 5, 100)
  )
  expect_silent(summarise_segments(rfm, labels))
})
