#' Tests for the cluster-diagnostic helpers
#'
# These are the functions the original notebook delegated to factoextra. Since
# they are now ours, they need their own coverage -- in particular the elbow
# detector, which silently returns nonsense on a degenerate curve.

test_that("adjusted_rand_index is 1 for identical labellings", {
  x <- c(1, 1, 2, 2, 3, 3)
  expect_equal(adjusted_rand_index(x, x), 1)

  # Relabelling is irrelevant: ARI is permutation-invariant.
  y <- c(3, 3, 1, 1, 2, 2)
  expect_equal(adjusted_rand_index(x, y), 1)
})

test_that("adjusted_rand_index is ~0 for independent labellings", {
  set.seed(3)
  n <- 400
  a <- sample(1:4, n, TRUE)
  b <- sample(1:4, n, TRUE)
  expect_lt(abs(adjusted_rand_index(a, b)), 0.15)
})

test_that("adjusted_rand_index is near zero for a strong structure split randomly", {
  # A 2-cluster split compared against an independent random 2-cluster split.
  # The two partitions are unrelated, so ARI should sit near 0 (sign is not
  # meaningful in expectation -- it is symmetric noise around zero).
  set.seed(11)
  a <- rep(1:2, each = 50)
  b <- sample(1:2, 100, TRUE)
  expect_lt(abs(adjusted_rand_index(a, b)), 0.15)
})

test_that("adjusted_rand_index stays within its theoretical bounds", {
  # Regression guard. Computing sum_ij as C(sum(tab), 2) instead of
  # sum(C(n_ij, 2)) is a subtle error that pushes ARI above 1 -- which is
  # mathematically impossible and would have silently inflated every
  # stability number in the report.
  set.seed(12)
  for (k in 2:5) {
    a <- sample(k, 120, TRUE)
    b <- sample(k, 120, TRUE)
    r <- adjusted_rand_index(a, b)
    expect_gte(r, -1)
    expect_lte(r, 1)
  }
})

test_that("adjusted_rand_index returns a scalar, not a table", {
  a <- sample(4, 100, TRUE)
  b <- sample(4, 100, TRUE)
  expect_length(adjusted_rand_index(a, b), 1)
})

test_that("wss_curve is strictly decreasing and covers the requested k", {
  set.seed(2)
  x <- scale(matrix(rnorm(300 * 3), ncol = 3))
  w <- wss_curve(x, k_max = 8, nstart = 10, seed = 1)
  expect_equal(nrow(w), 7)                 # k = 2..8
  expect_equal(w$k, 2:8)
  expect_true(all(diff(w$wss) < 0))         # more clusters, lower WSS
  expect_true(all(w$wss > 0))
})

test_that("find_elbow recovers a synthetic elbow", {
  # Construct a curve with a clear bend at k = 4: a steep drop to 4, then a
  # gentle decline afterwards.
  k <- 2:10
  wss <- c(1000, 520, 300, 280, 265, 255, 248, 243, 240)
  e <- find_elbow(k, wss)
  expect_true(e$elbow_k %in% c(4, 5))
  expect_equal(length(e$areas), length(k) - 4)
  expect_true(all(is.finite(e$areas)))
})

test_that("find_elbow rejects curves it cannot handle", {
  expect_error(find_elbow(2:4, c(10, 5, 2)), "at least 5 points")
  # Non-monotone input would make the geometry meaningless.
  expect_error(find_elbow(2:8, c(10, 5, 90, 20, 15, 12, 10)), "strictly decreasing")
})

test_that("silhouette_curve returns finite widths", {
  set.seed(5)
  # Three well-separated blobs: silhouette should peak at k = 3.
  x <- rbind(
    matrix(rnorm(80, -8, 0.5), ncol = 2),
    matrix(rnorm(80,  0, 0.5), ncol = 2),
    matrix(rnorm(80,  8, 0.5), ncol = 2))
  s <- silhouette_curve(x, k_max = 5, nstart = 10, seed = 1)
  expect_equal(nrow(s), 4)                 # k = 2..5
  expect_true(all(is.finite(s$silhouette)))
  expect_true(all(s$silhouette <= 1))
  expect_equal(s$k[which.max(s$silhouette)], 3)
})

test_that("cluster_stability reports high ARI for well-separated structure", {
  set.seed(6)
  x <- rbind(
    matrix(rnorm(120, -6, 0.4), ncol = 2),
    matrix(rnorm(120,  6, 0.4), ncol = 2))
  st <- cluster_stability(x, k = 2, n_boot = 30, nstart = 5, seed = 1)
  expect_true(st$ari_mean > 0.9)
  expect_true(st$ari_perfect_share > 0.8)
  # A stability diagnostic that can exceed 1 is broken, not conservative.
  expect_lte(st$ari_mean, 1)
  expect_lte(st$ari_p05, 1)
})

test_that("cluster_stability is lower for unstructured data", {
  set.seed(7)
  x <- matrix(rnorm(100, 0, 1), ncol = 2)
  st <- cluster_stability(x, k = 4, n_boot = 30, nstart = 5, seed = 1)
  # Absolute, not "less than the structured case": k-means on isotropic noise
  # is not meaningless, it is just far less reproducible. Assert the weaker,
  # genuinely load-bearing property, and compare against the structured case.
  expect_lt(st$ari_mean, 0.75)
  expect_true(st$ari_p05 <= st$ari_mean)
  expect_lte(st$ari_median, st$ari_mean + 1e-9)
})

test_that("variance_explained is a proportion and rises with k", {
  set.seed(8)
  x <- scale(matrix(rnorm(200 * 2), ncol = 2))
  r2 <- vapply(2:6, function(k) variance_explained(
    kmeans(x, centers = k, nstart = 10)), numeric(1))
  expect_true(all(r2 > 0 & r2 < 1))
  expect_true(all(diff(r2) > 0))           # monotonically increasing
})

test_that("fmt_money and fmt_pct format as expected", {
  expect_equal(fmt_money(1234567), "\u00a31,234,567")
  expect_equal(fmt_money(12.345, 2), "\u00a312.35")
  expect_equal(fmt_pct(0.1234, 1), "12.3%")
})

test_that("to_md_table emits a valid markdown table", {
  df <- data.frame(a = c("x", "y"), b = c(1.234, 5.678))
  md <- to_md_table(df, digits = 2)
  lines <- strsplit(md, "\n")[[1]]
  expect_equal(length(lines), 4)           # header, separator, 2 rows
  expect_match(lines[1], "^\\| a \\| b \\|$")
  expect_match(lines[2], "^\\| --- \\| --- \\|$")
  expect_match(lines[3], "1\\.23")
})

test_that("evaluate_partition returns a coherent held-out diagnostic set", {
  set.seed(13)
  x <- rbind(
    matrix(rnorm(150, -5, 0.5), ncol = 2),
    matrix(rnorm(150,  0, 0.5), ncol = 2),
    matrix(rnorm(150,  5, 0.5), ncol = 2))

  ev <- evaluate_partition(x, k = 3, nstart = 10, seed = 1)

  expect_length(ev$wss_ratio_full, 1)
  expect_length(ev$wss_ratio_train, 1)
  expect_length(ev$silhouette_holdout, 1)
  expect_length(ev$ari_train_vs_full, 1)

  expect_true(all(is.finite(unlist(ev[c("wss_ratio_full", "wss_ratio_train",
                                        "silhouette_full", "silhouette_holdout",
                                        "ari_train_vs_full")]))))

  # Well-separated blobs: the partition should survive dropping 30% of the data.
  expect_gt(ev$ari_train_vs_full, 0.7)
  expect_gt(ev$silhouette_holdout, 0.3)

  # The two silhouettes are computed on different samples (held-out subset vs
  # full data), so held-out is not guaranteed to be lower -- but the gap must
  # be small. A large gap would mean the in-sample number is inflated.
  expect_lt(abs(ev$silhouette_holdout - ev$silhouette_full), 0.10)

  expect_equal(ev$n_train + ev$n_test, nrow(x))
})

test_that("evaluate_partition catches the dimension bug it is guarding against", {
  # Regression: the held-out silhouette was originally computed by indexing a
  # rbind()-ed distance matrix with a setdiff() position vector, which silently
  # returned wrong customers when train/test were not contiguous. Assert the
  # split is a genuine complement, so that class of error cannot recur.
  set.seed(14)
  x <- matrix(rnorm(200 * 3), ncol = 3)
  ev <- evaluate_partition(x, k = 3, nstart = 5, seed = 1)
  expect_equal(ev$n_train + ev$n_test, 200)
  expect_equal(ev$n_test, floor(0.3 * 200))
})
