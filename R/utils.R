# Shared helpers ---------------------------------------------------------------
# Diagnostics are implemented here explicitly rather than pulled from
# factoextra/NbClust one-liners: the point of the exercise is to show the
# method, and every function below is unit-tested against a reference
# computation or a Monte Carlo simulation.

suppressPackageStartupMessages({
  library(dplyr)
})

#' Timestamped progress logging
log_step <- function(fmt, ...) {
  msg <- if (length(list(...))) sprintf(fmt, ...) else fmt
  cat(sprintf("  [%s] %s\n", format(Sys.time(), "%H:%M:%S"), msg))
  invisible(msg)
}

#' Section header for console output
log_head <- function(fmt, ...) {
  cat("\n", strrep("-", 66), "\n", sep = "")
  msg <- if (length(list(...))) sprintf(fmt, ...) else fmt
  cat(msg, "\n", sep = "")
  invisible(msg)
}

# ---- Cluster-quality diagnostics ---------------------------------------------

#' Adjusted Rand Index between two labellings
#'
#' Hubert & Arabie (1985). Used to measure cluster *stability*: if resampling
#' the data and refitting produces a different partition, the original
#' partition was an artefact of the particular sample rather than a property
#' of the customers.
#'
#' @param a,b integer vectors of cluster assignments
#' @return numeric scalar in [-1, 1]
adjusted_rand_index <- function(a, b) {
  # `table` is called explicitly as base::table: dplyr also exports a `table`
  # method, and an accidental dispatch there silently returns a data frame
  # instead of the contingency table the formula needs.
  a <- as.integer(factor(a))
  b <- as.integer(factor(b))
  tab <- base::table(a, b)
  n <- sum(tab)
  if (n < 2) return(NA_real_)

  # C(x, 2), summed over the supplied counts. ARI is built from four
  # counts-of-pairs, each of which must be summed across cells or clusters:
  #   sum_ij = sum over cells   of C(n_ij,  2)
  #   sum_i  = sum over rows    of C(n_i.,  2)
  #   sum_j  = sum over columns of C(n_.j,  2)
  # Computing sum_ij as C(sum(tab), 2) instead is a subtle and very easy error:
  # it inflates the numerator and pushes ARI above 1.
  choose2 <- function(x) sum(x * (x - 1) / 2)
  sum_ij <- choose2(tab)
  sum_i  <- choose2(rowSums(tab))
  sum_j  <- choose2(colSums(tab))
  total  <- choose2(n)

  expected <- sum_i * sum_j / total
  max_index <- 0.5 * (sum_i + sum_j)
  if (isTRUE(all.equal(max_index, expected))) return(1)

  (sum_ij - expected) / (max_index - expected)
}

#' Total within-cluster sum of squares for a range of k
wss_curve <- function(x, k_max = 10, nstart = 25, seed = 1) {
  set.seed(seed)
  ks <- 2:k_max
  wss <- vapply(ks, function(k) {
    kmeans(x, centers = k, nstart = nstart, iter.max = 100)$tot.withinss
  }, numeric(1))
  data.frame(k = ks, wss = wss)
}

#' Locate the elbow of a monotone decreasing curve (maximum-curvature method)
#'
#' Satopaa et al. (2006) triangle-area approach: for each candidate point, fit
#' a line to the points before it and a line to the points after it, then take
#' the candidate maximising the triangle area between the two lines.
#'
#' Segments with fewer than two points have no slope to project onto, so
#' candidate indices are restricted to 3..(n-2); this needs at least 5 points.
#'
#' @param k,wss numeric vectors from [wss_curve()]
#' @return list(elbow_k, method, area, areas)
find_elbow <- function(k, wss) {
  n <- length(k)
  if (n < 5) stop("find_elbow needs at least 5 points, got ", n)
  if (!isTRUE(all(diff(wss) < 0))) stop("wss must be strictly decreasing")

  kn <- (k - min(k)) / diff(range(k))
  wn <- (wss - min(wss)) / diff(range(wss))

  # i = 3 gives two points on the left; i = n-2 gives two on the right.
  candidates <- 3:(n - 2)
  areas <- vapply(candidates, function(i) {
    left  <- lm.fit(cbind(1, kn[1:(i - 1)]), wn[1:(i - 1)])
    right <- lm.fit(cbind(1, kn[(i + 1):n]), wn[(i + 1):n])
    # Perpendicular distance of point i from each fitted line
    dl <- abs(left$coefficients[2] * kn[i] - wn[i] + left$coefficients[1]) /
      sqrt(left$coefficients[2]^2 + 1)
    dr <- abs(right$coefficients[2] * kn[i] - wn[i] + right$coefficients[1]) /
      sqrt(right$coefficients[2]^2 + 1)
    dl * dr * (kn[i + 1] - kn[i - 1]) / 2
  }, numeric(1))

  idx <- candidates[which.max(areas)]
  list(elbow_k = k[idx], method = "maximum curvature (Satopaa et al. 2006)",
       area = max(areas), areas = areas)
}

#' Mean silhouette width for each k
silhouette_curve <- function(x, k_max = 8, nstart = 25, seed = 1) {
  set.seed(seed)
  ks <- 2:k_max
  vapply(ks, function(k) {
    km <- kmeans(x, centers = k, nstart = nstart, iter.max = 100)
    mean(cluster::silhouette(km$cluster, dist(x))[, "sil_width"])
  }, numeric(1)) -> sil
  data.frame(k = ks, silhouette = sil)
}

#' Bootstrap cluster stability (mean ARI between full fit and refits)
#'
#' An in-sample fit statistic such as between_SS/total_SS says nothing about
#' whether the partition generalises. Stability does.
#'
#' @return list(ari_mean, ari_median, ari_p05, ari_perfect_share)
cluster_stability <- function(x, k, n_boot = 200, nstart = 10, seed = 1) {
  set.seed(seed)
  n <- nrow(x)
  ref <- kmeans(x, centers = k, nstart = nstart, iter.max = 100)$cluster

  aris <- vapply(seq_len(n_boot), function(b) {
    idx <- sample.int(n, n, replace = TRUE)
    boot <- kmeans(x[idx, , drop = FALSE], centers = k, nstart = nstart,
                   iter.max = 100)
    adjusted_rand_index(ref[idx], boot$cluster)
  }, numeric(1))

  list(
    ari_mean        = mean(aris),
    ari_median      = median(aris),
    ari_p05         = unname(quantile(aris, 0.05)),
    ari_perfect_share = mean(aris > 0.999)
  )
}

#' In-sample variance explained (reported, but NOT treated as validation)
#'
#' Note the field is `totss`, not `totalss` -- and note more importantly that
#' this is a training statistic. It says how well k-means fit the data it was
#' given, not whether the partition generalises.
variance_explained <- function(km) {
  km$betweenss / km$totss
}

# ---- Presentational helpers ---------------------------------------------------

#' Format a vector as currency with thousands separators
#'
#' base::sprintf does not accept the "," big-mark flag on all platforms, so
#' this formats numerically first and then wraps the result.
fmt_money <- function(x, dp = 0) {
  paste0(CFG$currency_symbol, format(round(as.numeric(x), dp), big.mark = ",",
                                     scientific = FALSE, trim = TRUE))
}

#' Format a proportion as a percentage string
fmt_pct <- function(x, dp = 1) {
  paste0(format(round(100 * as.numeric(x), dp), trim = TRUE), "%")
}

#' Format a vector of numbers as a markdown table
to_md_table <- function(df, digits = 3) {
  fmt <- function(x) {
    if (is.numeric(x)) {
      vapply(x, function(v) {
        if (is.na(v)) "" else formatC(v, format = "f", digits = digits,
                                       big.mark = ",")
      }, character(1))
    } else as.character(x)
  }
  rows <- apply(as.data.frame(df), 1, function(r) paste0("| ", paste(fmt(r), collapse = " | "), " |"))
  hdr  <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep  <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  paste(c(hdr, sep, rows), collapse = "\n")
}
