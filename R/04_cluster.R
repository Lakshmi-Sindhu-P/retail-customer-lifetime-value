# 04 - Clustering --------------------------------------------------------------
# The original notebook reported between_SS/total_SS = 58.2% and called it
# done. That is an in-sample statistic: it measures how well k-means fit the
# data it was given, not whether the partition generalises. Everything below
# is either out-of-sample or a resampling diagnostic.

#' Select k using three criteria plus a stability check
select_k <- function(features, k_max = CFG$k_max, nstart = CFG$nstart,
                     seed = CFG$seed) {
  # `features` is already a standardised matrix (see cluster_matrix()).
  x_scaled <- as.matrix(features)
  log_head("Choosing k")

  wss <- wss_curve(x_scaled, k_max = k_max, nstart = nstart, seed = seed)
  elbow <- find_elbow(wss$k, wss$wss)
  cat(sprintf("  elbow       : k = %d  (%s)\n", elbow$elbow_k, elbow$method))

  # Silhouette
  sil <- silhouette_curve(x_scaled, k_max = min(k_max, 8),
                          nstart = nstart, seed = seed)
  sil_k <- sil$k[which.max(sil$silhouette)]
  cat(sprintf("  silhouette  : k = %d  (width %.3f)\n", sil_k, max(sil$silhouette)))

  # Gap statistic.
  #
  # k = 1 is excluded: a single cluster always has an ARI of 1 against itself
  # and a trivial gap, so including it both distorts the argmax and injects a
  # meaningless "perfectly stable" row into the stability table.
  gap <- tryCatch({
    # clusGap() calls kmeans() with default arguments, and stats::kmeans
    # defaults to iter.max = 10, so its internal refits emit "did not converge"
    # on every run. The warning refers to those throwaway refits, not to the
    # clusterings we ship, so it is muted here rather than left to bury real
    # warnings in the pipeline log.
    g <- suppressWarnings(
      cluster::clusGap(x_scaled, FUN = kmeans, nstart = nstart,
                       K.max = min(k_max, 8), B = 30))
    tab <- g$Tab[, "gap"]
    if (length(tab) < 2) NA_integer_ else as.integer(which.max(tab[-1]) + 1)
  }, error = function(e) NA_integer_)
  cat(sprintf("  gap stat    : k = %s\n", gap))

  # Stability for a shortlist of candidates
  candidates <- sort(unique(c(2:6, elbow$elbow_k, sil_k, gap)))
  candidates <- candidates[!is.na(candidates)]
  candidates <- candidates[candidates >= 2]
  log_step("stability over k = {%s}",
           paste(candidates, collapse = ", "))

  stab <- lapply(candidates, function(k) {
    s <- cluster_stability(x_scaled, k = k, n_boot = 100,
                           nstart = 10, seed = seed)
    data.frame(k = k, ari_mean = s$ari_mean, ari_p05 = s$ari_p05,
               stable_share = s$ari_perfect_share)
  }) %>% bind_rows()

  print(as.data.frame(stab))

  list(wss = wss, silhouette = sil, elbow_k = elbow$elbow_k,
       silhouette_k = sil_k, gap_k = gap, stability = stab)
}

#' Held-out evaluation of a fitted partition
#'
#' Fit on a training split, score the *held-out* customers: assign each to
#' the nearest fitted centre and measure how well-separated they are. Also
#' reports adjusted Rand between the training fit and the full-data fit, which
#' is a direct measure of how much the partition moves when 30% of the
#' customers are removed.
evaluate_partition <- function(features, k, nstart = CFG$nstart,
                               seed = CFG$seed, holdout = 0.3) {
  set.seed(seed)
  x <- as.matrix(features)   # standardised matrix, see cluster_matrix()
  n <- nrow(x)

  train_idx <- sample.int(n, floor((1 - holdout) * n))
  test_idx  <- setdiff(seq_len(n), train_idx)

  xtr <- x[train_idx, , drop = FALSE]
  xte <- x[test_idx,  , drop = FALSE]

  km_tr <- kmeans(xtr, centers = k, nstart = nstart, iter.max = 100)

  # Held-out silhouette.
  #
  # Each unseen customer is assigned to its nearest *fitted* centre, and the
  # silhouette is then computed on the pairwise distances among the held-out
  # points alone. Distance-to-centre and distance-between-points are different
  # quantities; using one for the other is a common and subtle error.
  centres <- km_tr$centers
  d_centre <- sapply(seq_len(nrow(centres)), function(j)
    colSums((t(xte) - centres[j, ])^2))
  assign_ho <- max.col(-d_centre, ties.method = "first")

  d_test <- dist(xte)
  sil_ho <- mean(cluster::silhouette(assign_ho, d_test)[, "sil_width"])

  km_full <- kmeans(x, centers = k, nstart = nstart, iter.max = 100)

  list(
    k = k,
    n_train = length(train_idx),
    n_test  = length(test_idx),
    wss_ratio_full     = variance_explained(km_full),
    wss_ratio_train    = variance_explained(km_tr),
    silhouette_full    = mean(cluster::silhouette(km_full$cluster, dist(x))[, "sil_width"]),
    silhouette_holdout = sil_ho,
    # The two fits cover different subsets (train vs all), so their labellings
    # are compared over the training customers only.
    ari_train_vs_full  = adjusted_rand_index(km_full$cluster[train_idx],
                                             km_tr$cluster)
  )
}

#' Fit the final clustering and attach ordered labels
#'
#' k-means returns clusters in arbitrary order, so the raw integer labels are
#' meaningless: "cluster 3" would denote a different group on every rerun. The
#' labels are therefore remapped by ascending mean monetary value, making
#' Segment 1 the lowest-value group and the highest-value group Segment k,
#' deterministically.
#'
#' @return list(km, labels, centres, k)
fit_segments <- function(features, k, nstart = CFG$nstart, seed = CFG$seed,
                         order_by = NULL) {
  set.seed(seed)
  x <- as.matrix(features)
  if (is.null(colnames(x))) {
    colnames(x) <- paste0("V", seq_len(ncol(x)))
  }
  fit <- kmeans(x, centers = k, nstart = nstart, iter.max = 100)

  # Order clusters by mean value so segment numbers mean something. The
  # default is the first column; pass order_by to sort on a specific feature.
  if (is.null(order_by)) order_by <- colnames(x)[1]
  if (length(order_by) != 1 || !order_by %in% colnames(x)) {
    stop("order_by = '", paste(order_by, collapse = ", "),
         "' is not one of the clustering columns: ",
         paste(colnames(x), collapse = ", "))
  }
  col <- match(order_by, colnames(x))

  # Mean of the ordering column within each cluster.
  #
  # Note the split argument: tapply() passes SUBSETS of X to FUN, so passing
  # fit$cluster as X would hand FUN the cluster labels themselves rather than
  # row indices. df$grp makes FUN receive genuine row indices.
  df <- data.frame(grp = fit$cluster, val = x[, col])
  means <- tapply(df$val, df$grp, mean)
  ord <- names(sort(means))
  labels_by_cluster <- stats::setNames(paste("Segment", seq_along(ord)), ord)

  labels <- factor(labels_by_cluster[as.character(fit$cluster)],
                   levels = paste("Segment", seq_along(ord)))

  centres <- fit$centers[ord, , drop = FALSE]
  rownames(centres) <- levels(labels)

  fit$cluster  <- labels
  fit$centers  <- centres
  fit$center   <- centres[fit$cluster, , drop = FALSE]

  list(km = fit, labels = labels, centres = centres, k = k)
}

#' Resolve a single k from competing diagnostics
#'
#' The three criteria routinely disagree -- on this data elbow prefers 4,
#' silhouette prefers 2, gap statistic prefers 1 -- so the choice has to be a
#' stated policy rather than a silent argmax. Averaging the criteria, as the
#' original notebook did ("majority rule"), is not defensible because they are
#' not equally trustworthy: silhouette directly measures separation and
#' stability measures reproducibility, while elbow and gap are shape-based
#' heuristics that are known to disagree with each other.
#'
#' Policy applied here:
#'
#'   1. Candidate k values must be *stable* (bootstrap ARI >= min_stability).
#'      An unstable partition is not a segmentation, whatever its silhouette.
#'   2. Among stable candidates, take the silhouette optimum.
#'
#' The original project chose k = 3 by majority vote while its own silhouette
#' curve peaked at 4. That override was never explained, and the resulting
#' cluster centres were quoted in the write-up with a contradiction attached
#' (the text said "cluster 3 has the most recent transactions" while the table
#' beside it showed otherwise).
choose_k <- function(k_sel, min_stability = CFG$min_stability) {
  log_head("Choosing k")

  sil_k <- k_sel$silhouette_k
  stab  <- k_sel$stability
  elbow <- k_sel$elbow_k
  gap   <- k_sel$gap_k

  cat(sprintf("  elbow          : k = %s\n", elbow))
  cat(sprintf("  silhouette     : k = %d\n", sil_k))
  cat(sprintf("  gap statistic  : k = %s\n", gap))
  print(as.data.frame(stab))

  sil_row <- stab[stab$k == sil_k, ]
  sil_ari <- if (nrow(sil_row)) sil_row$ari_mean else NA_real_

  if (!is.na(sil_ari) && sil_ari < min_stability) {
    stable <- stab$k[stab$ari_mean >= min_stability]
    chosen <- if (length(stable)) max(stable[stable <= sil_k]) else sil_k
    log_step("silhouette k = %d is unstable (ARI %.3f < %.2f); using k = %d",
             sil_k, sil_ari, min_stability, chosen)
    return(as.integer(chosen))
  }

  if (elbow != sil_k || (is.na(gap) || gap != sil_k)) {
    log_step("criteria disagree (elbow %s / silhouette %d / gap %s);",
             elbow, sil_k, gap)
    log_step("following the silhouette subject to a stability floor of %.2f",
             min_stability)
  }
  log_step("chosen k = %d (bootstrap stability ARI %.3f)", sil_k, sil_ari)

  as.integer(sil_k)
}

#' Summarise each segment against the whole base
summarise_segments <- function(rfm, labels) {
  seg <- rfm %>% mutate(segment = labels)

  # Warn on a degenerate partition. A cluster holding a handful of customers
  # out of thousands is a real property of the data (extreme wholesale
  # accounts), but it should never be published without being flagged.
  sizes <- table(seg$segment)
  if (min(sizes) < 0.01 * length(seg$segment)) {
    msg <- sprintf(paste0("degenerate segment: smallest holds %d of %d ",
                          "customers (%.2f%%). Treat it as a tail/wholesale ",
                          "group, not a broad segment."),
                   min(sizes), length(seg$segment),
                   100 * min(sizes) / length(seg$segment))
    log_head("WARNING: %s", msg)
    print(sizes)
    warning(msg, call. = FALSE)
  }

  seg %>%
    group_by(segment) %>%
    summarise(
      customers        = n(),
      share_of_base    = n() / nrow(seg),
      recency_median   = median(recency),
      frequency_median = median(frequency),
      monetary_median  = median(monetary),
      aov_median       = median(avg_order_value),
      revenue_share    = sum(monetary) / sum(seg$monetary),
      .groups = "drop"
    ) %>%
    arrange(desc(customers))
}
