#' Locate the project root
#'
#' Resolves relative to this helper file rather than the working directory, so
#' `test_dir("tests")` works from anywhere.
project_root <- function() {
  # sys.frames() at source() time carries the sourcing filename.
  this_file <- NULL
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && nzchar(of)) { this_file <- as.character(of)[1]; break }
  }

  candidates <- character(0)
  if (!is.null(this_file)) {
    d <- dirname(normalizePath(this_file, mustWork = FALSE))
    candidates <- c(candidates, dirname(d), d)
  }
  candidates <- c(candidates, normalizePath(".", mustWork = FALSE),
                  normalizePath("..", mustWork = FALSE))

  candidates <- unique(candidates[!is.na(candidates)])
  for (cand in candidates) {
    if (dir.exists(file.path(cand, "R"))) return(cand)
  }
  stop("Could not locate project root (no R/ directory found in: ",
       paste(candidates, collapse = ", "), ")")
}

#' Source every module in dependency order
load_project <- function(root = project_root()) {
  # 06_report.R is included so render_report() is testable: its "must not
  # silently skip" behaviour is one of the regression tests.
  files <- c("00_config.R", "root.R", "utils.R", "01_data.R", "02_features.R",
             "03_rfm.R", "04_cluster.R", "05_clv.R", "06_report.R")
  for (f in files) source(file.path(root, "R", f), local = FALSE)
  invisible(root)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(testthat)
})

load_project()
