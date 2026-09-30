#' Locate the project root
#'
#' Shiny and plumber both change or inherit the working directory, and
#' targets::tar_load() resolves its data store relative to that. Running the
#' app or API from inside app/ or api/ therefore failed with
#' "targets data store _targets not found". Both entry points call
#' set_project_root() first so they work from any directory.
set_project_root <- function(start = getwd()) {
  candidates <- c(start,
                  file.path(start, ".."),
                  file.path(start, "..", ".."),
                  normalizePath(file.path(start, ".."), mustWork = FALSE))
  candidates <- unique(normalizePath(candidates, mustWork = FALSE))
  for (cand in candidates) {
    if (dir.exists(file.path(cand, "R")) &&
        file.exists(file.path(cand, "_targets.R"))) {
      setwd(cand)
      return(invisible(cand))
    }
  }
  stop("Could not locate the project root (no R/ and _targets.R found near ",
       start, "). Run `make all` first.")
}
