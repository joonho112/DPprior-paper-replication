# Author: JoonHo Lee (jlee296@ua.edu)
# Resolve project inputs and outputs
#
# All analysis paths are relative to the project marker. Check the resolved
# path as well as the requested path: an otherwise valid relative name can
# still leave the project through a symbolic link.
#
# Sourced by common/R/bootstrap.R. These helpers resolve paths without file I/O.

# Walk upward from a supplied script directory until the project marker is
# found. Stop at the filesystem root if the project cannot be identified.
rp_find_root <- function(start) {
  here <- normalizePath(start, winslash = "/", mustWork = TRUE)
  if (!dir.exists(here)) here <- dirname(here)
  repeat {
    if (file.exists(file.path(here, ".replication-root"))) return(here)
    parent <- dirname(here)
    if (identical(parent, here)) stop("No .replication-root marker above script location.", call. = FALSE)
    here <- parent
  }
}

# Join relative path components and check that both the lexical and resolved
# path stay within the project. must_exist is used for required inputs.
rp_path <- function(area, ..., must_exist = FALSE) {
  allowed <- c(
    "data-frozen", "data-derived", "outputs", "calibration", "simulation",
    "applications", "rasch", "refit", "reference-code", "exhibits",
    "provenance", "verification", "docs", "common", ".library"
  )
  if (length(area) != 1L || !area %in% allowed) stop("Unknown project area.", call. = FALSE)
  pieces <- c(area, unlist(list(...), use.names = FALSE))
  if (anyNA(pieces) ||
    any(grepl("(^[/\\\\]|^[A-Za-z]:|(^|[/\\\\])\\.\\.([/\\\\]|$))", pieces)))
    stop("Paths must be relative and cannot contain parent traversal.", call. = FALSE)
  out <- do.call(file.path, as.list(c(.replication_root, pieces)))
  # Resolve the nearest existing ancestor to catch symlinks even before the
  # requested output exists. Parent traversal has already been rejected above.
  ancestor <- out
  while (!file.exists(ancestor) && !dir.exists(ancestor)) ancestor <- dirname(ancestor)
  actual <- normalizePath(ancestor, winslash = "/", mustWork = TRUE)
  if (!(identical(actual, .replication_root) ||
    startsWith(actual, paste0(.replication_root, "/"))))
    stop("Path leaves the replication root through a symlink.", call. = FALSE)
  if (must_exist && !file.exists(out)) stop("Required input is missing: ",
    paste(pieces, collapse = "/"),
    call. = FALSE
  )
  out
}

# Restrict generated destinations to the two output areas; this helper does
# not create or overwrite a file.
rp_output_path <- function(area, ...) {
  if (!area %in% c("data-derived", "outputs"))
    stop("Builders may write only data-derived/ or outputs/.", call. = FALSE)
  rp_path(area, ...)
}
