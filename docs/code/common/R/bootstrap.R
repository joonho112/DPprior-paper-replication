# Author: JoonHo Lee (jlee296@ua.edu)
# Initialize paths and the local R library
#
# Find the project from the calling script, then load the shared path and file
# helpers. The caller supplies .script_dir, so moving the repository or
# running a script from another working directory does not require editing
# paths.
#
# Sourced by the R entrypoints. Defines .replication_root and shared helpers.

# Find the project marker using the caller's script directory.
if (!exists(".script_dir", inherits = FALSE)) stop("Entrypoint must define .script_dir.", call. = FALSE)
.bootstrap_root <- normalizePath(.script_dir, winslash = "/", mustWork = TRUE)
while (!file.exists(file.path(.bootstrap_root, ".replication-root"))) {
  .parent <- dirname(.bootstrap_root)
  if (identical(.parent, .bootstrap_root)) stop("Replication root marker not found.", call. = FALSE)
  .bootstrap_root <- .parent
}

# Load the shared helpers and prefer the project's pinned package library.
source(file.path(.bootstrap_root, "common", "R", "paths.R"), local = TRUE)
.replication_root <- rp_find_root(.script_dir)
.local_library <- file.path(.replication_root, ".library")
if (dir.exists(.local_library)) .libPaths(c(.local_library, .libPaths()))
source(file.path(.replication_root, "common", "R", "io.R"), local = TRUE)
rm(.bootstrap_root, .local_library)
