# ==============================================================================
# utils_io.R — CSV I/O helpers with safe append and deduplication
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Paper:    "Design-Conditional Prior Elicitation for DP Mixtures"
# Author:   JoonHo Lee
#
# Functions:
#   write_csv_safe()  — Write a data.frame to CSV with safe append behavior
#   read_csv_auto()   — Read CSV using data.table::fread if available, else base R
#   dedup_by_key()    — Remove duplicate rows by composite key columns
#   read_many_csv()   — Read and combine multiple CSV files with deduplication
#
# Dependencies: base R (optional: data.table for faster reads)
# Called by:    sim_runner.R, sim_runner_parallel.R, results_collect.R
# ==============================================================================

write_csv_safe <- function(df, path, append = FALSE) {
  stopifnot(is.data.frame(df))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (!append || !file.exists(path)) {
    utils::write.csv(df, path, row.names = FALSE)
  } else {
    utils::write.table(df, path, sep = ",", row.names = FALSE,
                       col.names = FALSE, append = TRUE)
  }
}

read_csv_auto <- function(path) {
  if (!file.exists(path)) stop(sprintf("File not found: %s", path))

  if (requireNamespace("data.table", quietly = TRUE)) {
    return(as.data.frame(data.table::fread(path, data.table = FALSE)))
  } else {
    return(utils::read.csv(path, stringsAsFactors = FALSE))
  }
}

dedup_by_key <- function(df, key_cols) {
  stopifnot(all(key_cols %in% names(df)))
  key <- do.call(paste, c(df[key_cols], sep = "|"))
  dup <- duplicated(key)
  if (any(dup)) {
    warning(sprintf("Detected %d duplicated rows by key; keeping first occurrence.", sum(dup)))
    df <- df[!dup, , drop = FALSE]
  }
  df
}

read_many_csv <- function(files, key_cols = NULL) {
  files <- unique(files)
  if (length(files) == 0) stop("No files provided to read_many_csv().")

  dfs <- lapply(files, function(f) {
    df <- read_csv_auto(f)
    df$source_file <- basename(f)
    df
  })
  out <- do.call(rbind, dfs)

  if (!is.null(key_cols)) out <- dedup_by_key(out, key_cols)
  out
}
