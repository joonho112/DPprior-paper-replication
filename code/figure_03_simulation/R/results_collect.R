# ==============================================================================
# results_collect.R — Collect and merge raw simulation results from disk
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Paper:    "Design-Conditional Prior Elicitation for DP Mixtures"
# Author:   JoonHo Lee
#
# Description:
#   Detects and loads raw simulation result files with the following priority:
#     1) results/sim/raw_results_merged.csv (if present)
#     2) results/sim/raw_results_full_rep_*.csv (glob, if present)
#     3) results/sim/raw_results_pilot.csv
#   Returns a single deduplicated data.frame ready for plotting.
#
# Functions:
#   detect_raw_result_files() — Find available result files by priority
#   collect_raw_results()     — Load and combine results into one data.frame
#
# Dependencies: R/utils_io.R (sourced internally)
# Called by:    scripts/step4_plot_figures.R
# ==============================================================================

source("R/utils_io.R", local = TRUE)

detect_raw_result_files <- function(sim_dir) {
  merged <- file.path(sim_dir, "raw_results_merged.csv")
  if (file.exists(merged)) {
    return(list(mode = "merged", files = merged))
  }

  full_files <- Sys.glob(file.path(sim_dir, "raw_results_full_rep_*.csv"))
  if (length(full_files) > 0) {
    return(list(mode = "full_glob", files = sort(full_files)))
  }

  pilot <- file.path(sim_dir, "raw_results_pilot.csv")
  if (file.exists(pilot)) {
    return(list(mode = "pilot", files = pilot))
  }

  stop("No raw results found. Expected one of: raw_results_merged.csv, ",
       "raw_results_full_rep_*.csv, raw_results_pilot.csv")
}

collect_raw_results <- function(cfg,
                                key_cols = c("replicate", "K_true", "I_target",
                                             "method", "confidence"),
                                include_fail = TRUE) {
  sim_dir <- cfg$paths$sim_dir
  det <- detect_raw_result_files(sim_dir)

  if (length(det$files) == 1) {
    df <- read_csv_auto(det$files)
    df$source_file <- basename(det$files)
  } else {
    df <- read_many_csv(det$files, key_cols = key_cols)
  }

  if (!include_fail && "status" %in% names(df)) {
    df <- df[df$status == "ok", , drop = FALSE]
  }

  attr(df, "input_mode")  <- det$mode
  attr(df, "input_files") <- det$files
  df
}
