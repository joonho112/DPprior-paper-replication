#!/usr/bin/env Rscript
# ==============================================================================
# step3_run_full_parallel.R — Full simulation run (parallel, recommended)
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Pipeline: Step 3c' of 7 — Full simulation (parallel, recommended)
# Depends:  Steps 1-2, future, future.apply packages
# Produces: results/sim/raw_results_full_rep_{start}_{end}.csv
# Runtime:  ~4-8 hours for 200 reps (depends on core count)
#
# Description:
#   Parallel full simulation using future.apply with replication-level
#   checkpointing. Each replication is written to a separate CSV part file,
#   then merged into a single output CSV.
#
# Usage:
#   Rscript scripts/step3_run_full_parallel.R --rep_start 1 --rep_end 100 --workers 8
#   Rscript scripts/step3_run_full_parallel.R --rep_start 101 --rep_end 200 --workers 8
#
# Tip: Split into multiple ranges (e.g., 1-100, 101-200) and run separately
#      for better checkpoint granularity. Merge afterwards with step3_merge.
#
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)

config_path <- "config/fig3_design.yaml"
rep_start <- 1
rep_end <- 200
workers <- NA
chunk_size <- 1L
plan <- NA

i <- 1
while (i <= length(args)) {
  if (args[i] == "--config") { config_path <- args[i + 1]; i <- i + 2; next }
  if (args[i] == "--rep_start") { rep_start <- as.integer(args[i + 1]); i <- i + 2; next }
  if (args[i] == "--rep_end") { rep_end <- as.integer(args[i + 1]); i <- i + 2; next }
  if (args[i] == "--workers") { workers <- as.integer(args[i + 1]); i <- i + 2; next }
  if (args[i] == "--chunk_size") { chunk_size <- as.integer(args[i + 1]); i <- i + 2; next }
  if (args[i] == "--plan") { plan <- args[i + 1]; i <- i + 2; next }
  i <- i + 1
}

if (is.na(rep_start) || is.na(rep_end)) {
  stop("You must specify --rep_start and --rep_end.")
}

source("R/utils_config.R", local = TRUE)
source("R/sim_runner_parallel.R", local = TRUE)

cfg <- read_config(config_path)

if (is.na(workers)) {
  if (!is.null(cfg$parallel) && !is.null(cfg$parallel$workers)) {
    workers <- as.integer(cfg$parallel$workers)
  } else {
    workers <- get_default_workers()
  }
}

if (is.na(chunk_size) || chunk_size < 1L) {
  if (!is.null(cfg$parallel) && !is.null(cfg$parallel$chunk_size)) {
    chunk_size <- as.integer(cfg$parallel$chunk_size)
  } else {
    chunk_size <- 1L
  }
}

out_csv <- file.path(cfg$paths$sim_dir, sprintf("raw_results_full_rep_%d_%d.csv", rep_start, rep_end))
n_reps <- rep_end - rep_start + 1L

cat(sprintf("\n[Step 3 Full | Parallel] Running replications %d..%d (%d reps)\n", rep_start, rep_end, n_reps))
cat(sprintf("  workers=%d, chunk_size=%d\n", workers, chunk_size))

run_simulation_parallel(
  cfg = cfg,
  rep_start = rep_start,
  rep_end = rep_end,
  out_csv = out_csv,
  workers = workers,
  chunk_size = chunk_size,
  plan = if (is.na(plan)) NULL else plan,
  verbose = TRUE
)

cat("Saved:", out_csv, "\n")
cat("Done.\n")
