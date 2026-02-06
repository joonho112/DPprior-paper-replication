#!/usr/bin/env Rscript
# ==============================================================================
# step3_run_pilot_parallel.R — Pilot simulation (parallel)
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Pipeline: Step 3b' of 7 — Pilot run (parallel alternative)
# Depends:  Steps 1-2, future, future.apply packages
# Produces: results/sim/raw_results_pilot_parallel.csv
# Runtime:  ~5-10 minutes (depending on core count)
#
# Description:
#   Parallel version of step3_run_pilot.R. Uses future.apply for
#   replication-level parallelism with checkpointing.
#
# Usage:
#   Rscript scripts/step3_run_pilot_parallel.R
#   Rscript scripts/step3_run_pilot_parallel.R --workers 8 --chunk_size 1
#   Rscript scripts/step3_run_pilot_parallel.R --config config/fig3_design.yaml
#
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)

config_path <- "config/fig3_design.yaml"
workers <- NA
chunk_size <- 1L
plan <- NA

i <- 1
while (i <= length(args)) {
  if (args[i] == "--config") { config_path <- args[i + 1]; i <- i + 2; next }
  if (args[i] == "--workers") { workers <- as.integer(args[i + 1]); i <- i + 2; next }
  if (args[i] == "--chunk_size") { chunk_size <- as.integer(args[i + 1]); i <- i + 2; next }
  if (args[i] == "--plan") { plan <- args[i + 1]; i <- i + 2; next }
  i <- i + 1
}

source("R/utils_config.R", local = TRUE)
source("R/sim_runner_parallel.R", local = TRUE)

cfg <- read_config(config_path)

n_reps <- as.integer(cfg$replications$pilot_R)
rep_start <- 1L
rep_end <- n_reps

out_csv <- file.path(cfg$paths$sim_dir, "raw_results_pilot_parallel.csv")

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

cat(sprintf("\n[Step 3 Pilot | Parallel] reps=%d, workers=%d, chunk_size=%d\n",
            n_reps, workers, chunk_size))

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
