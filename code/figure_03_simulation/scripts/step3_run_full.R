#!/usr/bin/env Rscript
# ==============================================================================
# step3_run_full.R — Full simulation run (sequential, single-threaded)
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Pipeline: Step 3c of 7 — Full simulation (sequential)
# Depends:  Steps 1-2 (calibration + priors)
# Produces: results/sim/raw_results_full_rep_{start}_{end}.csv
# Runtime:  ~8-24 hours for 200 reps (depends on hardware)
#
# Description:
#   Sequential full simulation for a specified replication range.
#   For faster execution, prefer step3_run_full_parallel.R.
#
# Usage:
#   Rscript scripts/step3_run_full.R --rep_start 1 --rep_end 50
#   Rscript scripts/step3_run_full.R --config config/fig3_design.yaml --rep_start 1 --rep_end 100
#
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)

config_path <- "config/fig3_design.yaml"
rep_start <- NA
rep_end <- NA

i <- 1
while (i <= length(args)) {
  if (args[i] == "--config") { config_path <- args[i + 1]; i <- i + 2; next }
  if (args[i] == "--rep_start") { rep_start <- as.integer(args[i + 1]); i <- i + 2; next }
  if (args[i] == "--rep_end") { rep_end <- as.integer(args[i + 1]); i <- i + 2; next }
  i <- i + 1
}

if (is.na(rep_start) || is.na(rep_end)) stop("You must specify --rep_start and --rep_end.")

source("R/utils_config.R", local = TRUE)
source("R/sim_runner.R", local = TRUE)

cfg <- read_config(config_path)

out_csv <- file.path(cfg$paths$sim_dir, sprintf("raw_results_full_rep_%d_%d.csv", rep_start, rep_end))
n_reps <- rep_end - rep_start + 1L

cat(sprintf("\n[Step 3 Full] Running replications %d..%d (%d reps)\n", rep_start, rep_end, n_reps))
run_simulation(cfg, n_reps = n_reps, out_csv = out_csv, rep_start = rep_start, rep_end = rep_end, verbose = TRUE)

cat("Saved:", out_csv, "\n")
cat("Done.\n")
