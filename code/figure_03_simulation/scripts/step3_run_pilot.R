#!/usr/bin/env Rscript
# ==============================================================================
# step3_run_pilot.R — Pilot simulation (sequential, single-threaded)
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Pipeline: Step 3b of 7 — Pilot run (sequential)
# Depends:  Steps 1-2 (calibration + priors)
# Produces: results/sim/raw_results_pilot.csv
# Runtime:  ~15-30 minutes (3 reps × 9 scenarios × 5 methods)
#
# Description:
#   Runs a small number of replications (default: 3, from config) sequentially
#   to verify results before committing to the full 200-replication run.
#   For a faster alternative, use step3_run_pilot_parallel.R.
#
# Usage:
#   Rscript scripts/step3_run_pilot.R
#   Rscript scripts/step3_run_pilot.R --config config/fig3_design.yaml
#
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
config_path <- "config/fig3_design.yaml"
if (length(args) >= 2 && args[1] == "--config") config_path <- args[2]

source("R/utils_config.R", local = TRUE)
source("R/sim_runner.R", local = TRUE)

cfg <- read_config(config_path)

n_reps <- as.integer(cfg$replications$pilot_R)
out_csv <- file.path(cfg$paths$sim_dir, "raw_results_pilot.csv")

cat(sprintf("\n[Step 3 Pilot] Running %d replications per scenario...\n", n_reps))
run_simulation(cfg, n_reps = n_reps, out_csv = out_csv, rep_start = 1L, verbose = TRUE)

cat("Saved:", out_csv, "\n")
cat("Done.\n")
