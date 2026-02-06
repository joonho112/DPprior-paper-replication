#!/usr/bin/env Rscript
# ==============================================================================
# step1_calibrate_I.R — Calibrate sample sizes for target informativeness
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Pipeline: Step 1 of 7 — Informativeness calibration
# Depends:  Step 0 (directories created)
# Produces: results/sim/informativeness_calibration.csv
# Runtime:  ~1-2 minutes
#
# Description:
#   For each I_target in {0.2, 0.5, 0.8}, finds the mean sample size (n_mean)
#   such that E[I_realized] ≈ I_target via Monte Carlo grid search.
#
# Usage:
#   Rscript scripts/step1_calibrate_I.R
#   Rscript scripts/step1_calibrate_I.R --config config/fig3_design.yaml
#
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
config_path <- "config/fig3_design.yaml"
if (length(args) >= 2 && args[1] == "--config") config_path <- args[2]

source("R/utils_config.R", local = TRUE)
source("R/utils_io.R", local = TRUE)
source("R/calibrate_informativeness.R", local = TRUE)

cfg <- read_config(config_path)
ensure_project_dirs(cfg)

cat("\n[Step 1] Calibrating informativeness targets...\n")

tab <- run_calibration_table(cfg)
out_path <- file.path(cfg$paths$sim_dir, "informativeness_calibration.csv")
write_csv_safe(tab, out_path, append = FALSE)

cat("Saved:", out_path, "\n")
print(tab)
cat("\nDone.\n")
