#!/usr/bin/env Rscript
# ==============================================================================
# step1_test_calibration.R — Verify informativeness calibration results
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Pipeline: Step 1b of 7 (optional verification)
# Depends:  Step 1 (informativeness_calibration.csv)
# Produces: Console output only (pass/fail checks)
# Runtime:  ~5 seconds
#
# Checks:
#   - n_mean is strictly increasing with I_target
#   - |mean_I - I_target| <= tolerance for all rows
#
# Usage:
#   Rscript scripts/step1_test_calibration.R
#   Rscript scripts/step1_test_calibration.R --config config/fig3_design.yaml
#
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
config_path <- "config/fig3_design.yaml"
if (length(args) >= 2 && args[1] == "--config") config_path <- args[2]

source("R/utils_config.R", local = TRUE)
source("R/utils_io.R", local = TRUE)

cfg <- read_config(config_path)
calib_path <- file.path(cfg$paths$sim_dir, "informativeness_calibration.csv")
if (!file.exists(calib_path)) stop("Calibration file missing. Run step1_calibrate_I.R first.")

tab <- read_csv_auto(calib_path)

cat("\n[Step 1 Test] Calibration table checks\n")
print(tab)

ord <- order(tab$I_target)
n_mean_sorted <- tab$n_mean[ord]
if (any(diff(n_mean_sorted) <= 0)) stop("n_mean is not strictly increasing with I_target.")
cat("PASS: n_mean increases with I_target.\n")

tol <- 0.03
err <- abs(tab$mean_I - tab$I_target)
if (any(err > tol)) {
  warning(sprintf("Some rows have |mean_I - I_target| > %.2f. Consider increasing calibration$n_sims.", tol))
} else {
  cat(sprintf("PASS: all |mean_I - I_target| <= %.2f\n", tol))
}

cat("Done.\n")
