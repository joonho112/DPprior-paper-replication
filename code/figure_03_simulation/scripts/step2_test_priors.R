#!/usr/bin/env Rscript
# ==============================================================================
# step2_test_priors.R — Verify the prior table
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Pipeline: Step 2b of 7 (optional verification)
# Depends:  Step 2 (prior_table.csv)
# Produces: Console output only (pass/fail checks)
# Runtime:  ~5 seconds
#
# Checks:
#   - Non-vague methods produce hyperparameters different from (1, 1)
#
# Usage:
#   Rscript scripts/step2_test_priors.R
#   Rscript scripts/step2_test_priors.R --config config/fig3_design.yaml
#
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
config_path <- "config/fig3_design.yaml"
if (length(args) >= 2 && args[1] == "--config") config_path <- args[2]

source("R/utils_config.R", local = TRUE)
source("R/utils_io.R", local = TRUE)

cfg <- read_config(config_path)
prior_path <- file.path(cfg$paths$priors_dir, "prior_table.csv")
if (!file.exists(prior_path)) stop("Prior table missing. Run step2_build_priors.R first.")

prior_df <- read_csv_auto(prior_path)

cat("\n[Step 2 Test] Prior table checks\n")
cat("Rows:", nrow(prior_df), "\n")

non_vague <- prior_df[prior_df$method != "vague", ]
if (all(abs(non_vague$a - 1) < 1e-12 & abs(non_vague$b - 1) < 1e-12)) {
  stop("All non-vague methods have a=b=1. Prior build likely failed.")
} else {
  cat("PASS: Non-vague methods produce non-(1,1) priors.\n")
}

cat("Done.\n")
