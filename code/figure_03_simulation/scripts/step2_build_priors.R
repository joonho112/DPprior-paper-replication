#!/usr/bin/env Rscript
# ==============================================================================
# step2_build_priors.R — Build Gamma hyperprior table for all methods
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Pipeline: Step 2 of 7 — Prior calibration
# Depends:  Step 1 (calibration), DPprior package
# Produces: results/priors/prior_table.csv
# Runtime:  ~30 seconds
#
# Description:
#   Computes Gamma(a, b) hyperparameters for 5 methods:
#   vague, TSMM, Dual-Anchor, DORO-uniform, chi2-DORO
#   across all (K_true, confidence) combinations.
#
# Usage:
#   Rscript scripts/step2_build_priors.R
#   Rscript scripts/step2_build_priors.R --config config/fig3_design.yaml
#
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
config_path <- "config/fig3_design.yaml"
if (length(args) >= 2 && args[1] == "--config") config_path <- args[2]

source("R/utils_config.R", local = TRUE)
source("R/utils_io.R", local = TRUE)
source("R/prior_build.R", local = TRUE)

cfg <- read_config(config_path)
ensure_project_dirs(cfg)

cat("\n[Step 2] Building prior table...\n")
prior_df <- build_prior_table(cfg)

out_path <- file.path(cfg$paths$priors_dir, "prior_table.csv")
write_csv_safe(prior_df, out_path, append = FALSE)

cat("Saved:", out_path, "\n")
print(prior_df)
cat("\nDone.\n")
