#!/usr/bin/env Rscript
# ==============================================================================
# step3_merge_results.R — Merge raw simulation result files
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Pipeline: Step 3d of 7 — Merge and deduplicate results
# Depends:  Step 3c or 3c' (one or more raw_results_*.csv files)
# Produces: results/sim/raw_results_merged.csv
# Runtime:  ~10-30 seconds
#
# Description:
#   Merges all raw_results_full_rep_*.csv and raw_results_pilot*.csv files
#   into a single deduplicated CSV for downstream plotting.
#
# Usage:
#   Rscript scripts/step3_merge_results.R
#   Rscript scripts/step3_merge_results.R --config config/fig3_design.yaml
#
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
config_path <- "config/fig3_design.yaml"
if (length(args) >= 2 && args[1] == "--config") config_path <- args[2]

source("R/utils_config.R", local = TRUE)
source("R/utils_io.R", local = TRUE)

cfg <- read_config(config_path)
ensure_project_dirs(cfg)

sim_dir <- cfg$paths$sim_dir
full_files <- Sys.glob(file.path(sim_dir, "raw_results_full_rep_*.csv"))
pilot_files <- Sys.glob(file.path(sim_dir, "raw_results_pilot*.csv"))

files <- c(sort(full_files), sort(pilot_files))
files <- unique(files[file.exists(files)])

if (length(files) == 0) stop("No raw results files found to merge.")

cat("\n[Step 3d] Merging raw results files:\n")
print(files)

df <- read_many_csv(files, key_cols = c("replicate", "K_true", "I_target", "method", "confidence"))

out_path <- file.path(sim_dir, "raw_results_merged.csv")
write_csv_safe(df, out_path, append = FALSE)

cat("Saved merged results:", out_path, "\n")
cat("Rows:", nrow(df), "\n")
cat("Done.\n")
