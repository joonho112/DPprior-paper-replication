#!/usr/bin/env Rscript
# ==============================================================================
# step4_plot_fig3.R — Generate main Figure 3
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Pipeline: Step 4a of 7 — Main figure generation
# Depends:  Step 3 (raw simulation results)
# Produces: figures/main/fig03_simulation_grid.{pdf,png}
# Runtime:  ~30 seconds
#
# Description:
#   Auto-detects raw results (merged > full_rep_* > pilot) and creates
#   the main Figure 3 (3×3 grid: K* × I, Vague/TSMM/Dual at medium conf).
#
# Usage:
#   Rscript scripts/step4_plot_fig3.R
#   Rscript scripts/step4_plot_fig3.R --config config/fig3_design.yaml
#
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
config_path <- "config/fig3_design.yaml"
if (length(args) >= 2 && args[1] == "--config") config_path <- args[2]

source("R/utils_config.R", local = TRUE)
source("R/results_collect.R", local = TRUE)
source("R/plot_fig3.R", local = TRUE)

cfg <- read_config(config_path)
ensure_project_dirs(cfg)

raw_df <- collect_raw_results(cfg, include_fail = TRUE)
cat("\n[Step 4] Input mode:", attr(raw_df, "input_mode"), "\n")
cat("[Step 4] Files used:\n")
print(attr(raw_df, "input_files"))

out_pdf <- cfg$plot$output_pdf
out_png <- cfg$plot$output_png

cat("\n[Step 4] Plotting main Figure 3...\n")
plot_fig3_main(raw_df, cfg, out_pdf, out_png)

cat("Saved:\n")
cat("  - ", out_pdf, "\n", sep = "")
cat("  - ", out_png, "\n", sep = "")
cat("Done.\n")
