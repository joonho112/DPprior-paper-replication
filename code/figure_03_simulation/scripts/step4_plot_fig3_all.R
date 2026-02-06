#!/usr/bin/env Rscript
# ==============================================================================
# step4_plot_fig3_all.R — Generate all Figure 3 variants
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Pipeline: Step 4b of 7 — All figure variants (main + supplement)
# Depends:  Step 3 (raw simulation results)
# Produces:
#   figures/main/fig03_simulation_grid.{pdf,png}
#   figures/supplement/fig03_simulation_grid_extended_baselines.{pdf,png}
#   figures/supplement/fig03_simulation_grid_conf_low.{pdf,png}
#   figures/supplement/fig03_simulation_grid_conf_medium.{pdf,png}
#   figures/supplement/fig03_simulation_grid_conf_high.{pdf,png}
# Runtime:  ~1-2 minutes
#
# Description:
#   Generates all Figure 3 variants at once:
#   - Main figure (Vague/TSMM/Dual, medium confidence)
#   - Extended baselines (+ DORO-uniform, chi2-DORO)
#   - Confidence sensitivity (low, medium, high)
#
# Usage:
#   Rscript scripts/step4_plot_fig3_all.R
#   Rscript scripts/step4_plot_fig3_all.R --config config/fig3_design.yaml
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
cat("\n[Step 4b] Input mode:", attr(raw_df, "input_mode"), "\n")
cat("[Step 4b] Files used:\n")
print(attr(raw_df, "input_files"))

out_pdf_main <- cfg$plot$output_pdf
out_png_main <- cfg$plot$output_png

out_pdf_ext  <- file.path(cfg$paths$figures_dir, "supplement", "fig03_simulation_grid_extended_baselines.pdf")
out_png_ext  <- file.path(cfg$paths$figures_dir, "supplement", "fig03_simulation_grid_extended_baselines.png")

out_pdf_low  <- file.path(cfg$paths$figures_dir, "supplement", "fig03_simulation_grid_conf_low.pdf")
out_png_low  <- file.path(cfg$paths$figures_dir, "supplement", "fig03_simulation_grid_conf_low.png")

out_pdf_med  <- file.path(cfg$paths$figures_dir, "supplement", "fig03_simulation_grid_conf_medium.pdf")
out_png_med  <- file.path(cfg$paths$figures_dir, "supplement", "fig03_simulation_grid_conf_medium.png")

out_pdf_high <- file.path(cfg$paths$figures_dir, "supplement", "fig03_simulation_grid_conf_high.pdf")
out_png_high <- file.path(cfg$paths$figures_dir, "supplement", "fig03_simulation_grid_conf_high.png")

cat("\n[Step 4b] Plotting: main...\n")
plot_fig3_main(raw_df, cfg, out_pdf_main, out_png_main)

cat("[Step 4b] Plotting: extended baselines...\n")
plot_fig3_extended_baselines(raw_df, cfg, out_pdf_ext, out_png_ext)

cat("[Step 4b] Plotting: confidence sensitivity (low/medium/high)...\n")
plot_fig3_conf_sensitivity(raw_df, cfg, "low", out_pdf_low, out_png_low)
plot_fig3_conf_sensitivity(raw_df, cfg, "medium", out_pdf_med, out_png_med)
plot_fig3_conf_sensitivity(raw_df, cfg, "high", out_pdf_high, out_png_high)

cat("\nSaved files:\n")
cat("  - ", out_pdf_main, "\n", sep = "")
cat("  - ", out_png_main, "\n", sep = "")
cat("  - ", out_pdf_ext, "\n", sep = "")
cat("  - ", out_png_ext, "\n", sep = "")
cat("  - ", out_pdf_low, "\n", sep = "")
cat("  - ", out_png_low, "\n", sep = "")
cat("  - ", out_pdf_med, "\n", sep = "")
cat("  - ", out_png_med, "\n", sep = "")
cat("  - ", out_pdf_high, "\n", sep = "")
cat("  - ", out_png_high, "\n", sep = "")

cat("Done.\n")
