#!/usr/bin/env Rscript
# ==============================================================================
# R_analysis_code.R — R (ggplot2) companion analysis for simulation results
# ==============================================================================
#
# Part of:  DPprior Replication Package (Simulation Analysis Pipeline)
# Role:     Supplementary R-based analysis (companion to Python pipeline)
# Depends:  raw_results_merged.csv (from simulation), tidyverse, ggplot2,
#           scales, patchwork, knitr, kableExtra, xtable
# Produces: Alternative R-based figures and tables (mirrors Python pipeline)
#
# Description:
#   This script provides an R/ggplot2 implementation of the same analysis
#   performed by the Python pipeline (01-03_*.py). It can be used as an
#   alternative or for cross-validation of the Python results. The figures
#   use the same Okabe-Ito colorblind-safe palette and publication-quality
#   formatting.
#
# Figure-to-Paper Mapping (same as Python pipeline):
#   figure1_main_grid            → Figure 3  (Main Text)
#   figure3_collapse_probability → Figure 4  (Main Text)
#   figure4_bias_distribution    → Figure F1 (Appendix F)
#   figure2_extended_grid        → Figure F2 (Appendix F)
#   figure5_mse_comparison       → Figure F3 (Appendix F)
#   figure6_coverage             → Figure F5 (Appendix F)
#   figure7_ci_width             → Figure F6 (Appendix F)
#   figure10_heatmap             → Figure F4 (Appendix F)
#   figure14_summary_panel       → Figure F7 (Appendix F)
#   figure8_confidence_sens.     → Figure F9 (Appendix F)
#
# Usage:
#   cd <repo-root>
#   Rscript code/figure_03_simulation/analysis/R_analysis_code.R
#
# ==============================================================================

# Load required packages
library(tidyverse)
library(ggplot2)
library(scales)
library(patchwork)
library(knitr)
library(kableExtra)
library(xtable)

# --- Path configuration (relative to repo root) ---
REPO_ROOT <- normalizePath(file.path(dirname(sys.frame(1)$ofile), "..", "..", ".."), mustWork = FALSE)
# Fallback: if running interactively, assume working directory is repo root
if (!exists("REPO_ROOT") || is.na(REPO_ROOT)) REPO_ROOT <- getwd()

DATA_DIR      <- file.path(REPO_ROOT, "data", "simulation_results")
FIGURES_MAIN  <- file.path(REPO_ROOT, "figures", "main")
FIGURES_SUPP  <- file.path(REPO_ROOT, "figures", "supplement")
TABLES_MAIN   <- file.path(REPO_ROOT, "tables", "main")
TABLES_SUPP   <- file.path(REPO_ROOT, "tables", "supplement")

# Create output directories
for (d in c(FIGURES_MAIN, FIGURES_SUPP, TABLES_MAIN, TABLES_SUPP)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# Publication-quality theme
theme_pub <- function() {
  theme_minimal() +
    theme(
      text = element_text(family = "sans", size = 10),
      plot.title = element_text(face = "bold", size = 11, hjust = 0.5),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      strip.text = element_text(face = "bold", size = 10),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(fill = NA, color = "grey70", linewidth = 0.5)
    )
}

# Okabe-Ito colorblind-safe palette
OKABE_ITO <- c(
  "Vague" = "#E69F00",
  "DORO-Unif" = "#56B4E9",
  "TSMM" = "#009E73",
  "Dual-Anchor" = "#CC79A7",
  "chi2-DORO" = "#0072B2"
)

# Method order
MAIN_METHODS <- c("Vague", "DORO-Unif", "TSMM", "Dual-Anchor")
ALL_METHODS <- c("Vague", "DORO-Unif", "TSMM", "Dual-Anchor", "chi2-DORO")

################################################################################
# DATA LOADING AND PREPROCESSING
################################################################################

load_and_preprocess_data <- function(filepath = NULL) {
  if (is.null(filepath)) {
    filepath <- file.path(DATA_DIR, "raw_results_merged.csv")
  }
  df <- read_csv(filepath, show_col_types = FALSE)
  
  # Filter successful runs
  df <- df %>% filter(status == "ok")
  
  # Create method labels
  df <- df %>%
    mutate(
      method_label = case_when(
        method == "vague" ~ "Vague",
        method == "doro_uniform" ~ "DORO-Unif",
        method == "tsmm" ~ "TSMM",
        method == "dual" ~ "Dual-Anchor",
        method == "chi2_doro" ~ "chi2-DORO",
        TRUE ~ method
      ),
      method_label = factor(method_label, levels = ALL_METHODS),
      
      # Informativeness labels
      I_label = factor(
        case_when(
          I_target == 0.2 ~ "Low (I=0.2)",
          I_target == 0.5 ~ "Medium (I=0.5)",
          I_target == 0.8 ~ "High (I=0.8)"
        ),
        levels = c("Low (I=0.2)", "Medium (I=0.5)", "High (I=0.8)")
      ),
      
      # K labels
      K_label = factor(
        paste0("K* = ", K_true),
        levels = c("K* = 5", "K* = 10", "K* = 30")
      ),
      
      # Confidence labels
      conf_label = factor(
        case_when(
          confidence == "none" ~ "None",
          confidence == "low" ~ "Low",
          confidence == "medium" ~ "Medium",
          confidence == "high" ~ "High"
        ),
        levels = c("None", "Low", "Medium", "High")
      ),
      
      # Computed metrics
      bias_K = post_EK - K_true,
      abs_bias_K = abs(bias_K),
      rel_bias_K = (post_EK - K_true) / K_true * 100,
      mse_K = bias_K^2,
      ci_width = post_q975 - post_q025,
      covers_true = as.integer(post_q025 <= K_true & K_true <= post_q975)
    )
  
  return(df)
}

################################################################################
# FIGURE 1: Main 3x3 Grid - Posterior E[K_J] by Method
################################################################################

figure1_main_grid <- function(df, methods = MAIN_METHODS, save_prefix = "fig01") {
  # -> Paper Figure 3 (Main Text): Posterior E[K_J] 3x3 grid
  df_plot <- df %>%
    filter(confidence %in% c("none", "medium"),
           method_label %in% methods)
  
  p <- ggplot(df_plot, aes(x = method_label, y = post_EK, fill = method_label)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_hline(aes(yintercept = K_true), linetype = "dashed", color = "black", alpha = 0.7) +
    facet_grid(K_label ~ I_label, scales = "free_y") +
    scale_fill_manual(values = OKABE_ITO) +
    labs(
      title = "Posterior Expected Cluster Count by Method and Data Informativeness",
      x = "Method",
      y = "Posterior E[K|data]",
      fill = "Method"
    ) +
    theme_pub() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "bottom"
    )
  
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_simulation_grid.pdf")), p, 
         width = 10, height = 9, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_simulation_grid.png")), p, 
         width = 10, height = 9, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_simulation_grid.jpg")), p, 
         width = 10, height = 9, dpi = 300)
  
  message("✓ Saved: ", save_prefix, "_simulation_grid (pdf, png, jpg)")
  return(p)
}

################################################################################
# FIGURE 2: Extended Grid with chi2-DORO (Supplement)
################################################################################

figure2_extended_grid <- function(df, save_prefix = "fig02") {
  # -> Paper Figure F2 (Appendix F): Extended grid with all methods
  df_plot <- df %>%
    filter(confidence %in% c("none", "medium"))
  
  p <- ggplot(df_plot, aes(x = method_label, y = post_EK, fill = method_label)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_hline(aes(yintercept = K_true), linetype = "dashed", color = "black", alpha = 0.7) +
    facet_grid(K_label ~ I_label, scales = "free_y") +
    scale_fill_manual(values = OKABE_ITO) +
    labs(
      title = "Posterior Expected Cluster Count (All Methods Including χ²-DORO)",
      x = "Method",
      y = "Posterior E[K|data]",
      fill = "Method"
    ) +
    theme_pub() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      legend.position = "bottom"
    )
  
  ggsave(file.path(FIGURES_SUPP, paste0(save_prefix, "_extended_grid.pdf")), p, 
         width = 12, height = 9, dpi = 300)
  ggsave(file.path(FIGURES_SUPP, paste0(save_prefix, "_extended_grid.png")), p, 
         width = 12, height = 9, dpi = 300)
  ggsave(file.path(FIGURES_SUPP, paste0(save_prefix, "_extended_grid.jpg")), p, 
         width = 12, height = 9, dpi = 300)
  
  message("✓ Saved: ", save_prefix, "_extended_grid (pdf, png, jpg)")
  return(p)
}

################################################################################
# FIGURE 3: Collapse Probability P(K=1)
################################################################################

figure3_collapse_probability <- function(df, save_prefix = "fig03") {
  # -> Paper Figure 4 (Main Text): Probability of cluster collapse
  df_plot <- df %>%
    filter(confidence %in% c("none", "medium"),
           method_label %in% MAIN_METHODS)
  
  collapse_summary <- df_plot %>%
    group_by(K_true, I_target, method_label) %>%
    summarise(
      mean_collapse = mean(post_probK1),
      sd_collapse = sd(post_probK1),
      .groups = "drop"
    )
  
  p <- ggplot(collapse_summary, aes(x = factor(K_true), y = mean_collapse, 
                                     fill = method_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
             width = 0.7, alpha = 0.8) +
    geom_errorbar(aes(ymin = mean_collapse - sd_collapse, 
                      ymax = mean_collapse + sd_collapse),
                  position = position_dodge(width = 0.8), width = 0.2) +
    facet_wrap(~ factor(I_target, labels = c("I = 0.2", "I = 0.5", "I = 0.8")), 
               nrow = 1) +
    scale_fill_manual(values = OKABE_ITO) +
    labs(
      title = "Probability of Cluster Collapse (K = 1) Across Conditions",
      x = "True K*",
      y = "P(K = 1)",
      fill = "Method"
    ) +
    theme_pub() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_collapse_probability.pdf")), p, 
         width = 12, height = 4, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_collapse_probability.png")), p, 
         width = 12, height = 4, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_collapse_probability.jpg")), p, 
         width = 12, height = 4, dpi = 300)
  
  message("✓ Saved: ", save_prefix, "_collapse_probability (pdf, png, jpg)")
  return(p)
}

################################################################################
# FIGURE 4: Bias Distribution (Violin Plots)
################################################################################

figure4_bias_distribution <- function(df, save_prefix = "fig04") {
  # -> Paper Figure F1 (Appendix F): Bias distribution across conditions
  df_plot <- df %>%
    filter(confidence %in% c("none", "medium"),
           method_label %in% MAIN_METHODS)
  
  p <- ggplot(df_plot, aes(x = method_label, y = bias_K, fill = method_label)) +
    geom_violin(alpha = 0.7, draw_quantiles = c(0.5)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.7) +
    facet_grid(K_label ~ I_label) +
    scale_fill_manual(values = OKABE_ITO) +
    labs(
      title = "Distribution of Bias in Posterior Expected Cluster Count",
      x = "Method",
      y = "Bias (E[K] - K*)",
      fill = "Method"
    ) +
    theme_pub() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "bottom"
    )
  
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_bias_distribution.pdf")), p, 
         width = 11, height = 9, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_bias_distribution.png")), p, 
         width = 11, height = 9, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_bias_distribution.jpg")), p, 
         width = 11, height = 9, dpi = 300)
  
  message("✓ Saved: ", save_prefix, "_bias_distribution (pdf, png, jpg)")
  return(p)
}

################################################################################
# FIGURE 5: MSE Comparison
################################################################################

figure5_mse_comparison <- function(df, save_prefix = "fig05") {
  # -> Paper Figure F3 (Appendix F): Mean squared error comparison
  df_plot <- df %>%
    filter(confidence %in% c("none", "medium"),
           method_label %in% MAIN_METHODS)
  
  mse_summary <- df_plot %>%
    group_by(K_true, I_target, method_label) %>%
    summarise(mean_mse = mean(mse_K), .groups = "drop")
  
  p <- ggplot(mse_summary, aes(x = factor(K_true), y = mean_mse, fill = method_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
             width = 0.7, alpha = 0.8) +
    facet_wrap(~ factor(I_target, labels = c("I = 0.2", "I = 0.5", "I = 0.8")), 
               nrow = 1) +
    scale_y_log10() +
    scale_fill_manual(values = OKABE_ITO) +
    labs(
      title = "Mean Squared Error of Posterior E[K] Relative to True K*",
      x = "True K*",
      y = "MSE (log scale)",
      fill = "Method"
    ) +
    theme_pub() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_mse_comparison.pdf")), p, 
         width = 12, height = 4, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_mse_comparison.png")), p, 
         width = 12, height = 4, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_mse_comparison.jpg")), p, 
         width = 12, height = 4, dpi = 300)
  
  message("✓ Saved: ", save_prefix, "_mse_comparison (pdf, png, jpg)")
  return(p)
}

################################################################################
# FIGURE 6: Coverage Probability
################################################################################

figure6_coverage <- function(df, save_prefix = "fig06") {
  # -> Paper Figure F5 (Appendix F): 95% credible interval coverage
  df_plot <- df %>%
    filter(confidence %in% c("none", "medium"),
           method_label %in% MAIN_METHODS)
  
  coverage_summary <- df_plot %>%
    group_by(K_true, I_target, method_label) %>%
    summarise(coverage = mean(covers_true), .groups = "drop")
  
  p <- ggplot(coverage_summary, aes(x = factor(K_true), y = coverage, fill = method_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
             width = 0.7, alpha = 0.8) +
    geom_hline(yintercept = 0.95, linetype = "dashed", color = "black", alpha = 0.7) +
    facet_wrap(~ factor(I_target, labels = c("I = 0.2", "I = 0.5", "I = 0.8")), 
               nrow = 1) +
    scale_fill_manual(values = OKABE_ITO) +
    scale_y_continuous(limits = c(0, 1.05)) +
    labs(
      title = "95% Credible Interval Coverage of True K*",
      x = "True K*",
      y = "Coverage Probability",
      fill = "Method"
    ) +
    theme_pub() +
    theme(legend.position = "bottom")
  
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_coverage.pdf")), p, 
         width = 12, height = 4, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_coverage.png")), p, 
         width = 12, height = 4, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_coverage.jpg")), p, 
         width = 12, height = 4, dpi = 300)
  
  message("✓ Saved: ", save_prefix, "_coverage (pdf, png, jpg)")
  return(p)
}

################################################################################
# FIGURE 7: CI Width
################################################################################

figure7_ci_width <- function(df, save_prefix = "fig07") {
  # -> Paper Figure F6 (Appendix F): Credible interval width distribution
  df_plot <- df %>%
    filter(confidence %in% c("none", "medium"),
           method_label %in% MAIN_METHODS)
  
  p <- ggplot(df_plot, aes(x = method_label, y = ci_width, fill = method_label)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    facet_grid(K_label ~ I_label, scales = "free_y") +
    scale_fill_manual(values = OKABE_ITO) +
    labs(
      title = "Width of 95% Credible Interval for K",
      x = "Method",
      y = "95% CI Width",
      fill = "Method"
    ) +
    theme_pub() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      legend.position = "bottom"
    )
  
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_ci_width.pdf")), p, 
         width = 10, height = 9, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_ci_width.png")), p, 
         width = 10, height = 9, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_ci_width.jpg")), p, 
         width = 10, height = 9, dpi = 300)
  
  message("✓ Saved: ", save_prefix, "_ci_width (pdf, png, jpg)")
  return(p)
}

################################################################################
# FIGURE 10: Relative Bias Heatmap
################################################################################

figure10_heatmap <- function(df, save_prefix = "fig10") {
  # -> Paper Figure F4 (Appendix F): Relative bias heatmap
  df_plot <- df %>%
    filter(confidence %in% c("none", "medium"),
           method_label %in% MAIN_METHODS)
  
  rel_bias_summary <- df_plot %>%
    group_by(K_true, I_target, method_label) %>%
    summarise(mean_rel_bias = mean(rel_bias_K), .groups = "drop")
  
  p <- ggplot(rel_bias_summary, aes(x = factor(I_target), y = factor(K_true), 
                                     fill = mean_rel_bias)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.1f%%", mean_rel_bias)), size = 3) +
    facet_wrap(~ method_label, nrow = 1) +
    scale_fill_gradient2(low = "#009E73", mid = "#FFFFFF", high = "#D55E00",
                         midpoint = 0, limits = c(-80, 80)) +
    labs(
      title = "Relative Bias in Posterior E[K] by Method",
      x = "Informativeness (I)",
      y = "True K*",
      fill = "Relative\nBias (%)"
    ) +
    theme_pub() +
    theme(legend.position = "right")
  
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_heatmap_bias.pdf")), p, 
         width = 14, height = 4, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_heatmap_bias.png")), p, 
         width = 14, height = 4, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_heatmap_bias.jpg")), p, 
         width = 14, height = 4, dpi = 300)
  
  message("✓ Saved: ", save_prefix, "_heatmap_bias (pdf, png, jpg)")
  return(p)
}

################################################################################
# FIGURE 14: Summary Panel for Low Information
################################################################################

figure14_summary_panel <- function(df, save_prefix = "fig14") {
  # -> Paper Figure F7 (Appendix F): Summary panel for low-information setting
  df_plot <- df %>%
    filter(confidence %in% c("none", "medium"),
           method_label %in% MAIN_METHODS,
           I_target == 0.2)
  
  # Panel A: Mean E[K]
  mean_K <- df_plot %>%
    group_by(K_true, method_label) %>%
    summarise(mean_EK = mean(post_EK), .groups = "drop")
  
  p_a <- ggplot(mean_K, aes(x = factor(K_true), y = mean_EK, fill = method_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
             width = 0.7, alpha = 0.8) +
    geom_segment(data = data.frame(K_true = c(5, 10, 30)),
                 aes(x = as.numeric(factor(K_true)) - 0.4,
                     xend = as.numeric(factor(K_true)) + 0.4,
                     y = K_true, yend = K_true),
                 inherit.aes = FALSE, linetype = "dashed", alpha = 0.5) +
    scale_fill_manual(values = OKABE_ITO) +
    labs(title = "(A) Mean Posterior E[K|data]", x = "True K*", y = "Mean E[K]") +
    theme_pub() +
    theme(legend.position = "none")
  
  # Panel B: MSE
  mse <- df_plot %>%
    group_by(K_true, method_label) %>%
    summarise(mean_mse = mean(mse_K), .groups = "drop")
  
  p_b <- ggplot(mse, aes(x = factor(K_true), y = mean_mse, fill = method_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
             width = 0.7, alpha = 0.8) +
    scale_y_log10() +
    scale_fill_manual(values = OKABE_ITO) +
    labs(title = "(B) Mean Squared Error", x = "True K*", y = "MSE (log)") +
    theme_pub() +
    theme(legend.position = "none")
  
  # Panel C: Collapse P(K=1)
  collapse <- df_plot %>%
    group_by(K_true, method_label) %>%
    summarise(mean_collapse = mean(post_probK1), .groups = "drop")
  
  p_c <- ggplot(collapse, aes(x = factor(K_true), y = mean_collapse, fill = method_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
             width = 0.7, alpha = 0.8) +
    scale_fill_manual(values = OKABE_ITO) +
    labs(title = "(C) Cluster Collapse P(K=1)", x = "True K*", y = "P(K=1)") +
    theme_pub() +
    theme(legend.position = "none")
  
  # Panel D: Coverage
  cov <- df_plot %>%
    group_by(K_true, method_label) %>%
    summarise(coverage = mean(covers_true), .groups = "drop")
  
  p_d <- ggplot(cov, aes(x = factor(K_true), y = coverage, fill = method_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
             width = 0.7, alpha = 0.8) +
    geom_hline(yintercept = 0.95, linetype = "dashed", alpha = 0.7) +
    scale_y_continuous(limits = c(0, 1.05)) +
    scale_fill_manual(values = OKABE_ITO) +
    labs(title = "(D) 95% CI Coverage", x = "True K*", y = "Coverage") +
    theme_pub() +
    theme(legend.position = "bottom")
  
  # Combine
  p <- (p_a | p_b) / (p_c | p_d) +
    plot_annotation(
      title = "Summary of Method Performance in Low-Information Setting (I = 0.2)",
      theme = theme(plot.title = element_text(face = "bold", hjust = 0.5))
    )
  
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_summary_low_info.pdf")), p, 
         width = 10, height = 8, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_summary_low_info.png")), p, 
         width = 10, height = 8, dpi = 300)
  ggsave(file.path(FIGURES_MAIN, paste0(save_prefix, "_summary_low_info.jpg")), p, 
         width = 10, height = 8, dpi = 300)
  
  message("✓ Saved: ", save_prefix, "_summary_low_info (pdf, png, jpg)")
  return(p)
}

################################################################################
# FIGURE 8: Confidence Level Sensitivity
################################################################################

figure8_confidence_sensitivity <- function(df, save_prefix = "fig08") {
  # -> Paper Figure F9 (Appendix F): Effect of confidence level on TSMM/Dual-Anchor
  df_plot <- df %>%
    filter(method %in% c("tsmm", "dual"))
  
  conf_colors <- c("low" = "#D55E00", "medium" = "#009E73", "high" = "#0072B2")
  
  p <- ggplot(df_plot, aes(x = interaction(method_label, confidence), 
                            y = post_EK, fill = confidence)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_hline(aes(yintercept = K_true), linetype = "dashed", alpha = 0.7) +
    facet_grid(K_label ~ I_label, scales = "free_y") +
    scale_fill_manual(values = conf_colors, 
                      labels = c("Low", "Medium", "High")) +
    labs(
      title = "Effect of Confidence Level on TSMM and Dual-Anchor Methods",
      x = "Method.Confidence",
      y = "Posterior E[K|data]",
      fill = "Confidence"
    ) +
    theme_pub() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
      legend.position = "bottom"
    )
  
  ggsave(file.path(FIGURES_SUPP, paste0(save_prefix, "_confidence_sensitivity.pdf")), p, 
         width = 11, height = 9, dpi = 300)
  ggsave(file.path(FIGURES_SUPP, paste0(save_prefix, "_confidence_sensitivity.png")), p, 
         width = 11, height = 9, dpi = 300)
  ggsave(file.path(FIGURES_SUPP, paste0(save_prefix, "_confidence_sensitivity.jpg")), p, 
         width = 11, height = 9, dpi = 300)
  
  message("✓ Saved: ", save_prefix, "_confidence_sensitivity (pdf, png, jpg)")
  return(p)
}

################################################################################
# TABLE GENERATION FUNCTIONS
################################################################################

# Table 1: Main Results
generate_table1 <- function(df) {
  # -> Paper Table 1 (Main Text): Summary of E[K] by method and condition
  df_sub <- df %>%
    filter(confidence %in% c("none", "medium"),
           method_label %in% MAIN_METHODS)
  
  summary <- df_sub %>%
    group_by(K_true, I_target, method_label) %>%
    summarise(
      mean_EK = mean(post_EK),
      sd_EK = sd(post_EK),
      .groups = "drop"
    ) %>%
    mutate(
      value = sprintf("%.2f (%.2f)", mean_EK, sd_EK)
    )
  
  table_wide <- summary %>%
    select(K_true, I_target, method_label, value) %>%
    pivot_wider(names_from = method_label, values_from = value)
  
  # Save in multiple formats
  write_csv(table_wide, file.path(TABLES_MAIN, "table01_main_results.csv"))
  
  md_table <- kable(table_wide, format = "markdown")
  writeLines(as.character(md_table), file.path(TABLES_MAIN, "table01_main_results.md"))
  
  latex_table <- xtable(table_wide)
  print(latex_table, file = file.path(TABLES_MAIN, "table01_main_results.tex"), 
        include.rownames = FALSE)
  
  message("✓ Saved: table01_main_results (csv, md, tex)")
  return(table_wide)
}

# Table 6: Low Information Summary
generate_table6 <- function(df) {
  # -> Paper Table 6 (Main Text): Summary metrics in low-information setting (I=0.2)
  df_sub <- df %>%
    filter(confidence %in% c("none", "medium"),
           method_label %in% MAIN_METHODS,
           I_target == 0.2)
  
  summary <- df_sub %>%
    group_by(K_true, method_label) %>%
    summarise(
      EK_mean = mean(post_EK),
      EK_sd = sd(post_EK),
      Bias = mean(bias_K),
      RMSE = sqrt(mean(mse_K)),
      PK1 = mean(post_probK1),
      Coverage = mean(covers_true),
      CI_Width = mean(ci_width),
      .groups = "drop"
    ) %>%
    mutate(
      `E[K]` = sprintf("%.2f (%.2f)", EK_mean, EK_sd),
      Bias = sprintf("%+.2f", Bias),
      RMSE = sprintf("%.2f", RMSE),
      `P(K=1)` = sprintf("%.3f", PK1),
      Coverage = sprintf("%.3f", Coverage),
      `CI Width` = sprintf("%.2f", CI_Width)
    ) %>%
    select(K_true, method_label, `E[K]`, Bias, RMSE, `P(K=1)`, Coverage, `CI Width`)
  
  write_csv(summary, file.path(TABLES_MAIN, "table06_low_info_summary.csv"))
  
  md_table <- kable(summary, format = "markdown")
  writeLines(as.character(md_table), file.path(TABLES_MAIN, "table06_low_info_summary.md"))
  
  latex_table <- xtable(summary)
  print(latex_table, file = file.path(TABLES_MAIN, "table06_low_info_summary.tex"), 
        include.rownames = FALSE)
  
  message("✓ Saved: table06_low_info_summary (csv, md, tex)")
  return(summary)
}

################################################################################
# MAIN EXECUTION
################################################################################

main <- function(data_path = NULL) {
  if (is.null(data_path)) {
    data_path <- file.path(DATA_DIR, "raw_results_merged.csv")
  }
  
  message("\n", paste(rep("=", 60), collapse = ""))
  message("GENERATING R FIGURES AND TABLES")
  message(paste(rep("=", 60), collapse = ""), "\n")
  
  # Load data
  df <- load_and_preprocess_data(data_path)
  message("✓ Data loaded: ", nrow(df), " rows\n")
  
  # Generate main figures
  message("--- Main Text Figures ---")
  figure1_main_grid(df)
  figure3_collapse_probability(df)
  figure4_bias_distribution(df)
  figure5_mse_comparison(df)
  figure6_coverage(df)
  figure7_ci_width(df)
  figure10_heatmap(df)
  figure14_summary_panel(df)
  
  # Generate supplement figures
  message("\n--- Supplement Figures ---")
  figure2_extended_grid(df)
  figure8_confidence_sensitivity(df)
  
  # Generate tables
  message("\n--- Main Text Tables ---")
  generate_table1(df)
  generate_table6(df)
  
  message("\n", paste(rep("=", 60), collapse = ""))
  message("ALL R OUTPUTS GENERATED SUCCESSFULLY!")
  message(paste(rep("=", 60), collapse = ""))
}

# Run if executed as script
if (!interactive()) {
  main(file.path(DATA_DIR, "raw_results_merged.csv"))
} else {
  message("R code loaded. Run main() to generate outputs.")
}
