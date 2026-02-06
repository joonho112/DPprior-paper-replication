#' DPprior Replication Package — Appendix F Supplementary Figures
#'
#' This script generates 5 supplementary figures for Appendix F:
#'   - Figure F02: Collapse probability (detailed version)
#'   - Figure F04: Relative MSE improvement heatmap
#'   - Figure F08: χ²-DORO comparison
#'   - Figure F12: Mode recovery (exact and near)
#'   - Figure F14: Bias-variance decomposition
#'
#' Paper Mapping:
#'   These figures complement the main paper analysis by providing:
#'   - Detailed breakdowns of posterior behavior across all priors
#'   - Comparative analysis of method performance
#'   - Diagnostic metrics (mode recovery, bias-variance tradeoffs)
#'   - Appendix-level baseline comparisons (χ²-DORO)
#'
#' Dependencies: ggplot2, dplyr, tidyverse, scales, patchwork
#'
#' Input:  ../../data/simulation_results/preprocessed_results.csv
#' Output: ../../figures/supplement/
#'

# ==============================================================================
# SETUP: LIBRARIES AND CONFIGURATION
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyverse)
library(scales)
library(patchwork)

# Set working directory relative to script location
# Works in both Rscript (command line) and RStudio
script_dir <- tryCatch(
  dirname(normalizePath(sys.frame(1)$ofile)),
  error = function(e) {
    if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
      dirname(rstudioapi::getSourceEditorContext()$path)
    } else {
      getwd()
    }
  }
)
base_dir <- file.path(script_dir, "..", "..")
data_dir <- file.path(base_dir, "data", "simulation_results")
output_dir <- file.path(base_dir, "figures", "supplement")

# Create output directory if it doesn't exist
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

# Define color palette (Okabe-Ito colorblind-safe)
OKABE_ITO <- c(
  "Vague" = "#E69F00",
  "DORO-Unif" = "#56B4E9",
  "TSMM" = "#009E73",
  "Dual-Anchor" = "#CC79A7",
  "χ²-DORO" = "#0072B2"
)

MAIN_METHODS <- c("Vague", "DORO-Unif", "TSMM", "Dual-Anchor")
ALL_METHODS <- c("Vague", "DORO-Unif", "TSMM", "Dual-Anchor", "χ²-DORO")

# Publication-quality theme
theme_pub <- function() {
  theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "gray95"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray30", fill = NA, linewidth = 0.5),
      axis.text = element_text(size = 10, color = "gray20"),
      axis.title = element_text(size = 11, color = "gray10", face = "bold"),
      plot.title = element_text(size = 12, color = "gray10", face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0.5, margin = margin(b = 5)),
      legend.position = "bottom",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 10),
      legend.box.spacing = unit(0.3, "lines"),
      strip.text = element_text(size = 10, face = "bold", color = "gray10"),
      strip.background = element_rect(color = "gray80", fill = "gray95"),
      plot.margin = margin(t = 5, r = 10, b = 5, l = 10, unit = "pt")
    )
}

# ==============================================================================
# DATA LOADING AND PREPROCESSING
# ==============================================================================

# Load preprocessed simulation results
results <- read.csv(file.path(data_dir, "preprocessed_results.csv"), stringsAsFactors = FALSE)

# Filter to confidence levels used in main analysis
results_filt <- results %>%
  filter(confidence %in% c("none", "medium"))

# Ensure method_label is factor with correct order
results_filt <- results_filt %>%
  mutate(
    method_label = factor(method_label, levels = ALL_METHODS),
    K_true = factor(K_true, levels = c(5, 10, 30)),
    I_target = as.numeric(I_target)
  )

cat("Data loaded and filtered.\n")
cat("Dimensions:", dim(results_filt), "\n")
cat("Methods:", unique(results_filt$method_label), "\n")
cat("K values:", unique(results_filt$K_true), "\n")
cat("I values:", unique(results_filt$I_target), "\n")

# ==============================================================================
# FIGURE F02: COLLAPSE PROBABILITY (DETAILED VERSION)
# ==============================================================================

cat("\n--- Generating Figure F02: Collapse Probability ---\n")

# Aggregate: mean and SD of post_probK1 by K_true, I_target, method_label
collapse_summary <- results_filt %>%
  group_by(K_true, I_target, method_label) %>%
  summarise(
    mean_collapse = mean(post_probK1, na.rm = TRUE),
    sd_collapse = sd(post_probK1, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  mutate(
    I_label = paste0("I* = ", I_target)
  )

# Filter to MAIN_METHODS only
collapse_main <- collapse_summary %>%
  filter(method_label %in% MAIN_METHODS)

# Create figure with 1×3 panels (columns = I values)
fig_f02 <- ggplot(collapse_main, aes(x = K_true, y = mean_collapse, fill = method_label, group = method_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_collapse - sd_collapse, ymax = mean_collapse + sd_collapse),
                position = position_dodge(width = 0.8), width = 0.25, size = 0.4) +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  facet_wrap(~ I_label, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = OKABE_ITO[MAIN_METHODS], name = "Method") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Posterior Collapse Probability",
    subtitle = "P(K_J = 1) by Method and Information Level",
    x = "True K*",
    y = "Collapse Probability"
  ) +
  theme_pub()

# Save Figure F02
ggsave(file.path(output_dir, "Figure_F02.pdf"), fig_f02, width = 12, height = 5, dpi = 300)
ggsave(file.path(output_dir, "Figure_F02.png"), fig_f02, width = 12, height = 5, dpi = 300)
cat("Figure F02 saved.\n")

# ==============================================================================
# FIGURE F04: RELATIVE MSE IMPROVEMENT HEATMAP
# ==============================================================================

cat("\n--- Generating Figure F04: Relative MSE Improvement Heatmap ---\n")

# Calculate MSE by K_true, I_target, method_label
mse_summary <- results_filt %>%
  group_by(K_true, I_target, method_label) %>%
  summarise(
    mean_mse = mean(mse_K, na.rm = TRUE),
    .groups = "drop"
  )

# Get Vague baseline MSE
vague_mse <- mse_summary %>%
  filter(method_label == "Vague") %>%
  select(K_true, I_target, vague_mse = mean_mse)

# Calculate relative improvement for MAIN_METHODS (excluding Vague)
improvement <- mse_summary %>%
  filter(method_label %in% MAIN_METHODS[-1]) %>%  # Exclude "Vague"
  left_join(vague_mse, by = c("K_true", "I_target")) %>%
  mutate(
    improvement_pct = ((vague_mse - mean_mse) / vague_mse) * 100
  ) %>%
  select(K_true, I_target, method_label, improvement_pct)

# Create heatmaps: one panel per method
heatmap_list <- list()

for (method in MAIN_METHODS[-1]) {  # Exclude Vague
  data_method <- improvement %>%
    filter(method_label == method) %>%
    mutate(
      I_label = paste0("I = ", I_target)
    )
  
  # Create heatmap
  hm <- ggplot(data_method, aes(x = I_label, y = K_true, fill = improvement_pct)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = sprintf("%.1f%%", improvement_pct)), 
              size = 4, color = "gray10", fontface = "bold") +
    scale_fill_gradient(low = "#F0F0F0", high = "#2D7A2D", name = "Improvement (%)") +
    labs(
      title = method,
      x = "Information Level",
      y = "True K*"
    ) +
    theme_pub() +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "bottom"
    )
  
  heatmap_list[[method]] <- hm
}

# Combine heatmaps
fig_f04 <- wrap_plots(heatmap_list, nrow = 1) +
  plot_annotation(
    title = "Relative MSE Improvement over Vague Prior (%)",
    subtitle = "(MSE_Vague - MSE_Method) / MSE_Vague × 100%",
    theme = theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5)
    )
  )

# Save Figure F04
ggsave(file.path(output_dir, "Figure_F04.pdf"), fig_f04, width = 14, height = 5, dpi = 300)
ggsave(file.path(output_dir, "Figure_F04.png"), fig_f04, width = 14, height = 5, dpi = 300)
cat("Figure F04 saved.\n")

# ==============================================================================
# FIGURE F08: χ²-DORO COMPARISON
# ==============================================================================

cat("\n--- Generating Figure F08: χ²-DORO Comparison ---\n")

# Filter to I = 0.2 only and ALL_METHODS
results_f08 <- results_filt %>%
  filter(I_target == 0.2, method_label %in% ALL_METHODS)

# Prepare data for RMSE (log scale)
rmse_data <- results_f08 %>%
  mutate(rmse_K = sqrt(mse_K)) %>%
  group_by(K_true, method_label) %>%
  summarise(
    mean_rmse = mean(rmse_K, na.rm = TRUE),
    sd_rmse = sd(rmse_K, na.rm = TRUE),
    .groups = "drop"
  )

# Create left panel: RMSE with log scale
fig_f08_left <- ggplot(rmse_data, aes(x = K_true, y = mean_rmse, fill = method_label, group = method_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_rmse - sd_rmse, ymax = mean_rmse + sd_rmse),
                position = position_dodge(width = 0.8), width = 0.25, size = 0.4) +
  scale_fill_manual(values = OKABE_ITO[ALL_METHODS], name = "Method") +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(
    title = "RMSE (log scale)",
    x = "True K*",
    y = "RMSE"
  ) +
  theme_pub() +
  theme(legend.position = "none")

# Prepare data for collapse probability
collapse_data <- results_f08 %>%
  group_by(K_true, method_label) %>%
  summarise(
    mean_collapse = mean(post_probK1, na.rm = TRUE),
    sd_collapse = sd(post_probK1, na.rm = TRUE),
    .groups = "drop"
  )

# Create right panel: Collapse probability
fig_f08_right <- ggplot(collapse_data, aes(x = K_true, y = mean_collapse, fill = method_label, group = method_label)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = mean_collapse - sd_collapse, ymax = mean_collapse + sd_collapse),
                position = position_dodge(width = 0.8), width = 0.25, size = 0.4) +
  geom_hline(yintercept = 0.50, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  scale_fill_manual(values = OKABE_ITO[ALL_METHODS], name = "Method") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(
    title = "Collapse Probability",
    x = "True K*",
    y = "P(K_J = 1)"
  ) +
  theme_pub() +
  theme(legend.position = "bottom")

# Combine into 2-panel figure
fig_f08 <- fig_f08_left + fig_f08_right +
  plot_layout(ncol = 2, widths = c(1, 1.2)) +
  plot_annotation(
    title = "χ²-DORO Comparison (I* = 0.2)",
    subtitle = "Method comparison including χ²-DORO baseline",
    theme = theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5)
    )
  )

# Save Figure F08
ggsave(file.path(output_dir, "Figure_F08.pdf"), fig_f08, width = 12, height = 5, dpi = 300)
ggsave(file.path(output_dir, "Figure_F08.png"), fig_f08, width = 12, height = 5, dpi = 300)
cat("Figure F08 saved.\n")

# ==============================================================================
# FIGURE F12: MODE RECOVERY (EXACT AND NEAR)
# ==============================================================================

cat("\n--- Generating Figure F12: Mode Recovery ---\n")

# Prepare mode recovery data for MAIN_METHODS
mode_data <- results_filt %>%
  filter(method_label %in% MAIN_METHODS) %>%
  group_by(K_true, I_target, method_label) %>%
  summarise(
    exact_recovery = mean(post_modeK == K_true, na.rm = TRUE),
    near_recovery = mean(abs(post_modeK - K_true) <= 1, na.rm = TRUE),
    sd_exact = sd(post_modeK == K_true, na.rm = TRUE),
    sd_near = sd(abs(post_modeK - K_true) <= 1, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    I_label = paste0("I = ", I_target)
  )

# Create exact recovery panels (top row)
exact_plots <- list()
for (i_val in unique(mode_data$I_target)) {
  data_i <- mode_data %>% filter(I_target == i_val)
  
  p <- ggplot(data_i, aes(x = K_true, y = exact_recovery, fill = method_label, group = method_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = exact_recovery - sd_exact, ymax = exact_recovery + sd_exact),
                  position = position_dodge(width = 0.8), width = 0.25, size = 0.4) +
    scale_fill_manual(values = OKABE_ITO[MAIN_METHODS], name = "Method") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(
      title = paste0("I = ", i_val),
      x = "True K*",
      y = "Exact Recovery"
    ) +
    theme_pub() +
    theme(legend.position = "none")
  
  exact_plots[[paste0("I_", i_val)]] <- p
}

# Create near recovery panels (bottom row)
near_plots <- list()
for (i_val in unique(mode_data$I_target)) {
  data_i <- mode_data %>% filter(I_target == i_val)
  
  p <- ggplot(data_i, aes(x = K_true, y = near_recovery, fill = method_label, group = method_label)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_errorbar(aes(ymin = near_recovery - sd_near, ymax = near_recovery + sd_near),
                  position = position_dodge(width = 0.8), width = 0.25, size = 0.4) +
    scale_fill_manual(values = OKABE_ITO[MAIN_METHODS], name = "Method") +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(
      title = paste0("I = ", i_val),
      x = "True K*",
      y = "Near Recovery (|Mode - K*| ≤ 1)"
    ) +
    theme_pub() +
    theme(legend.position = ifelse(i_val == unique(mode_data$I_target)[3], "bottom", "none"))
  
  near_plots[[paste0("I_", i_val)]] <- p
}

# Combine into 2×3 grid
fig_f12 <- (
  exact_plots[[1]] + exact_plots[[2]] + exact_plots[[3]] +
  near_plots[[1]] + near_plots[[2]] + near_plots[[3]]
) +
  plot_layout(nrow = 2, ncol = 3) +
  plot_annotation(
    title = "Mode-Based Recovery Performance",
    subtitle = "Top: Exact recovery P(mode(K) = K*). Bottom: Near recovery P(|mode(K) - K*| ≤ 1)",
    theme = theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5)
    )
  )

# Save Figure F12
ggsave(file.path(output_dir, "Figure_F12.pdf"), fig_f12, width = 14, height = 8, dpi = 300)
ggsave(file.path(output_dir, "Figure_F12.png"), fig_f12, width = 14, height = 8, dpi = 300)
cat("Figure F12 saved.\n")

# ==============================================================================
# FIGURE F14: BIAS-VARIANCE DECOMPOSITION
# ==============================================================================

cat("\n--- Generating Figure F14: Bias-Variance Decomposition ---\n")

# Filter to I = 0.2 and MAIN_METHODS
results_f14 <- results_filt %>%
  filter(I_target == 0.2, method_label %in% MAIN_METHODS)

# Calculate bias² and variance
# MSE = Bias² + Variance
bv_data <- results_f14 %>%
  group_by(K_true, method_label) %>%
  summarise(
    mean_bias = mean(bias_K, na.rm = TRUE),
    mean_var = mean((post_sdK)^2, na.rm = TRUE),
    mean_mse = mean(mse_K, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    bias_sq = mean_bias^2,
    variance = mean_var,
    # Ensure bias² + variance approximately equals MSE (may have small discrepancies due to averaging)
    mse_check = bias_sq + variance
  )

# For K*=30, note that we'll use log scale in the plot
# Prepare data in long format for stacked bars
bv_long <- bv_data %>%
  select(K_true, method_label, bias_sq, variance) %>%
  pivot_longer(
    cols = c(bias_sq, variance),
    names_to = "component",
    values_to = "value"
  ) %>%
  mutate(
    component = factor(component, levels = c("bias_sq", "variance"), 
                      labels = c("Bias²", "Variance"))
  )

# Separate plots for regular and log scales
# For K*=5, 10: regular scale
bv_regular <- bv_long %>%
  filter(K_true %in% c(5, 10))

fig_f14_regular <- ggplot(bv_regular, aes(x = K_true, y = value, fill = component)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6, alpha = 0.85) +
  scale_fill_manual(values = c("Bias²" = "#D95F02", "Variance" = "#7570B3"), name = "MSE Component") +
  facet_wrap(~ method_label, nrow = 1) +
  labs(
    title = "",
    x = "True K*",
    y = "Value"
  ) +
  theme_pub() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 9)
  )

# For K*=30: log scale
bv_log <- bv_long %>%
  filter(K_true == 30)

fig_f14_log <- ggplot(bv_log, aes(x = K_true, y = value, fill = component)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6, alpha = 0.85) +
  scale_fill_manual(values = c("Bias²" = "#D95F02", "Variance" = "#7570B3"), name = "MSE Component") +
  scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  facet_wrap(~ method_label, nrow = 1) +
  labs(
    title = "",
    x = "True K*",
    y = "Value (log scale)"
  ) +
  theme_pub() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 9)
  )

# For now, create a single figure showing all K values with appropriate scales
# Combine K=5,10 (linear) and K=30 (log) in one visualization
# We'll create faceted plot with free y-scales

fig_f14 <- ggplot(bv_long, aes(x = method_label, y = value, fill = component)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7, alpha = 0.85) +
  facet_wrap(~ K_true, scales = "free_y", nrow = 1,
             labeller = labeller(K_true = c("5" = "K* = 5", "10" = "K* = 10", "30" = "K* = 30 (log scale)"))) +
  scale_fill_manual(values = c("Bias²" = "#D95F02", "Variance" = "#7570B3"), name = "Component") +
  labs(
    title = "Bias–Variance Decomposition of MSE",
    subtitle = "I* = 0.2. Bias² (solid) and Variance contributions. Right panel on log scale.",
    x = "Prior Method",
    y = "MSE Component"
  ) +
  theme_pub() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    strip.text = element_text(size = 10)
  )

# For K*=30, apply log scale to y-axis specifically
# Create a custom version
bv_long_k30 <- bv_long %>% filter(K_true == 30)
bv_long_k5_10 <- bv_long %>% filter(K_true != 30)

# Actually, let's create it as originally specified: one combined figure for I=0.2
# showing K=5, 10, 30 with K=30 having log scale

# Create separate plots and combine
plots_list <- list()

for (k_val in c(5, 10, 30)) {
  data_k <- bv_long %>% filter(K_true == k_val)
  
  p <- ggplot(data_k, aes(x = method_label, y = value, fill = component)) +
    geom_bar(stat = "identity", position = "stack", width = 0.6, alpha = 0.85) +
    scale_fill_manual(values = c("Bias²" = "#D95F02", "Variance" = "#7570B3"), name = "Component") +
    labs(
      title = paste0("K* = ", k_val),
      x = "Prior Method",
      y = "Value"
    ) +
    theme_pub() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      legend.position = ifelse(k_val == 30, "bottom", "none"),
      plot.title = element_text(size = 11)
    )
  
  # Apply log scale for K*=30
  if (k_val == 30) {
    p <- p + scale_y_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                           labels = scales::trans_format("log10", scales::math_format(10^.x)))
  }
  
  plots_list[[paste0("K_", k_val)]] <- p
}

fig_f14 <- wrap_plots(plots_list, nrow = 1) +
  plot_annotation(
    title = "Bias–Variance Decomposition of MSE (I* = 0.2)",
    subtitle = "Solid bars = Bias². Stacked portion = Variance. Right panel (K* = 30) on log scale.",
    theme = theme(
      plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, color = "gray40", hjust = 0.5)
    )
  )

# Save Figure F14
ggsave(file.path(output_dir, "Figure_F14.pdf"), fig_f14, width = 13, height = 5, dpi = 300)
ggsave(file.path(output_dir, "Figure_F14.png"), fig_f14, width = 13, height = 5, dpi = 300)
cat("Figure F14 saved.\n")

# ==============================================================================
# COMPLETION
# ==============================================================================

cat("\n========================================\n")
cat("All Appendix F supplementary figures generated successfully!\n")
cat("========================================\n")
cat("Output directory:", output_dir, "\n")
cat("Files created:\n")
cat("  - Figure_F02.pdf, Figure_F02.png\n")
cat("  - Figure_F04.pdf, Figure_F04.png\n")
cat("  - Figure_F08.pdf, Figure_F08.png\n")
cat("  - Figure_F12.pdf, Figure_F12.png\n")
cat("  - Figure_F14.pdf, Figure_F14.png\n")

