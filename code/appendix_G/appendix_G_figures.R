#!/usr/bin/env Rscript
# ==============================================================================
# appendix_G_figures.R — Appendix G: Software Implementation Figures
# ==============================================================================
#
# Part of:  DPprior Replication Package
# Paper:    "Design-Conditional Prior Elicitation for Dirichlet Process Mixtures"
# Author:   JoonHo Lee
# Date:     January 2026
#
# Generates:
#   → Figure G01 (Appendix G): TSMM Diagnostic Dashboard
#       J=50, μ_K=5, medium confidence — 4-panel dashboard from plot(fit)
#   → Figure G02 (Appendix G): Weight Distribution Comparison
#       w₁ density before (K-only) vs after (Dual-Anchor) refinement
#   → Figure G03 (Appendix G): K_J Sensitivity to Confidence Levels
#       PMF of K_J under high/medium/low confidence specifications
#   → Figure G04 (Appendix G): Dual-Anchor Trade-off Curve
#       E[K_J] vs P(w₁ > 0.5) across λ values (2-panel)
#   → Table G07 (Appendix G): Sensitivity Analysis Summary
#       Markdown table of diagnostics across confidence levels
#
# Outputs:
#   ../../figures/supplement/Figure_G01.{pdf,png}
#   ../../figures/supplement/Figure_G02.{pdf,png}
#   ../../figures/supplement/Figure_G03.{pdf,png}
#   ../../figures/supplement/Figure_G04.{pdf,png}
#   results/Table_G07_sensitivity_summary.md
#
# Dependencies:
#   CRAN:   ggplot2, dplyr, tidyr, patchwork
#   GitHub: DPprior (remotes::install_github("joonho112/DPprior"))
#
# Usage:
#   cd code/appendix_G
#   Rscript appendix_G_figures.R
#
# ==============================================================================


# =============================================================================
# 0) Setup & Configuration
# =============================================================================

set.seed(2026)

# --- Output paths (relative to this script's directory: code/appendix_G/) ---
DIR_FIG_SUPP <- file.path("..", "..", "figures", "supplement")
DIR_RESULTS  <- file.path("results")

# Create output directories
for (d in c(DIR_FIG_SUPP, DIR_RESULTS)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# --- Package checks ---
required_pkgs <- c("DPprior", "ggplot2", "dplyr", "tidyr", "patchwork")
missing <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                 FUN.VALUE = logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  stop(
    sprintf("Missing required packages: %s.\n", paste(missing, collapse = ", ")),
    "Install DPprior via: remotes::install_github('joonho112/DPprior')\n",
    "Install CRAN packages via: install.packages(c('ggplot2','dplyr','tidyr','patchwork'))",
    call. = FALSE
  )
}

library(DPprior)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# --- Color palettes (consistent with paper OSM) ---
palette_methods <- c(
  "High confidence"   = "#4DAF4A",   # Green
  "Medium confidence" = "#377EB8",   # Blue
  "Low confidence"    = "#E41A1C"    # Red
)

palette_dual <- c(
  "Before (K-only)"    = "#377EB8",  # Blue
  "After (Dual-Anchor)" = "#E41A1C"  # Red
)


# =============================================================================
# Helper Functions (self-contained, replaces jleetools::myggsave)
# =============================================================================

#' Save a ggplot or gtable object in PDF and PNG formats
#'
#' Handles both ggplot objects and gtable objects (returned by DPprior::plot()).
#' Replaces the jleetools::myggsave() dependency with standard ggsave().
#'
#' @param p     A ggplot or gtable object
#' @param stub  Output filename stub (without extension), e.g., "Figure_G01"
#' @param w     Width in inches
#' @param h     Height in inches
#' @param dpi   Resolution for PNG output (default: 300)
save_figure <- function(p, stub, w = 10, h = 8, dpi = 300) {
  pdf_path <- paste0(stub, ".pdf")
  png_path <- paste0(stub, ".png")

  ggsave(filename = pdf_path, plot = p, width = w, height = h,
         device = "pdf", useDingbats = FALSE)
  ggsave(filename = png_path, plot = p, width = w, height = h,
         dpi = dpi, device = "png", bg = "white")

  cat("  Saved:", basename(stub), "(pdf + png)\n")
}


#' Compute marginal density of w₁ via Monte Carlo integration
#'
#' The largest stick-breaking weight w₁ | α ~ Beta(1, α), and α ~ Gamma(a, b).
#' This function estimates the marginal density f(w) = E_α[Beta(w; 1, α)]
#' by averaging over n_mc Monte Carlo draws of α.
#'
#' @param w_grid  Numeric vector of w₁ values at which to evaluate density
#' @param a       Shape parameter of Gamma(a, b) hyperprior
#' @param b       Rate parameter of Gamma(a, b) hyperprior
#' @param n_mc    Number of Monte Carlo draws (default: 50000)
#' @return Numeric vector of estimated densities at each point in w_grid
compute_w1_density <- function(w_grid, a, b, n_mc = 50000) {
  alpha_samples <- rgamma(n_mc, shape = a, rate = b)
  sapply(w_grid, function(w) {
    mean(dbeta(w, shape1 = 1, shape2 = alpha_samples))
  })
}


# =============================================================================
# Section 1: Figure G01 — TSMM Diagnostic Dashboard
# =============================================================================
# Generated via DPprior::plot(fit), which returns a gtable object.
# Shows: (A) marginal PMF of K_J, (B) prior density of α,
#        (C) marginal density of w₁, (D) calibration summary.

cat("Generating Figure G01: TSMM Diagnostic Dashboard...\n")

# Worked example: J=50, μ_K=5, medium confidence
J    <- 50
mu_K <- 5
fit  <- DPprior_fit(J = J, mu_K = mu_K, confidence = "medium")

# Generate the 4-panel dashboard (returns a gtable, not ggplot)
p_g1 <- plot(fit)

save_figure(p_g1, file.path(DIR_FIG_SUPP, "Figure_G01"), w = 10, h = 8)
cat("\n")


# =============================================================================
# Section 2: Figure G02 — Dual-Anchor Weight Comparison
# =============================================================================
# Compares the marginal density of w₁ before and after Dual-Anchor
# refinement. Uses a scenario where the K-only solution has high
# dominance risk (J=50, μ_K=3, low confidence).

cat("Generating Figure G02: Weight Distribution Comparison...\n")

# Create a high-risk scenario
fit_high_risk <- DPprior_fit(J = 50, mu_K = 3, confidence = "low")

# Diagnose dominance risk before refinement
diag_high_risk <- DPprior_diagnostics(fit_high_risk)
P_dom05_before <- prob_w1_exceeds(0.5, fit_high_risk$a, fit_high_risk$b)
cat("  Before Dual-Anchor: P(w1 > 0.5) =", round(P_dom05_before, 3), "\n")

# Apply Dual-Anchor refinement
fit_dual <- DPprior_dual(
  fit = fit_high_risk,
  w1_target = list(prob = list(threshold = 0.5, value = 0.25)),
  lambda = 0.7,                # Trade-off: 70% weight on K-fit
  loss_type = "adaptive",
  verbose = FALSE
)

P_dom05_after <- prob_w1_exceeds(0.5, fit_dual$a, fit_dual$b)
cat("  After Dual-Anchor:  P(w1 > 0.5) =", round(P_dom05_after, 3), "\n")

# Compute marginal densities of w₁ (Monte Carlo, n=50000)
w_grid <- seq(0.01, 0.99, length.out = 200)
cat("  Computing marginal densities of w1 (Monte Carlo)...\n")

density_before <- compute_w1_density(w_grid, fit_high_risk$a, fit_high_risk$b)
density_after  <- compute_w1_density(w_grid, fit_dual$a, fit_dual$b)

w_df <- data.frame(
  w1 = rep(w_grid, 2),
  density = c(density_before, density_after),
  Prior = rep(c("Before (K-only)", "After (Dual-Anchor)"), each = length(w_grid))
)
w_df$Prior <- factor(w_df$Prior, levels = c("Before (K-only)", "After (Dual-Anchor)"))

# Subtitle with math rendering
subtitle_g2 <- bquote(
  "J = 50, " * mu[K] * " = 3 | Before: P(" * w[1] * " > 0.5) = " *
    .(round(P_dom05_before, 3)) * " \u2192 After: " *
    .(round(P_dom05_after, 3))
)

p_g2 <- ggplot(w_df, aes(x = w1, y = density, color = Prior, linetype = Prior)) +
  geom_line(linewidth = 1.2) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_vline(xintercept = 0.9, linetype = "dotted", color = "gray40", linewidth = 0.8) +
  annotate("text", x = 0.52, y = max(w_df$density) * 0.95,
           label = "t = 0.5", hjust = 0, size = 3.5, color = "gray30") +
  annotate("text", x = 0.92, y = max(w_df$density) * 0.95,
           label = "t = 0.9", hjust = 0, size = 3.5, color = "gray30") +
  scale_color_manual(values = palette_dual) +
  scale_linetype_manual(values = c("Before (K-only)" = "solid",
                                   "After (Dual-Anchor)" = "dashed")) +
  labs(
    x = expression(w[1]),
    y = "Density",
    title = expression("Marginal Density of First Stick-Breaking Weight " * w[1]),
    subtitle = subtitle_g2,
    color = "Prior",
    linetype = "Prior"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank()
  )

save_figure(p_g2, file.path(DIR_FIG_SUPP, "Figure_G02"), w = 6, h = 6)
cat("\n")


# =============================================================================
# Section 3: Figure G03 — Confidence Sensitivity
# =============================================================================
# Compares the marginal PMF of K_J across confidence levels
# (high / medium / low), demonstrating how uncertainty specification
# affects the implied prior on cluster counts.

cat("Generating Figure G03: K_J Sensitivity Analysis...\n")

# Fit models with different confidence levels
fits <- list(
  "High confidence"   = DPprior_fit(J = 50, mu_K = 5, confidence = "high"),
  "Medium confidence" = DPprior_fit(J = 50, mu_K = 5, confidence = "medium"),
  "Low confidence"    = DPprior_fit(J = 50, mu_K = 5, confidence = "low")
)

# Compute log-Stirling numbers (needed for PMF calculation)
logS <- compute_log_stirling(J)

# Evaluate the PMF of K_J on a grid of K values
k_grid <- 1:20

k_df <- do.call(rbind, lapply(names(fits), function(nm) {
  f <- fits[[nm]]
  pmf <- pmf_K_marginal(J, f$a, f$b, logS = logS)

  # VIF values used to define confidence levels
  vif <- switch(nm,
                "High confidence" = 1.5,
                "Medium confidence" = 2.5,
                "Low confidence" = 5.0)
  var_K <- vif * (mu_K - 1)   # Var(K) = VIF × (μ_K - 1)

  data.frame(
    K = k_grid,
    probability = pmf[k_grid],
    Confidence = nm,
    VIF = vif,
    Var_K = var_K
  )
}))

k_df$Confidence <- factor(k_df$Confidence,
                          levels = c("High confidence", "Medium confidence", "Low confidence"))

p_g3 <- ggplot(k_df, aes(x = K, y = probability, color = Confidence)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = mu_K, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  annotate("text", x = mu_K + 0.3, y = max(k_df$probability) * 0.95,
           label = expression(mu[K] == 5), parse = FALSE,
           hjust = 0, size = 3.5, color = "gray30") +
  scale_color_manual(values = palette_methods) +
  scale_x_continuous(breaks = seq(0, 20, by = 2)) +
  labs(
    x = expression("Number of Clusters (" * K[J] * ")"),
    y = "Probability",
    title = expression("Marginal PMF of " * K[J] * " Under Different Uncertainty Specifications"),
    subtitle = expression("J = 50, " * mu[K] * " = 5 | VIF: High = 1.5, Medium = 2.5, Low = 5.0"),
    color = "Uncertainty"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank()
  )

save_figure(p_g3, file.path(DIR_FIG_SUPP, "Figure_G03"), w = 6, h = 5)
cat("\n")


# =============================================================================
# Section 4: Figure G04 — Dual-Anchor Trade-off Curve
# =============================================================================
# Varies λ from 1.0 (K-only) to 0.1 and plots the achieved
# E[K_J] vs P(w₁ > 0.5), showing the Pareto frontier.
# Uses the high-risk scenario from Section 2.

cat("Generating Figure G04: Dual-Anchor Trade-off Curve...\n")

# Re-use the high-risk scenario from Figure G02
fit_K_only <- fit_high_risk
target_E_K  <- fit_K_only$target$mu_K
target_Var_K <- fit_K_only$target$var_K

# Lambda grid: from K-only (1.0) to weight-focused (0.1)
lambda_grid <- c(1.0, 0.95, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

tradeoff_results <- lapply(lambda_grid, function(lam) {
  if (lam == 1.0) {
    fit_lam <- fit_K_only   # λ=1 is the K-only solution
  } else {
    fit_lam <- DPprior_dual(
      fit = fit_K_only,
      w1_target = list(prob = list(threshold = 0.5, value = 0.20)),
      lambda = lam,
      loss_type = "adaptive",
      verbose = FALSE
    )
  }

  # Compute achieved metrics
  moments <- exact_K_moments(J, fit_lam$a, fit_lam$b)

  data.frame(
    lambda = lam,
    a = fit_lam$a,
    b = fit_lam$b,
    E_K = moments$mean,
    Var_K = moments$var,
    E_K_deviation = moments$mean - target_E_K,
    Var_K_deviation = moments$var - target_Var_K,
    P_w1_gt_0.5 = prob_w1_exceeds(0.5, fit_lam$a, fit_lam$b),
    E_rho = mean_rho(fit_lam$a, fit_lam$b)
  )
})

tradeoff_df <- do.call(rbind, tradeoff_results)

# Panel A: Achieved E[K_J] vs P(w₁ > 0.5)
subtitle_g4a <- bquote("J = 50, " * mu[K] * " = " * .(target_E_K) *
                         " | Target P(" * w[1] * " > 0.5) = 0.20")

p_g4a <- ggplot(tradeoff_df, aes(x = E_K, y = P_w1_gt_0.5)) +
  geom_path(color = "gray50", linewidth = 1) +
  geom_point(aes(color = lambda), size = 4) +
  geom_hline(yintercept = 0.20, linetype = "dashed", color = "#E41A1C", alpha = 0.7) +
  geom_vline(xintercept = target_E_K, linetype = "dashed", color = "#377EB8", alpha = 0.7) +
  annotate("text", x = target_E_K + 0.05, y = 0.55,
           label = "K target", hjust = 0, size = 3, color = "#377EB8") +
  annotate("text", x = min(tradeoff_df$E_K) + 0.1, y = 0.22,
           label = "w1 target", hjust = 0, size = 3, color = "#E41A1C") +
  scale_color_viridis_c(option = "viridis", begin = 0.2, end = 0.9,
                        direction = -1, name = expression(lambda)) +
  labs(
    x = expression("Achieved E[" * K[J] * "]"),
    y = expression("P(" * w[1] * " > 0.5)"),
    title = "Dual-Anchor Trade-off: Cluster Count vs. Dominance Risk",
    subtitle = subtitle_g4a
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank()
  )

# Panel B: Deviation from target E[K_J] vs P(w₁ > 0.5)
p_g4b <- ggplot(tradeoff_df, aes(x = E_K_deviation, y = P_w1_gt_0.5)) +
  geom_path(color = "gray50", linewidth = 1) +
  geom_point(aes(color = lambda), size = 4) +
  geom_hline(yintercept = 0.20, linetype = "dashed", color = "#E41A1C", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#377EB8", alpha = 0.7) +
  scale_color_viridis_c(option = "viridis", begin = 0.2, end = 0.9,
                        direction = -1, name = expression(lambda)) +
  labs(
    x = expression("Deviation from Target: E[" * K[J] * "] - " * mu[K]),
    y = expression("P(" * w[1] * " > 0.5)"),
    title = "Trade-off in Terms of Deviation",
    subtitle = "Negative deviation indicates lower-than-target E[K]"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 10, color = "gray30"),
    panel.grid.minor = element_blank()
  )

# Combine panels with patchwork
p_g4 <- p_g4a + p_g4b +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

save_figure(p_g4, file.path(DIR_FIG_SUPP, "Figure_G04"), w = 12, h = 6)
cat("\n")


# =============================================================================
# Section 5: Table G07 — Sensitivity Analysis Summary
# =============================================================================
# Summarizes diagnostics across confidence levels for the worked example.

cat("Generating Table G07: Sensitivity Analysis Summary...\n")

summary_tbl <- do.call(rbind, lapply(names(fits), function(nm) {
  f <- fits[[nm]]
  d <- DPprior_diagnostics(f)

  vif <- switch(nm,
                "High confidence" = 1.5,
                "Medium confidence" = 2.5,
                "Low confidence" = 5.0)

  data.frame(
    Confidence = nm,
    VIF = vif,
    `Var(K)` = round(f$target$var_K, 1),
    a = round(f$a, 3),
    b = round(f$b, 3),
    `E[K]` = round(d$K$mean, 2),
    `P(w1>0.5)` = round(d$weights$prob_exceeds["prob_gt_0.5"], 3),
    `E[rho]` = round(d$coclustering$mean, 3),
    check.names = FALSE
  )
}))

# Save as markdown table
md_header <- "| Confidence | VIF | Var(K) | a | b | E[K] | P(w1>0.5) | E[rho] |"
md_sep    <- "|:-----------|:---:|:------:|:----:|:----:|:----:|:---------:|:------:|"
md_rows <- apply(summary_tbl, 1, function(row) {
  paste0("| ", paste(row, collapse = " | "), " |")
})

md_table <- c(
  "**Table G.7: Sensitivity Analysis Summary (J = 50, mu_K = 5)**",
  "",
  md_header,
  md_sep,
  md_rows,
  "",
  "*Note:* VIF = Variance Inflation Factor. Higher VIF implies greater uncertainty about K_J."
)

table_path <- file.path(DIR_RESULTS, "Table_G07_sensitivity_summary.md")
writeLines(md_table, table_path)
cat("  Saved:", table_path, "\n\n")


# =============================================================================
# Completion Summary
# =============================================================================

cat(strrep("=", 70), "\n")
cat("Appendix G Figure Generation Complete!\n")
cat(strrep("=", 70), "\n\n")

cat("Generated files:\n")
cat("  - Figure_G01.pdf/png  (TSMM Diagnostic Dashboard)\n")
cat("  - Figure_G02.pdf/png  (Weight Distribution Comparison)\n")
cat("  - Figure_G03.pdf/png  (K_J Sensitivity to Confidence)\n")
cat("  - Figure_G04.pdf/png  (Dual-Anchor Trade-off Curve)\n")
cat("  - Table_G07_sensitivity_summary.md\n\n")

cat("Session Info:\n")
cat("-------------\n")
print(sessionInfo())
cat("\nDPprior version:", as.character(packageVersion("DPprior")), "\n")
