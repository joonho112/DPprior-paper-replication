# ==============================================================================
# Figure 2: The "Unintended Prior" Phenomenon
# ==============================================================================
#
# Paper:   "Design-Conditional Prior Elicitation for Dirichlet Process Mixtures:
#            A Unified Framework for Cluster Counts and Weight Control"
# Author:  JoonHo Lee (University of Alabama, jlee296@ua.edu)
# Output:  Figure 2 (Main Text, Section 2.3, Figure 1 in the manuscript)
#
# Description:
#   Demonstrates that a Gamma hyperprior calibrated to match a plausible target
#   distribution for K_J (cluster counts) can nevertheless imply substantial
#   dominance risk under the weight view.
#
#   Panel A — The Cluster View (K_J):
#     Target (shifted negative-binomial) vs. induced marginal K_J distribution.
#     Shows that the prior fits the expected cluster count well.
#
#   Panel B — The Weight View (w_1):
#     Marginal density of the first stick-breaking weight w_1, with shaded
#     regions for Pr(w_1 > 0.5) and Pr(w_1 > 0.9). Shows that the same prior
#     implies high probability of a single dominant cluster.
#
# Scenario:
#   J = 50 (design size), mu_K = 5 (target E[K_J]),
#   alpha ~ Gamma(1.60, 1.22) (shape-rate parameterization)
#
# Required packages:
#   ggplot2, patchwork, scales
#
# Output files:
#   ../../figures/main/Figure_02.pdf
#   ../../figures/main/Figure_02.png
#
# Usage:
#   Set working directory to this file's location, then:
#   source("figure_02_unintended_prior.R")
#
# ==============================================================================


# =============================================================================
# (1) Setup & Packages
# =============================================================================

if (!require("ggplot2"))  install.packages("ggplot2")
if (!require("patchwork")) install.packages("patchwork")
if (!require("scales"))    install.packages("scales")

library(ggplot2)
library(patchwork)
library(scales)

set.seed(2026)


# =============================================================================
# (2) Helper Functions (self-contained, no external package dependency)
# =============================================================================

#' Compute log unsigned Stirling numbers of the first kind via dynamic programming.
#' Uses log-sum-exp for numerical stability.
#'
#' @param J_max Maximum design size
#' @return Matrix of log |s(n, k)| values, (J_max+1) x (J_max+1), 0-indexed
compute_log_stirling <- function(J_max) {
  logS <- matrix(-Inf, nrow = J_max + 1, ncol = J_max + 1)
  logS[1, 1] <- 0  # |s(0, 0)| = 1

  for (n in 1:J_max) {
    logS[n + 1, n + 1] <- 0  # |s(n, n)| = 1
    if (n >= 2) {
      log_n <- log(n - 1)
      for (k in 1:(n - 1)) {
        # Recurrence: |s(n, k)| = |s(n-1, k-1)| + (n-1) * |s(n-1, k)|
        t1 <- logS[n, k]
        t2 <- log_n + logS[n, k + 1]
        m <- pmax(t1, t2)
        logS[n + 1, k + 1] <- m + log1p(exp(-abs(t1 - t2)))
      }
    }
  }
  return(logS)
}

#' Marginal PMF of K_J under alpha ~ Gamma(a, b) via grid integration.
#' Integrates the Antoniak likelihood over a Gamma prior on alpha.
#'
#' @param J   Design size
#' @param a   Gamma shape parameter
#' @param b   Gamma rate parameter
#' @param logS Log-Stirling matrix from compute_log_stirling()
#' @return Vector of length J+1: pmf[k+1] = Pr(K_J = k), k = 0, ..., J
pmf_K_marginal <- function(J, a, b, logS) {
  alpha_grid <- seq(0.001, 20, length.out = 3000)
  delta <- alpha_grid[2] - alpha_grid[1]

  prior_dens <- dgamma(alpha_grid, shape = a, rate = b)
  log_alpha  <- log(alpha_grid)
  log_poch   <- lgamma(alpha_grid + J) - lgamma(alpha_grid)

  pmf <- numeric(J + 1)
  for (k in 1:J) {
    log_stirling <- logS[J + 1, k + 1]
    log_lik <- log_stirling + k * log_alpha - log_poch
    pmf[k + 1] <- sum(exp(log_lik) * prior_dens) * delta
  }

  return(pmf / sum(pmf))
}

#' Compute mean and variance of K_J from its PMF.
compute_K_moments <- function(pmf) {
  k_vals <- 0:(length(pmf) - 1)
  E_K    <- sum(k_vals * pmf)
  E_K2   <- sum(k_vals^2 * pmf)
  Var_K  <- E_K2 - E_K^2
  list(mean = E_K, var = Var_K, sd = sqrt(Var_K))
}

#' Marginal PDF of w_1 under alpha ~ Gamma(a, b).
#' Closed-form: f(w) = a * b^a * (1-w)^{-1} * (b - log(1-w))^{-(a+1)}
#' Reference: Vicentini & Jermyn (2025), Appendix E.3 in the manuscript.
pdf_w1 <- function(w, a, b) {
  w <- pmin(w, 0.999999)
  a * (b^a) * (1 - w)^(-1) * (b - log(1 - w))^(-(a + 1))
}

#' Tail probability Pr(w_1 > t) under alpha ~ Gamma(a, b).
#' Closed-form: (b / (b - log(1 - t)))^a
prob_w1_exceeds <- function(t, a, b) {
  (b / (b - log(1 - t)))^a
}

#' E[w_1] under alpha ~ Gamma(a, b) via numerical integration.
#' Uses E[1/(1 + alpha)] integrated against the Gamma density.
mean_w1 <- function(a, b) {
  alpha_grid <- seq(0.001, 30, length.out = 5000)
  delta <- alpha_grid[2] - alpha_grid[1]
  integrand <- 1 / (1 + alpha_grid) * dgamma(alpha_grid, shape = a, rate = b)
  sum(integrand) * delta
}


# =============================================================================
# (3) Parameters & Configuration
# =============================================================================

# Scenario parameters (representative example from Section 2.3)
J     <- 50      # Design size (number of exchangeable units)
mu_K  <- 5       # Target expected cluster count E[K_J]
var_K <- 12.5    # Target Var(K_J), corresponds to "medium" confidence

# Calibrated hyperprior via TSMM: alpha ~ Gamma(a, b) in shape-rate form
a_fit <- 1.60
b_fit <- 1.22

# Color palette (colorblind-friendly)
COL_INDUCED    <- "#377EB8"   # Blue for induced distribution
COL_TARGET     <- "black"     # Black for target reference
COL_RISK_LIGHT <- "#FDAE6B"   # Light orange for Pr(w1 > 0.5) region
COL_RISK_DARK  <- "#D94701"   # Dark orange for Pr(w1 > 0.9) region
COL_ANNOT      <- "#E41A1C"   # Red for probability annotations

# Publication theme
theme_pub <- theme_classic(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13, hjust = 0),
    plot.subtitle = element_text(size = 11, color = "grey30",
                                 margin = margin(b = 10)),
    plot.caption  = element_text(size = 10, color = "grey30", hjust = 0,
                                 margin = margin(t = 12)),
    axis.title    = element_text(face = "bold", size = 11),
    axis.text     = element_text(size = 10, color = "grey20"),
    panel.grid.major.y = element_line(color = "grey90", linetype = "dashed"),
    legend.position = "none"
  )


# =============================================================================
# (4) Computation
# =============================================================================

cat("=== Generating Figure 2 Data ===\n\n")

# --- K_J distribution for Panel A ---
cat("Computing K_J distribution...\n")
logS <- compute_log_stirling(J)
pmf_induced_full <- pmf_K_marginal(J, a_fit, b_fit, logS)

# Extract k = 1, ..., J (drop k = 0 which has probability 0)
pmf_induced <- as.numeric(pmf_induced_full[-1])
pmf_induced <- pmf_induced / sum(pmf_induced)

# Exact moments
K_moments <- compute_K_moments(pmf_induced_full)
E_K  <- K_moments$mean
SD_K <- K_moments$sd

cat(sprintf("  E[K_%d] = %.3f\n", J, E_K))
cat(sprintf("  SD(K_%d) = %.3f\n", J, SD_K))

# Target distribution: Shifted Negative Binomial
# K-1 ~ NegBin(size, prob) with E[K-1] = mu_K - 1, Var[K-1] = var_K
M_shift <- mu_K - 1
D_gap   <- var_K - M_shift
size_nb <- (M_shift^2) / D_gap
prob_nb <- size_nb / (size_nb + M_shift)

k_vals         <- 1:J
pmf_target_raw <- dnbinom(k_vals - 1, size = size_nb, prob = prob_nb)
pmf_target     <- pmf_target_raw / sum(pmf_target_raw)

# Combine into data frame
df_k <- data.frame(K = k_vals, Induced = pmf_induced, Target = pmf_target)
K_max     <- 15
df_k_plot <- df_k[df_k$K <= K_max, , drop = FALSE]

# --- w_1 distribution for Panel B ---
cat("\nComputing w_1 distribution...\n")
w_grid <- seq(0.001, 0.999, length.out = 1000)
df_w   <- data.frame(w1 = w_grid, density = pdf_w1(w_grid, a_fit, b_fit))

# Dominance risk statistics
p_05 <- prob_w1_exceeds(0.5, a_fit, b_fit)
p_09 <- prob_w1_exceeds(0.9, a_fit, b_fit)
E_w1 <- mean_w1(a_fit, b_fit)

cat(sprintf("  E[w_1] = %.3f\n", E_w1))
cat(sprintf("  P(w_1 > 0.5) = %.3f\n", p_05))
cat(sprintf("  P(w_1 > 0.9) = %.3f\n", p_09))


# =============================================================================
# (5) Plotting
# =============================================================================

# --- Panel A: K_J distribution ---
pA <- ggplot(df_k_plot, aes(x = K)) +
  geom_col(aes(y = Induced), fill = COL_INDUCED, alpha = 0.7, width = 0.75) +
  geom_line(aes(y = Target), color = COL_TARGET, linetype = "dashed",
            linewidth = 0.7) +
  geom_point(aes(y = Target), color = COL_TARGET, size = 2, shape = 21,
             fill = "white", stroke = 1) +
  # Statistics annotations
  annotate("text", x = 9.5, y = max(df_k_plot$Induced) * 0.95,
           label = sprintf("E[K[50]] == %.2f", E_K),
           parse = TRUE, hjust = 0, size = 4, color = "grey20") +
  annotate("text", x = 9.5, y = max(df_k_plot$Induced) * 0.82,
           label = sprintf("SD(K[50]) == %.2f", SD_K),
           parse = TRUE, hjust = 0, size = 4, color = "grey20") +
  # Manual legend
  annotate("point", x = 9.5, y = max(df_k_plot$Induced) * 0.66,
           shape = 21, color = COL_TARGET, fill = "white", size = 2.5,
           stroke = 1) +
  annotate("text", x = 10, y = max(df_k_plot$Induced) * 0.66,
           label = "Target", hjust = 0, size = 3.8, color = "grey30") +
  annotate("rect", xmin = 9.3, xmax = 9.9,
           ymin = max(df_k_plot$Induced) * 0.53,
           ymax = max(df_k_plot$Induced) * 0.58,
           fill = COL_INDUCED, alpha = 0.7) +
  annotate("text", x = 10, y = max(df_k_plot$Induced) * 0.555,
           label = "Induced", hjust = 0, size = 3.8, color = "grey30") +
  scale_x_continuous(breaks = seq(2, 14, 2)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = expression(bold("A.") ~ " The Cluster View (" * K[J] * ")"),
    subtitle = "Prior matches expected cluster count well",
    x = expression("Number of Clusters (" * K[J] * ")"),
    y = "Probability"
  ) +
  theme_pub

# --- Panel B: w_1 density ---
y_max <- max(df_w$density[is.finite(df_w$density)], na.rm = TRUE)
if (!is.finite(y_max) || y_max > 12) y_max <- 12

pB <- ggplot(df_w, aes(x = w1, y = density)) +
  geom_ribbon(data = subset(df_w, w1 >= 0.5),
              aes(ymin = 0, ymax = density),
              fill = COL_RISK_LIGHT, alpha = 0.5) +
  geom_ribbon(data = subset(df_w, w1 >= 0.9),
              aes(ymin = 0, ymax = density),
              fill = COL_RISK_DARK, alpha = 0.6) +
  geom_line(color = COL_INDUCED, linewidth = 1) +
  geom_vline(xintercept = c(0.5, 0.9), linetype = "dotted", color = "grey50") +
  annotate("text", x = 0.55, y = y_max * 0.88,
           label = sprintf("P(w[1] > 0.5) == %.2f", p_05),
           parse = TRUE, hjust = 0, size = 4, color = COL_ANNOT) +
  annotate("text", x = 0.65, y = y_max * 0.42,
           label = sprintf("P(w[1] > 0.9) == %.2f", p_09),
           parse = TRUE, hjust = 0, size = 4, color = COL_ANNOT) +
  annotate("segment", x = 0.63, xend = 0.88,
           y = y_max * 0.35, yend = y_max * 0.18,
           arrow = arrow(length = unit(0.15, "cm")), color = "grey40") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     labels = c("0", ".25", ".5", ".75", "1")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, y_max)) +
  labs(
    title    = expression(bold("B.") ~ " The Weight View (" * w[1] * ")"),
    subtitle = "But implies high risk of a single dominant cluster",
    x = expression("Size of First Cluster (" * w[1] * ")"),
    y = "Density"
  ) +
  theme_pub


# =============================================================================
# (6) Save Outputs
# =============================================================================

# Assemble two-panel figure
caption_text <- sprintf(
  "Note: J = %d. Prior alpha ~ Gamma(%.2f, %.2f). w1 is the size-biased (GEM-order) first cluster proportion.",
  J, a_fit, b_fit
)

fig_final <- pA + pB +
  plot_layout(ncol = 2) +
  plot_annotation(
    caption = caption_text,
    theme = theme(
      plot.caption = element_text(size = 10, color = "grey30", hjust = 0)
    )
  )

# Output directory (relative to this script's location: code/figure_02/)
out_dir <- file.path("..", "..", "figures", "main")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "Figure_02.pdf"), fig_final,
       width = 10, height = 5, device = cairo_pdf)
ggsave(file.path(out_dir, "Figure_02.png"), fig_final,
       width = 10, height = 5, dpi = 300)

cat("\nFigures saved to:", normalizePath(out_dir, mustWork = FALSE), "\n")


# =============================================================================
# Summary for Manuscript
# =============================================================================

cat("\n")
cat(strrep("=", 70), "\n")
cat("FIGURE 2 SUMMARY FOR MANUSCRIPT\n")
cat(strrep("=", 70), "\n")
cat(sprintf("Design size: J = %d\n", J))
cat(sprintf("Prior: alpha ~ Gamma(%.3f, %.3f)\n", a_fit, b_fit))
cat(sprintf("Target: E[K_J] = %d, Var(K_J) = %.1f\n", mu_K, var_K))
cat(sprintf("Achieved: E[K_J] = %.3f, SD(K_J) = %.3f\n", E_K, SD_K))
cat(sprintf("Weight statistics: E[w1] = %.3f\n", E_w1))
cat(sprintf("Dominance risk: P(w1 > 0.5) = %.3f\n", p_05))
cat(sprintf("Extreme dominance: P(w1 > 0.9) = %.3f\n", p_09))
cat(strrep("=", 70), "\n")
cat("\nFigure 2 generated successfully.\n")
