# Author: JoonHo Lee (jlee296@ua.edu)
# Plot the two panels with the 104 artwork settings.
J <- 50L
a <- 1.60
b <- 1.22
k_show <- 15L
moments <- ex_moments(J, a, b)
E_K <- moments[["mean"]]
SD_K <- sqrt(moments[["var"]])
p05 <- ex_tail(a, b, .5)
p09 <- ex_tail(a, b, .9)
pmf <- as.numeric(DPprior::pmf_K_marginal(J, a, b, DPprior::compute_log_stirling(J),
                                          M = 160L, M_verify = 320L, strict = TRUE))[-1L]
nb_mean <- E_K - 1
target <- dnbinom(0:(J - 1), size = nb_mean^2 / (moments[["var"]] - nb_mean), mu = nb_mean)
target <- target / sum(target)
w <- seq(.001, .999, length.out = 500L)
fw <- a * b^a / ((1 - w) * (b - log1p(-w))^(a + 1))
frame <- rbind(data.frame(panel = "A", x = 1:J, induced = pmf, target = target, density = NA_real_),
               data.frame(panel = "B", x = w, induced = NA_real_, target = NA_real_, density = fw))
# --- panel A ------------------------------------------------------------------------
dA <- data.frame(K = 1:k_show, induced = pmf[1:k_show], target = target[1:k_show]); ymax <- max(dA$induced, dA$target)
ink <- "#1F2328"; bar_blue <- "#4E79A7"
pA <- ggplot(dA, aes(x = K)) +
  geom_col(aes(y = induced), fill = bar_blue, width = 0.72) +
  geom_line(aes(y = target), colour = ink, linetype = "22", linewidth = 0.45) +
  geom_point(aes(y = target), shape = 21, fill = "white", colour = ink, size = 1.9, stroke = 0.6) +
  annotate("text", x = 9.4, y = ymax * 0.95, label = sprintf("E(K[50]) == %.2f", E_K), parse = TRUE, hjust = 0, size = pt_size(9), family = v3_font, colour = ink) +
  annotate("text", x = 9.4, y = ymax * 0.82, label = sprintf("SD(K[50]) == %.2f", SD_K), parse = TRUE, hjust = 0, size = pt_size(9), family = v3_font, colour = ink) +
  annotate("point", x = 9.7, y = ymax * 0.64, shape = 21, fill = "white", colour = ink, size = 1.9, stroke = 0.6) +
  annotate("text", x = 10.3, y = ymax * 0.64, label = "Target", hjust = 0, size = pt_size(9), family = v3_font, colour = "#3B4045") +
  annotate("rect", xmin = 9.4, xmax = 10.0, ymin = ymax * 0.50, ymax = ymax * 0.56, fill = bar_blue) +
  annotate("text", x = 10.3, y = ymax * 0.53, label = "Induced", hjust = 0, size = pt_size(9), family = v3_font, colour = "#3B4045") +
  scale_x_continuous(breaks = seq(1, 15, 2)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs_panel(panel_title("A", "The cluster view", "The prior reproduces the stated cluster count", title_expr = quote(paste("The cluster view (", K[J], ")"))),
             x = expression(paste("Number of clusters ", K[50])), y = "Probability") +
  theme_v3(grid = "y", legend = "none")

# --- panel B ------------------------------------------------------------------------
dB <- data.frame(w = w, f = fw); fmax <- max(fw)
pB <- ggplot(dB, aes(x = w, y = f)) +
  geom_ribbon(data = dB[dB$w >= 0.5, ], aes(ymin = 0, ymax = f), fill = "#FDAE6B", alpha = 0.55) +
  geom_ribbon(data = dB[dB$w >= 0.9, ], aes(ymin = 0, ymax = f), fill = "#D94701", alpha = 0.70) +
  reference_line(xintercept = 0.5, kind = "guide") + reference_line(xintercept = 0.9, kind = "guide") +
  geom_line(colour = bar_blue, linewidth = 0.8) +
  annotate("text", x = 0.52, y = fmax * 0.93, label = sprintf("P(W[SB] > 0.5) == %.2f", p05), parse = TRUE, hjust = 0, size = pt_size(9), family = v3_font, colour = "#B03A2E") +
  annotate("text", x = 0.56, y = fmax * 0.50, label = sprintf("P(W[SB] > 0.9) == %.2f", p09), parse = TRUE, hjust = 0, size = pt_size(9), family = v3_font, colour = "#B03A2E") +
  annotate("segment", x = 0.78, xend = 0.935, y = fmax * 0.44, yend = fmax * 0.16, arrow = arrow(length = unit(1.6, "mm")), colour = "#5B6167", linewidth = 0.35) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), labels = c("0", "0.25", "0.5", "0.75", "1")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs_panel(panel_title("B", "The weight view", "A random unit often belongs to a large component", title_expr = quote(paste("The weight view (", W[SB], ")"))),
             x = expression(paste("Size-biased first weight ", W[SB])), y = "Density") +
  theme_v3(grid = "y", legend = "none")

p <- pA + pB + plot_layout(widths = c(1, 1))

ex_figure("main-figure-1", p, frame, "calibration/v104/figure1.R", width = 6.5, height = 3.25,
          tier = "recalculated", note = "Exact finite count distribution and size-biased weight density for Gamma(1.60, 1.22).")
