# Author: JoonHo Lee (jlee296@ua.edu)
# Rebuild the 104 design-size plot from its calibration catalogue.
BASE_SIZE <- 10
d <- ex_read("calibration", "defaults_by_J.csv", "data-frozen")
for (i in which(d$family == "gamma" & is.finite(d$a) & is.finite(d$b))) {
  mom <- ex_moments(d$J[i], d$a[i], d$b[i])
  stopifnot(abs(mom[["mean"]] - d$EK[i]) < 1e-5, abs(ex_tail(d$a[i], d$b[i], .5) - d$wsb_tail_50[i]) < 1e-9)
  d$EK[i] <- mom[["mean"]]
  d$wsb_tail_50[i] <- ex_tail(d$a[i], d$b[i], .5)
}
# --- contract checks on the frozen table ------------------------------------
J_GRID   <- c(10, 20, 30, 50, 79, 100, 200, 500, 1000, 2000, 5000)
J_POINTS <- c(10, 20, 50, 100, 200, 500, 1000, 2000, 5000)   # float plan grid: point markers
J_MAX_Q  <- 500                                                 # DPprior 2.0.0 quantile support

series_levels <- c("Gamma(1, 1)", "Gamma(2, 4)", "Gamma(1, 3)", "alpha = 1 fixed",
                   "count-calibrated at each J",
                   "count-calibrated at J = 500, carried across J")
need_cols <- c("prior_label", "curve", "family", "a", "b", "J", "EK", "SDK", "q05", "q95",
               "wsb_tail_50", "fit_status")
stopifnot(
  all(need_cols %in% names(d)),
  nrow(d) == 66L,
  setequal(unique(d$prior_label), series_levels),
  setequal(unique(d$J), J_GRID),
  all(table(d$prior_label) == length(J_GRID)),
  # quantiles exist only at J <= 500 and never above
  all(is.na(d$q05[d$J > J_MAX_Q])), all(is.na(d$q95[d$J > J_MAX_Q])),
  # the recalibrated form is usable exactly at J = 20 to 500 on this grid
  all(is.na(d$EK[d$curve == "tsmm_recalibrated" & (d$J < 20 | d$J > J_MAX_Q)])),
  all(!is.na(d$EK[d$curve == "tsmm_recalibrated" & d$J >= 20 & d$J <= J_MAX_Q])),
  # the transported prior is the design Section 3.5 count-calibrated prior at J = 500
  all(abs(d$a[d$curve == "tsmm_transported"] - 2.106228394) < 1e-8),
  all(abs(d$b[d$curve == "tsmm_transported"] - 3.078882488) < 1e-8),
  # every fixed hyperprior has a J-free majority tail (closed form) and a mean rising in J
  all(vapply(split(d, d$prior_label), function(s) {
    fixed <- s$curve[1] != "tsmm_recalibrated"
    !fixed || (diff(range(s$wsb_tail_50)) < 1e-12 && all(diff(s$EK[order(s$J)]) > 0))
  }, logical(1)))
)
stopifnot(all(d$prior_label[d$curve == "fixed_reference"] == "alpha = 1 fixed"),
          all(d$family[d$curve == "fixed_reference"] == "point_mass"))
cat("  contract checks on the CSV: pass (66 rows, 6 series, J grid of 11, quantiles only at J <= 500)\n")

# ---------------------------------------------------------------------------
# 2. Series encoding: black/greys plus line type for the catalogue
#    (palette_literature / linetype_literature), TSMM green for the overlay,
#    shapes redundant with colour and line type (grayscale-safe).
# ---------------------------------------------------------------------------
series_labels <- c(
  "Gamma(1, 1)", "Gamma(2, 4)", "Gamma(1, 3)",
  "α = 1 (fixed value, not a prior)",
  "count-calibrated at each J",
  "calibrated at J = 500, transported"   # the same count-calibrated prior, held fixed across J
)
tsmm_green <- unname(palette_roles["TSMM"])
series_colour <- c(palette_literature[1:3], palette_roles[["Neutral"]], tsmm_green, tsmm_green)
series_lty    <- c(linetype_literature[1:3], "dotdash", "solid", "longdash")
series_shape  <- c(16, 17, 18, 1, 15, 0)   # filled circle, triangle, diamond; open circle; filled / open square
names(series_colour) <- names(series_lty) <- names(series_shape) <- series_levels

d$series <- factor(d$prior_label, levels = series_levels)

lines_df  <- d[!is.na(d$EK), ]
points_df <- lines_df[lines_df$J %in% J_POINTS, ]
ribbon_df <- d[d$curve == "tsmm_transported" & d$J <= J_MAX_Q & !is.na(d$q05), ]
ribbon_df <- ribbon_df[order(ribbon_df$J), ]

bands <- data.frame(
  xmin  = c(20, 200),
  xmax  = c(100, 5000),
  label = c("multisite trials\nmeta-analyses\n20 to 100 units",
            "psychometric studies\n200 to 5,000 examinees"),
  x_lab = c(sqrt(20 * 100), 1000),
  stringsAsFactors = FALSE
)
guides_J <- c(48, 79, 500)   # Bangert-Drowns, STAR, Rasch (design sizes of the three applications)
band_fill <- "#56B4E9"       # Okabe-Ito sky blue at low alpha: light, not a role hue in this figure
band_alpha <- 0.13
label_ink <- "#5B6167"

x_breaks <- c(10, 20, 50, 100, 200, 500, 2000, 5000)   # 1000 dropped: its label would touch 500 and 2000 at 9 pt
x_labels <- format(x_breaks, big.mark = ",", trim = TRUE, scientific = FALSE)

scale_x_J <- function() {
  scale_x_log10(breaks = x_breaks, labels = x_labels, limits = c(10, 5000),
                expand = expansion(mult = 0.035))
}
scales_series <- function() {
  list(
    scale_colour_manual(values = series_colour, labels = series_labels, name = NULL, drop = FALSE),
    scale_linetype_manual(values = series_lty, labels = series_labels, name = NULL, drop = FALSE),
    scale_shape_manual(values = series_shape, labels = series_labels, name = NULL, drop = FALSE)
  )
}
# Design bands plus the dotted guides. In panel A the guides stop below the
# band labels (y_top finite) so that no text is drawn over a line; the guide
# style is that of reference_line(kind = "guide") in theme_v3.R.
band_layers <- function(y_top = Inf) {
  guides <- if (is.finite(y_top)) {
    annotate("segment", x = guides_J, xend = guides_J, y = 0, yend = y_top,
             colour = "#9AA0A6", linewidth = 0.35, linetype = "13")
  } else {
    reference_line(xintercept = guides_J, kind = "guide")
  }
  list(
    annotate("rect", xmin = bands$xmin, xmax = bands$xmax, ymin = -Inf, ymax = Inf,
             fill = band_fill, alpha = band_alpha),
    guides
  )
}
x_lab <- expression(paste("Number of units ", J, " (log scale)"))

# ---------------------------------------------------------------------------
# 3. Panel A: expected number of clusters
# ---------------------------------------------------------------------------
yA_top <- 13.6
yA_label_floor <- 10.9   # the band labels hang from yA_top down to about here
pA <- ggplot(lines_df, aes(J, EK, colour = series, linetype = series, shape = series)) +
  band_layers(y_top = yA_label_floor) +
  geom_ribbon(data = ribbon_df, aes(x = J, ymin = q05, ymax = q95),
              inherit.aes = FALSE, fill = tsmm_green, alpha = 0.18) +
  geom_line(linewidth = 0.55) +
  geom_point(data = points_df, size = 1.6) +
  annotate("text", x = bands$x_lab, y = yA_top - 0.25, label = bands$label,
           size = pt_size(8), colour = label_ink, lineheight = 0.95, vjust = 1) +
  scale_x_J() +
  scale_y_continuous(breaks = seq(0, 12, 2), limits = c(0, yA_top), expand = c(0, 0)) +
  scales_series() +
  labs_panel(panel_title("A", "Expected cluster count")) +
  labs(x = x_lab, y = "Expected number of clusters") +
  theme_v3(base_size = BASE_SIZE, legend = "bottom")

# ---------------------------------------------------------------------------
# 4. Panel B: majority-cluster probability (J-free for a fixed hyperprior)
# ---------------------------------------------------------------------------
pB <- ggplot(lines_df, aes(J, wsb_tail_50, colour = series, linetype = series, shape = series)) +
  band_layers() +
  geom_line(linewidth = 0.55) +
  geom_point(data = points_df, size = 1.6) +
  scale_x_J() +
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = c("0", "0.25", "0.50", "0.75", "1.00"),
                     limits = c(0, 1), expand = expansion(mult = c(0, 0.02))) +
  scales_series() +
  labs_panel(panel_title("B", "Size-biased majority probability")) +
  labs(x = x_lab, y = "Size-biased majority probability") +
  theme_v3(base_size = BASE_SIZE, legend = "bottom")

# ---------------------------------------------------------------------------
# 5. Assemble: one collected bottom legend (two rows: the six keys do not fit
#    one 6.5-in line at 9 pt), wider keys so the line types read.
# ---------------------------------------------------------------------------
fig <- wrap_plots(pA, pB, ncol = 2, widths = c(1, 1)) +
  plot_layout(guides = "collect") &
  guides(colour = guide_legend(nrow = 2, byrow = TRUE),
         linetype = guide_legend(nrow = 2, byrow = TRUE),
         shape = guide_legend(nrow = 2, byrow = TRUE)) &
  theme(legend.position = "bottom",
        legend.key.width = grid::unit(20, "pt"),
        legend.key.spacing.x = grid::unit(3, "pt"),
        legend.text = element_text(size = BASE_SIZE - 1, margin = margin(r = 4)))


ex_figure("main-figure-2", fig, d, "data-frozen/calibration/defaults_by_J.csv", width = 6.5, height = 3.6,
          tier = "recalculated moments with frozen discrete quantiles",
          note = "The transported prior's 5th to 95th percentile band ends at 500 units. The calibrated curve begins at 20.")
