#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Rebuild main Figure 6 and OSM Figure H.5 using the 104 vector artwork.
options(device = function(...) grDevices::pdf(file = NULL))
suppressPackageStartupMessages(library(ggplot2))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: Rscript figure-pooling.R FRAMEDIR OUTDIR")
data_dir <- normalizePath(args[1], mustWork = TRUE)
dir.create(args[2], showWarnings = FALSE, recursive = TRUE)
fig_dir <- normalizePath(args[2], mustWork = TRUE)
read_results <- function(name) read.csv(file.path(data_dir, name), stringsAsFactors = FALSE)
ink <- "#263641"
middle <- "#697B86"
upper <- "#A74622"
ref <- "#3E6367"
apps <- c(star = "A   Project STAR", bd = "B   Writing-to-learn")
panel <- function(app, labels = apps) factor(app, levels = names(apps), labels = labels)

figure_theme <- function(base_size, title_size, tick_size) {
  theme_minimal(base_size = base_size, base_family = "Helvetica") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#E5E8EA", linewidth = .24),
        panel.spacing = grid::unit(1.15, "lines"),
        strip.text = element_text(hjust = 0, face = "bold", color = ink, size = base_size,
                                  margin = margin(t = 3, r = 0, b = 6, l = 0)),
        axis.title = element_text(size = title_size, color = ink),
        axis.text = element_text(size = tick_size, color = ink),
        legend.position = "bottom", legend.title = element_blank(),
        legend.margin = margin(t = -1, b = -1),
        legend.box.spacing = grid::unit(2, "pt"), plot.margin = margin(2, 5, 2, 4))
}
save_figure <- function(plot, name, height, width = 6.3) {
  pdf <- file.path(fig_dir, name)
  ggsave(pdf, plot, device = grDevices::pdf, width = width, height = height,
         useDingbats = FALSE, bg = "white")
  png_stem <- tools::file_path_sans_ext(pdf)
  status <- system2("pdftocairo", c("-png", "-r", "180", "-singlefile", shQuote(pdf), shQuote(png_stem)))
  if (status != 0L) stop("Poppler failed to render ", name)
}

u <- subset(read_results("figure-6.csv"), group %in% c("upper", "central"))
stopifnot(nrow(u) == 77L, !anyDuplicated(u[c("app", "unit")]),
          identical(as.integer(table(factor(u$app[u$group == "upper"], levels = names(apps)))), c(8L, 5L)))
u$group <- factor(u$group, levels = c("central", "upper"), labels = c("Middle 50%", "Upper 10%"))
scatter_panels <- paste0(apps, "\n", c("Achievement SDs", "Standardized mean differences"))
u$panel <- panel(u$app, scatter_panels)
u$x <- 100 * (u$coassign_dual - u$coassign_gamma)
u$y <- u$displacement_dual - u$displacement_gamma
stopifnot(max(abs(u$x - 100 * u$delta_coassign)) < 1e-10,
          max(abs(u$y - u$delta_displacement)) < 1e-10)
hi <- subset(u, group == "Upper 10%")
lo <- subset(u, group == "Middle 50%")
refs <- aggregate(cbind(gamma = coassign_gamma, dual = coassign_dual) ~ app, lo, mean)
refs$panel <- panel(refs$app, scatter_panels)
refs$x <- 100 * (refs$dual - refs$gamma)
refs$align <- ifelse(refs$app == "bd", .82, .50)
stopifnot(identical(round(refs$x[match(names(apps), refs$app)], 2), c(-26.01, -16.20)))
refs$label <- sprintf("Central mean: %.2f pp\n%.2f%% to %.2f%%", refs$x, 100 * refs$gamma, 100 * refs$dual)
# Label the largest shift in each application on the open side of its error bar.
labels <- subset(hi, (app == "star" & unit == 22) | (app == "bd" & unit == 46))
labels$label <- ifelse(labels$app == "star", paste("School", labels$unit_id), "Willey (1988)\nstudy 46")
labels$label_x <- labels$x + .56
windows <- data.frame(app = rep(c("star", "bd"), each = 2),
                      edge = c(-35.5, -22.5, -26.5, -13.5))
windows$panel <- panel(windows$app, scatter_panels)
for (app_name in names(apps)) {
  bounds <- windows$edge[windows$app == app_name]
  x <- u$x[u$app == app_name]
  stopifnot(diff(range(bounds)) == 13, all(x > min(bounds)), all(x < max(bounds)))
}
scatter <- ggplot(u, aes(x, y)) +
  geom_blank(data = windows, aes(x = edge, y = 0), inherit.aes = FALSE) +
  geom_hline(yintercept = 0, linewidth = .37, color = "#49555E") +
  geom_segment(data = refs, aes(x = x, xend = x, y = -.063, yend = .012),
               inherit.aes = FALSE, linetype = "22", linewidth = .4, color = ref) +
  geom_text(data = refs, aes(x = x, label = label, hjust = align), y = .0134, vjust = 0,
            inherit.aes = FALSE, size = 3.0, lineheight = .94, color = ref) +
  geom_point(data = lo, aes(shape = group, color = group), size = 1.7, alpha = .72) +
  geom_linerange(data = hi, aes(ymin = y - mcse_delta_mean, ymax = y + mcse_delta_mean),
                 color = upper, linewidth = .46) +
  geom_point(data = hi, aes(shape = group, color = group), size = 2.4) +
  geom_text(data = labels, aes(x = label_x, label = label), hjust = 0, size = 3.10,
            color = "#793519", lineheight = .93, show.legend = FALSE) +
  facet_wrap(~panel, nrow = 1, scales = "free_x") +
  scale_shape_manual(values = c("Middle 50%" = 16, "Upper 10%" = 17)) +
  scale_color_manual(values = c("Middle 50%" = middle, "Upper 10%" = upper)) +
  scale_x_continuous(breaks = seq(-36, -12, 2), labels = function(x) x, expand = expansion(0)) +
  scale_y_continuous(limits = c(-.063, .025), breaks = seq(-.06, .02, .02),
                     labels = function(x) sprintf("%.02f", x), expand = expansion(0)) +
  labs(subtitle = "Changes are Dual-Anchor minus Gamma(1, 1).",
       x = "Change in probability of sharing a cluster with a middle-group unit\n(percentage points)",
       y = "Change in distance from posterior mean\nto observed effect",
       caption = "Dashed lines mark the middle-group means. Bars show ±1 Monte Carlo SE.\nThe horizontal axes cover different ranges with the same spacing per percentage point.") +
  figure_theme(10.5, 10, 9.25) +
  theme(plot.subtitle = element_text(size = 10, colour = ink, margin = margin(b = 9)),
        plot.caption = element_text(size = 8, hjust = 0, colour = ink, margin = margin(t = 5)),
        axis.title.x = element_text(margin = margin(t = 6)),
        legend.margin = margin(t=2, b=3),
        plot.margin = margin(9, 10, 9, 10))
save_figure(scatter, "main-figure-6.pdf", height = 4.05, width = 6.45)

k <- read_results("fixed-count.csv")
k$panel <- panel(k$app)
k$prior <- factor(k$prior, levels = c("gamma", "dual"), labels = c("Gamma(1, 1)", "Dual-Anchor"))
k$gap <- -k$upper_minus_central_pp
stopifnot(all(k$K %in% 1:4), all(k$gap[k$K == 1] == 0))
by_count <- ggplot(k, aes(K, gap, group = prior, color = prior, linetype = prior, shape = prior)) +
  geom_line(linewidth = .7) + geom_point(size = 2.5) + facet_wrap(~panel, nrow = 1) +
  scale_color_manual(values = c("Gamma(1, 1)" = middle, "Dual-Anchor" = upper)) +
  scale_linetype_manual(values = c("Gamma(1, 1)" = "solid", "Dual-Anchor" = "32")) +
  scale_shape_manual(values = c("Gamma(1, 1)" = 1, "Dual-Anchor" = 3)) +
  scale_x_continuous(breaks = 1:4, expand = expansion(mult = .06)) +
  scale_y_continuous(limits = c(-.5, 15), breaks = seq(0, 15, 3)) +
  labs(x = "Number of clusters in a posterior draw",
       y = "Difference in co-clustering probability\n(percentage points)") + figure_theme(9.5, 9, 8.4)
save_figure(by_count, "osm-figure-H.5.pdf", 2.75)
