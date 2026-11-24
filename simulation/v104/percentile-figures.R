# Author: JoonHo Lee (jlee296@ua.edu)
# The two OSM percentile figures share the finite-site paired-data summaries.
draw_percentile_figures <- function(s, out) {
  probs <- c(1, 5, 25, 50, 75, 95, 99)
  s <- s[order(s$I_target, s$K_true, s$percentile), ]
  s$rank <- match(s$percentile, probs)
  s$count <- factor(s$K_true, levels = c(5, 10, 30),
                    labels = c("K* = 5", "K* = 10", "K* = 30"))
  s$info <- factor(s$I_target, levels = c(.2, .5, .8),
                   labels = c("I = 0.2", "I = 0.5", "I = 0.8"))
  stopifnot(nrow(s) == 63, !anyNA(s[c("rank", "count", "info")]),
            all(xtabs(~ rank + count + info, s) == 1L),
            sum(unique(s[c("count", "info", "n")])$n) == 1681)

  series <- function(y, label, offset, se = NULL) {
    d <- data.frame(rank = s$rank + offset, y, label, count = s$count, info = s$info)
    if (!is.null(se)) {
      margin <- qt(.975, s$n - 1) * se
      d$lo <- y - margin
      d$hi <- y + margin
    }
    d
  }
  make_plot <- function(d, levels, colors, symbols, ylab, scale_y, intervals) {
    d$label <- factor(d$label, levels = levels)
    free <- if (intervals) "free_y" else "fixed"
    p <- ggplot(d, aes(rank, y, group = label, colour = label, linetype = label, shape = label)) +
      facet_grid(info ~ count, scales = free) +
      geom_vline(xintercept = 4, colour = "grey80", linewidth = .25) +
      geom_hline(yintercept = 0, colour = "grey68", linewidth = .28) +
      geom_line(linewidth = .4)
    if (intervals) {
      p <- p + geom_linerange(aes(ymin = lo, ymax = hi), linetype = 1,
                              linewidth = .35, show.legend = FALSE)
    }
    p + geom_point(size = 1.45, stroke = .43, fill = "white") +
      scale_x_continuous(breaks = seq_along(probs), labels = probs,
                         limits = c(.65, 7.35), expand = expansion(mult = 0)) +
      scale_y + scale_colour_manual(values = colors, name = NULL) +
      scale_linetype_manual(values = c("solid", "32"), name = NULL) +
      scale_shape_manual(values = symbols, name = NULL) +
      labs(x = "Percentile of the 100 site effects", y = ylab) +
      theme_minimal(base_size = 8.6) +
      theme(legend.position = "top", legend.justification = "left",
            legend.margin = margin(0, 0, 0, 0), legend.key.height = grid::unit(10, "pt"),
            legend.key.width = grid::unit(24, "pt"), legend.box.spacing = grid::unit(2, "pt"),
            axis.text = element_text(size = 7.6, colour = "#26363E"),
            strip.text = element_text(size = 8.2, colour = "#26363E", face = "bold"),
            strip.text.y.right = element_text(angle = 0),
            strip.background = element_rect(fill = "#EFF3F3", colour = NA),
            panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
            panel.grid.major.y = element_line(colour = "#E0E5E5", linewidth = .25),
            panel.spacing = grid::unit(7, "pt"), plot.margin = margin(2, 2, 1, 1))
  }

  mv <- rbind(series(s$mean_abs_shift, "Mean absolute change", -.07, s$mcse_abs_shift),
              series(s$mean_shift, "Mean signed change", .07, s$mcse_shift))
  breaks_by_row <- function(limits) {
    extent <- max(abs(limits))
    step <- if (extent > .025) .03 else if (extent > .008) .01 else .002
    scales::breaks_width(step)(limits)
  }
  movement <- make_plot(mv, c("Mean absolute change", "Mean signed change"),
                        c("#167366", "#AD492B"), c(16, 24), "Change (effect units)",
                        scale_y_continuous(labels = scales::label_number(accuracy = .001),
                                           breaks = breaks_by_row), TRUE)
  rec <- rbind(series(s$rmse_gamma, "Gamma(1, 1)", -.07),
               series(s$rmse_dual, "Dual-Anchor", .07))
  recovery <- make_plot(rec, c("Gamma(1, 1)", "Dual-Anchor"),
                        c("#314D65", "#AD492B"), c(21, 17), "RMSE (effect units)",
                        scale_y_continuous(breaks = seq(0, .6, .1)), FALSE)
  for (name in c("movement", "recovery")) {
    id <- if (name == "movement") "osm-figure-F.6" else "osm-figure-F.7"
    ggsave(file.path(out, paste0(id, ".pdf")), get(name), device = grDevices::pdf,
           useDingbats = FALSE, width = 6, height = 3.50, bg = "white")
    ggsave(file.path(out, paste0(id, ".png")), get(name), device = ragg::agg_png,
           width = 6, height = 3.50, dpi = 180, bg = "white")
  }
}
