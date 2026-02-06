# ==============================================================================
# plot_fig3.R — Visualization functions for Figure 3 simulation results
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Paper:    "Design-Conditional Prior Elicitation for DP Mixtures"
# Author:   JoonHo Lee
#
# Description:
#   Creates the Figure 3 faceted grid: rows = K_true (5, 10, 30),
#   columns = I_target (0.2, 0.5, 0.8). Each panel shows boxplots of
#   posterior E[K_J | data] across 200 replications.
#
# Functions:
#   theme_DPprior()                  — Publication-quality ggplot2 theme
#   make_method_label()              — Map internal method names to display labels
#   filter_by_confidence()           — Filter data by confidence level
#   plot_fig3_generic()              — Core plotting function
#   plot_fig3_main()                 — Main figure (Vague/TSMM/Dual)
#   plot_fig3_extended_baselines()   — Extended figure with all 5 methods
#   plot_fig3_conf_sensitivity()     — Confidence sensitivity panels
#
# Dependencies: ggplot2
# Called by:    scripts/step4_plot_figures.R
# ==============================================================================

theme_DPprior <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position  = "bottom",
      strip.text       = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank()
    )
}

make_method_label <- function(method) {
  out <- method
  out[method == "vague"]        <- "Vague"
  out[method == "tsmm"]         <- "TSMM"
  out[method == "dual"]         <- "Dual\nanchor"
  out[method == "doro_uniform"] <- "DORO\n(unif.)"
  out[method == "chi2_doro"]    <- "Chi2\nDORO"
  out
}

filter_by_confidence <- function(df, confidence_tsmm_dual = "medium") {
  keep <- (df$method %in% c("vague", "doro_uniform", "chi2_doro") & df$confidence == "none") |
          (df$method %in% c("tsmm", "dual") & df$confidence == confidence_tsmm_dual)
  df[keep, , drop = FALSE]
}

plot_fig3_generic <- function(raw_df, methods_keep, confidence_tsmm_dual,
                              out_pdf, out_png,
                              title = "Figure 3. Simulation results across informativeness") {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required. Install with install.packages('ggplot2').")
  }

  df <- raw_df
  if ("status" %in% names(df)) df <- df[df$status == "ok", , drop = FALSE]
  df <- filter_by_confidence(df, confidence_tsmm_dual = confidence_tsmm_dual)
  df <- df[df$method %in% methods_keep, , drop = FALSE]

  df$method_label <- make_method_label(df$method)
  df$I_target_f   <- factor(df$I_target, levels = sort(unique(df$I_target)))
  df$K_true_f     <- factor(df$K_true, levels = sort(unique(df$K_true)))
  df$method_label <- factor(df$method_label, levels = unique(make_method_label(methods_keep)))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = method_label, y = post_EK)) +
    ggplot2::geom_boxplot(outlier.size = 0.6) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = K_true), linetype = "dashed") +
    ggplot2::facet_grid(
      K_true_f ~ I_target_f,
      labeller = ggplot2::labeller(
        K_true_f   = function(x) paste0("K* = ", x),
        I_target_f = function(x) paste0("I = ", x)
      )
    ) +
    ggplot2::labs(x = "Method", y = "Posterior E[K_J | data]", title = title) +
    theme_DPprior()

  n_methods  <- length(unique(df$method_label))
  plot_width <- if (n_methods > 3) 10 else 9
  if (n_methods > 3) {
    p <- p + ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 20, hjust = 1, vjust = 1)
    )
  }

  dir.create(dirname(out_pdf), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(out_png), recursive = TRUE, showWarnings = FALSE)

  ggplot2::ggsave(out_pdf, p, width = plot_width, height = 7, units = "in")
  ggplot2::ggsave(out_png, p, width = plot_width, height = 7, units = "in", dpi = 300)
  invisible(p)
}

plot_fig3_main <- function(raw_df, cfg, out_pdf, out_png) {
  confidence   <- as.character(cfg$plot$main_confidence)
  methods_keep <- as.character(cfg$plot$main_methods)
  plot_fig3_generic(
    raw_df, methods_keep = methods_keep, confidence_tsmm_dual = confidence,
    out_pdf = out_pdf, out_png = out_png,
    title = "Figure 3. Simulation results across informativeness (main)"
  )
}

plot_fig3_extended_baselines <- function(raw_df, cfg, out_pdf, out_png) {
  confidence   <- as.character(cfg$plot$main_confidence)
  methods_keep <- as.character(cfg$plot$extended_methods)
  plot_fig3_generic(
    raw_df, methods_keep = methods_keep, confidence_tsmm_dual = confidence,
    out_pdf = out_pdf, out_png = out_png,
    title = "Figure 3 (extended). Including baseline alpha-prior specifications"
  )
}

plot_fig3_conf_sensitivity <- function(raw_df, cfg, confidence_level, out_pdf, out_png) {
  methods_keep <- as.character(cfg$plot$main_methods)
  plot_fig3_generic(
    raw_df, methods_keep = methods_keep, confidence_tsmm_dual = confidence_level,
    out_pdf = out_pdf, out_png = out_png,
    title = paste0("Figure 3 (sensitivity). TSMM/Dual confidence = ", confidence_level)
  )
}
