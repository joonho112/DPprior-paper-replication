# Author: JoonHo Lee (jlee296@ua.edu)
# Use consistent publication graphics
#
# Share role labels, colours, fonts and output devices across the figures. The
# palette and device routines are adapted from the locked manuscript; local
# font probing supplies a usable installed font.
#
# Sourced by exhibits/helpers.R.
# save_figure_v3() returns the PDF and PNG paths used in exhibit receipts.
# Legacy v3 function names are retained so existing calls remain compatible.

suppressPackageStartupMessages({
  library(ggplot2)
  library(grid)
})
V3_FIG_DIR <- rp_path('outputs', 'figures')
# Font: the first installed family in the candidate list. Helvetica is the
# family used by v0 Figure 1 and by the v2 cairo renders (embedded as a
# TrueType subset by cairo_pdf on macOS). Arial is the cross-platform fallback.
# ---------------------------------------------------------------------------
font_v3 <- function(candidates = c(
                      "Helvetica", "Arial", "Helvetica Neue",
                      "Liberation Sans", "DejaVu Sans"
                    )) {
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    fams <- unique(systemfonts::system_fonts()$family)
    hit <- candidates[candidates %in% fams]
    if (length(hit)) return(hit[[1L]])
  }
  "sans"
}
v3_font <- font_v3()
# Point sizes used throughout (print points, figure rendered at print width).
v3_sizes <- list(
  base = 10, # axis titles, panel labels (A./B. headers, strips)
  axis_text = 9, # minimum axis text at print size
  subtitle = 9, # one-line grey subtitle under a panel header
  legend = 9,
  geom_text_min = 8 # in-panel annotations, never below this
)
# geom_text()/annotate() take sizes in mm; convert print points to that unit.
pt_size <- function(points) points / ggplot2::.pt
# ---------------------------------------------------------------------------
# Role palette (Okabe-Ito), with shape and linetype redundancy.
# Semantics: solid = calibrated primary role, dashed = external reference,
# dotted = sensitivity-only, dot-dash = literature default, "22" = neutral
# reference. Colours for the four v0 roles are the v0 Figure 3 assignments.
# ---------------------------------------------------------------------------
okabe_ito <- c(
  black = "#000000", orange = "#E69F00", skyblue = "#56B4E9",
  green = "#009E73", yellow = "#F0E442", blue = "#0072B2",
  vermillion = "#D55E00", purple = "#CC79A7", grey = "#999999"
)
palette_roles <- c(
  "Vague"              = "#E69F00", # orange
  "DORO-Unif"          = "#56B4E9", # sky blue (external reference)
  "TSMM"               = "#009E73", # bluish green
  "Dual-Anchor"        = "#CC79A7", # reddish purple
  "Dual-Anchor (hard)" = "#D55E00", # vermillion (sensitivity only)
  "SSI-Weight"         = "#0072B2", # blue
  "Literature default" = "#000000", # black
  "Neutral"            = "#999999" # grey: K* targets, benchmarks, context
)
shape_roles <- c(
  "Vague"              = 16, # filled circle
  "DORO-Unif"          = 2, # open triangle
  "TSMM"               = 15, # filled square
  "Dual-Anchor"        = 17, # filled triangle
  "Dual-Anchor (hard)" = 0, # open square
  "SSI-Weight"         = 18, # filled diamond
  "Literature default" = 4, # cross
  "Neutral"            = 1 # open circle
)
linetype_roles <- c(
  "Vague"              = "solid",
  "DORO-Unif"          = "dashed",
  "TSMM"               = "solid",
  "Dual-Anchor"        = "solid",
  "Dual-Anchor (hard)" = "dotted",
  "SSI-Weight"         = "solid",
  "Literature default" = "dotdash",
  "Neutral"            = "22"
)
# Secondary palette for the defaults-by-J figure, where several literature
# defaults share one panel: black plus greys, separated by linetype.
palette_literature <- c("#000000", "#4D4D4D", "#8C8C8C", "#B5B5B5")
linetype_literature <- c("solid", "dashed", "dotted", "dotdash")
role_levels <- names(palette_roles)
# Map the raw labels found in the frozen CSVs (prior, prior_label,
# display_label, abbreviated axis labels) onto the canonical role names.
canonical_role <- function(x, extra_levels = character()) {
  x <- as.character(x)
  map <- c(
    "Dual-Soft-Selected"    = "Dual-Anchor",
    "Dual"                  = "Dual-Anchor",
    "Dual-Hard-Sensitivity" = "Dual-Anchor (hard)",
    "Dual (hard)"           = "Dual-Anchor (hard)",
    "DORO"                  = "DORO-Unif",
    "DORO*"                 = "DORO-Unif",
    "DORO-Unif*"            = "DORO-Unif",
    "SSI"                   = "SSI-Weight",
    "Default"               = "Literature default",
    "Reference"             = "Neutral"
  )
  hit <- x %in% names(map)
  x[hit] <- unname(map[x[hit]])
  levels <- c(role_levels, extra_levels)
  unknown <- setdiff(unique(x[!is.na(x)]), levels)
  if (length(unknown)) {
    stop("canonical_role(): unknown role label(s): ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  factor(x, levels = levels)
}
scale_colour_roles <- function(..., name = NULL) {
  ggplot2::scale_colour_manual(values = palette_roles, name = name, ...)
}
scale_color_roles <- scale_colour_roles
scale_fill_roles <- function(..., name = NULL) {
  ggplot2::scale_fill_manual(values = palette_roles, name = name, ...)
}
scale_shape_roles <- function(..., name = NULL) {
  ggplot2::scale_shape_manual(values = shape_roles, name = name, ...)
}
scale_linetype_roles <- function(..., name = NULL) {
  ggplot2::scale_linetype_manual(values = linetype_roles, name = name, ...)
}
# ---------------------------------------------------------------------------
# Theme
# ---------------------------------------------------------------------------
theme_v3 <- function(base_size = 10,
                     base_family = v3_font,
                     grid = c("y", "xy", "none"),
                     border = FALSE,
                     legend = c("bottom", "inside", "right", "none"),
                     markdown_strips = FALSE) {
  grid <- match.arg(grid)
  legend <- match.arg(legend)
  ink <- "#1F2328"
  muted <- "#5B6167"
  rule <- "#B7BCC1"
  gridline <- "#E3E6E8"
  th <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = ink),
      # Two-line panel header (v0 style): bold title, grey one-line subtitle.
      plot.title = ggplot2::element_text(
        face = "bold", size = base_size,
        hjust = 0, margin = ggplot2::margin(b = 2)
      ),
      plot.subtitle = ggplot2::element_text(
        colour = muted, size = base_size - 1,
        hjust = 0, margin = ggplot2::margin(b = 6)
      ),
      plot.title.position = "plot",
      # plot.caption is styled but must stay unused in main-text figures.
      plot.caption = ggplot2::element_text(
        colour = muted, size = base_size - 1.5,
        hjust = 0, margin = ggplot2::margin(t = 6)
      ),
      plot.caption.position = "plot",
      axis.title = ggplot2::element_text(size = base_size),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 4)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 4)),
      axis.text = ggplot2::element_text(size = base_size - 1, colour = "#3B4045"),
      axis.ticks = ggplot2::element_line(colour = rule, linewidth = 0.3),
      axis.ticks.length = grid::unit(2, "pt"),
      axis.line = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = gridline, linewidth = 0.3),
      panel.spacing = grid::unit(8, "pt"),
      # Naked strips: no grey box, bold left-aligned text.
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(
        face = "bold", size = base_size, hjust = 0,
        margin = ggplot2::margin(t = 2, b = 4)
      ),
      strip.text.y = ggplot2::element_text(
        angle = -90, hjust = 0.5,
        margin = ggplot2::margin(l = 4, r = 2)
      ),
      strip.placement = "outside",
      legend.title = ggplot2::element_text(size = base_size - 1),
      legend.text = ggplot2::element_text(size = base_size - 1),
      legend.key.size = grid::unit(10, "pt"),
      legend.key.width = grid::unit(16, "pt"),
      legend.margin = ggplot2::margin(t = 0, b = 0),
      legend.box.spacing = grid::unit(4, "pt"),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.margin = ggplot2::margin(4, 6, 4, 4)
    )
  if (grid == "y") th <- th + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  if (grid == "none") th <- th + ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  if (isTRUE(border)) {
    th <- th + ggplot2::theme(
      panel.border = ggplot2::element_rect(colour = rule, fill = NA, linewidth = 0.3)
    )
  }
  th <- th + switch(legend,
    bottom = ggplot2::theme(
      legend.position = "bottom",
      legend.direction = "horizontal"
    ),
    inside = ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = c(0.98, 0.98),
      legend.justification.inside = c(1, 1),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA)
    ),
    right = ggplot2::theme(legend.position = "right"),
    none = ggplot2::theme(legend.position = "none")
  )
  if (isTRUE(markdown_strips)) {
    if (!requireNamespace("ggtext", quietly = TRUE)) {
      stop("markdown_strips = TRUE needs the ggtext package", call. = FALSE)
    }
    th <- th + ggplot2::theme(
      strip.text = ggtext::element_markdown(
        size = base_size,
        hjust = 0,
        lineheight = 1.15,
        margin = ggplot2::margin(t = 2, b = 4)
      )
    )
  }
  th
}
# ---------------------------------------------------------------------------
# Panel headers (v0 style): "A. The Cluster View (K_J)" in bold, one-line grey
# subtitle underneath. Two ways to place them:
#   (1) patchwork: each panel is its own ggplot; add labs_panel(panel_title(...))
#       to each and combine with patchwork::wrap_plots(). Preferred.
#   (2) facets: use panel_title(...)$strip as the facet label (plain two-line
#       strip) or panel_title(...)$markdown with theme_v3(markdown_strips = TRUE)
#       to keep the bold/grey contrast inside a strip.
# ---------------------------------------------------------------------------
panel_title <- function(letter, title, subtitle = NULL, title_expr = NULL) {
  header <- paste0(letter, ". ", title)
  markdown <- paste0("**", header, "**")
  if (!is.null(subtitle) && nzchar(subtitle)) {
    markdown <- paste0(
      markdown,
      "<br><span style='color:#5B6167;font-weight:normal;font-size:9pt'>",
      subtitle, "</span>"
    )
  }
  # title_expr: optional plotmath call for the header (subscripts, K^"*"),
  # e.g. quote(paste("Posterior mean of ", K[J])). The letter prefix is
  # juxtaposed in front of it and the whole header is wrapped in bold(),
  # because theme(face = "bold") does not reach plotmath expressions.
  # The string `title` still feeds strip/markdown.
  title_plot <- if (is.null(title_expr)) header else
    bquote(bold(.(paste0(letter, ". ")) * .(title_expr)))
  structure(
    list(
      letter = letter, title = title_plot, title_string = header,
      subtitle = subtitle,
      strip = if (is.null(subtitle)) header else paste0(header, "\n", subtitle),
      markdown = markdown
    ),
    class = "v3_panel_title"
  )
}
labs_panel <- function(pt, ...) {
  stopifnot(inherits(pt, "v3_panel_title"))
  ggplot2::labs(title = pt$title, subtitle = pt$subtitle, ...)
}
# Labeller for facet strips built from a named list of panel_title objects
# (names = facet values), using the plain or markdown two-line form.
strip_labeller_v3 <- function(titles, markdown = FALSE) {
  stopifnot(is.list(titles), !is.null(names(titles)))
  lookup <- vapply(titles, function(pt) if (markdown) pt$markdown else pt$strip, "")
  ggplot2::as_labeller(lookup)
}
# ---------------------------------------------------------------------------
# Reference lines
#   target   : generating reference K* (dashed, near-black, thin)
#   zero     : zero / no-difference line (solid, near-black, thin)
#   guide    : secondary threshold, e.g. 0.5 dominance guide (dotted, grey)
#   diagonal : identity line y = x (dotted, grey)
# Supply a constant (yintercept / xintercept) or data + mapping for values
# that vary by facet, e.g. reference_line(data = cells, mapping = aes(yintercept = K_true)).
# ---------------------------------------------------------------------------
reference_line <- function(yintercept = NULL, xintercept = NULL,
                           kind = c("target", "zero", "guide", "diagonal"),
                           data = NULL, mapping = NULL, ...) {
  kind <- match.arg(kind)
  style <- switch(kind,
    target   = list(colour = "#3B4045", linewidth = 0.4, linetype = "22"),
    zero     = list(colour = "#3B4045", linewidth = 0.3, linetype = "solid"),
    guide    = list(colour = "#9AA0A6", linewidth = 0.35, linetype = "13"),
    diagonal = list(colour = "#9AA0A6", linewidth = 0.35, linetype = "13")
  )
  extra <- list(...)
  if (kind == "diagonal") {
    return(do.call(
      ggplot2::geom_abline,
      c(list(intercept = 0, slope = 1), style, extra)
    ))
  }
  if (!is.null(mapping)) {
    geom <- if ("yintercept" %in% names(mapping)) ggplot2::geom_hline else ggplot2::geom_vline
    return(do.call(geom, c(list(data = data, mapping = mapping), style, extra)))
  }
  if (!is.null(yintercept)) {
    return(do.call(ggplot2::geom_hline, c(list(yintercept = yintercept), style, extra)))
  }
  if (!is.null(xintercept)) {
    return(do.call(ggplot2::geom_vline, c(list(xintercept = xintercept), style, extra)))
  }
  stop("reference_line(): supply yintercept, xintercept, or data + mapping",
    call. = FALSE
  )
}
# ---------------------------------------------------------------------------
# Font embedding report (pdffonts) and the pdftocairo embedding pass.
# pdffonts columns end with: emb sub uni objectID gen; we read emb = NF-4
# The following field, NF-3, reports subsetting rather than embedding.
# ---------------------------------------------------------------------------
pdf_font_report <- function(path) {
  if (!nzchar(Sys.which("pdffonts"))) {
    return(list(
      available = FALSE, unembedded = NA_integer_, fonts = character(),
      raw = "pdffonts not found on PATH"
    ))
  }
  raw <- suppressWarnings(system2("pdffonts", shQuote(path), stdout = TRUE, stderr = TRUE))
  body <- raw[-seq_len(min(2L, length(raw)))]
  body <- body[nzchar(trimws(body))]
  emb <- vapply(strsplit(trimws(body), "[[:space:]]+"), function(f) {
    if (length(f) < 5L) return(NA_character_)
    f[[length(f) - 4L]]
  }, "")
  names_ <- vapply(strsplit(trimws(body), "[[:space:]]+"), `[[`, "", 1L)
  list(
    available = TRUE,
    unembedded = sum(emb == "no", na.rm = TRUE),
    fonts = setNames(emb, names_),
    raw = raw
  )
}
embed_fonts_pdftocairo <- function(path) {
  if (!nzchar(Sys.which("pdftocairo"))) {
    warning("pdftocairo not found; cannot embed fonts in ", basename(path))
    return(FALSE)
  }
  pages_of <- function(p) {
    info <- suppressWarnings(system2("pdfinfo", shQuote(p), stdout = TRUE))
    as.integer(sub("^Pages:[[:space:]]+", "", grep("^Pages:", info, value = TRUE)))
  }
  tmp <- paste0(path, ".tmp")
  on.exit(if (file.exists(tmp)) unlink(tmp), add = TRUE)
  status <- system2("pdftocairo", c("-pdf", shQuote(path), shQuote(tmp)))
  if (status != 0L || !file.exists(tmp)) return(FALSE)
  ok <- identical(pages_of(path), pages_of(tmp)) &&
    isTRUE(pdf_font_report(tmp)$unembedded == 0L)
  if (ok) file.rename(tmp, path)
  ok
}
# ---------------------------------------------------------------------------
# PDF device chain. capabilities("cairo") can be TRUE while the cairo DLL
# fails to load (R 4.6.0 on this Mac links grDevices/libs/cairo.so against
# XQuartz libraries that are not installed), so the device is tested by
# actually opening it once. Order of preference:
#   1. cairo_pdf  : embeds the font as a TrueType subset with a ToUnicode map;
#   2. quartz pdf : macOS Core Graphics; embeds the font as a TrueType subset
#                   (pdffonts: emb yes, uni no), handles plotmath and Unicode;
#   3. base pdf() : writes unembedded base-14 Helvetica; the pdftocairo pass in
#                   save_figure_v3() then embeds it.
# ---------------------------------------------------------------------------
pdf_device_v3 <- local({
  cached <- NULL
  function(refresh = FALSE) {
    if (!is.null(cached) && !refresh) return(cached)
    probe <- function(open) {
      f <- tempfile(fileext = ".pdf")
      on.exit(unlink(f), add = TRUE)
      ok <- tryCatch(
        {
          withCallingHandlers(
            {
              open(f)
              grDevices::dev.off()
            },
            warning = function(w) stop(conditionMessage(w), call. = FALSE)
          )
          file.exists(f) && isTRUE(file.info(f)$size > 0)
        },
        error = function(e) FALSE
      )
      isTRUE(ok)
    }
    cairo_ok <- isTRUE(capabilities("cairo")) &&
      probe(function(f) grDevices::cairo_pdf(f, width = 2, height = 2))
    quartz_ok <- !cairo_ok && .Platform$OS.type == "unix" &&
      isTRUE(capabilities("aqua")) &&
      probe(function(f) grDevices::quartz(file = f, type = "pdf", width = 2, height = 2))
    cached <<- if (cairo_ok) {
      list(
        name = "cairo_pdf",
        device = function(filename, width, height, family = v3_font, ...)
          grDevices::cairo_pdf(filename,
            width = width, height = height,
            family = family, ...
          )
      )
    } else if (quartz_ok) {
      list(
        name = "quartz_pdf",
        device = function(filename, width, height, family = v3_font, ...)
          grDevices::quartz(
            file = filename, type = "pdf", width = width,
            height = height, family = family, ...
          )
      )
    } else {
      list(
        name = "base_pdf",
        device = function(filename, width, height, family = v3_font, ...)
          grDevices::pdf(filename,
            width = width, height = height,
            family = family, ...
          )
      )
    }
    cached
  }
})
# ---------------------------------------------------------------------------
# save_figure_v3(): PDF via the device chain above + PNG at 300 dpi (ragg,
# else cairo png). Every PDF is checked with pdffonts; if any font is left
# unembedded the pdftocairo pass replaces the file. Returns
# c(pdf = ..., png = ...) with attributes "fonts" (pdffonts report) and
# "device" (the PDF device actually used).
# ---------------------------------------------------------------------------
save_figure_v3 <- function(plot, name, width_in = 6.5, height_in = 4,
                           dir = V3_FIG_DIR, dpi = 300, family = v3_font,
                           embed_fonts = TRUE, verbose = TRUE) {
  stopifnot(is.character(name), length(name) == 1L, nzchar(name))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  pdf_path <- file.path(dir, paste0(name, ".pdf"))
  png_path <- file.path(dir, paste0(name, ".png"))
  dev <- pdf_device_v3()
  ggplot2::ggsave(pdf_path,
    plot = plot, device = dev$device,
    width = width_in, height = height_in, units = "in", family = family
  )
  if (dev$name == "base_pdf" && !isTRUE(embed_fonts)) {
    warning("save_figure_v3(): base pdf() device used with embed_fonts = FALSE; ",
      "fonts in ", basename(pdf_path), " are not embedded",
      call. = FALSE
    )
  }
  if (requireNamespace("ragg", quietly = TRUE)) {
    ggplot2::ggsave(png_path,
      plot = plot, device = ragg::agg_png,
      width = width_in, height = height_in, units = "in", dpi = dpi
    )
  } else {
    ggplot2::ggsave(png_path,
      plot = plot, device = grDevices::png, type = "cairo",
      width = width_in, height = height_in, units = "in", dpi = dpi
    )
  }
  for (p in c(pdf_path, png_path)) {
    if (!file.exists(p) || file.info(p)$size <= 0) {
      stop("save_figure_v3(): ", basename(p), " was not written", call. = FALSE)
    }
  }
  fonts <- pdf_font_report(pdf_path)
  if (isTRUE(embed_fonts) && isTRUE(fonts$available) && fonts$unembedded > 0L) {
    if (verbose) message(
      "save_figure_v3(): ", fonts$unembedded,
      " unembedded font(s) in ", basename(pdf_path),
      "; running pdftocairo embedding pass"
    )
    if (embed_fonts_pdftocairo(pdf_path)) fonts <- pdf_font_report(pdf_path)
  }
  if (verbose) {
    status <- if (!isTRUE(fonts$available)) "pdffonts unavailable" else
      sprintf("%d unembedded font(s)", fonts$unembedded)
    message(sprintf(
      "save_figure_v3(): wrote %s (%.2f x %.2f in; %s; %s) and %s (%d dpi)",
      basename(pdf_path), width_in, height_in, dev$name, status,
      basename(png_path), dpi
    ))
  }
  invisible(structure(c(pdf = pdf_path, png = png_path),
    fonts = fonts,
    device = dev$name
  ))
}
# ---------------------------------------------------------------------------
