# Author: JoonHo Lee (jlee296@ua.edu)
# Accepted manuscript artwork, assembled from computed five-prior and paired-prior summaries.
draw_main_figure <- function(means, ranges, all_results, out) {
  library(grid)
  scratch <- tempfile("figure-4-")
  dir.create(scratch)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)
  priors <- c("Vague", "DORO-Unif", "TSMM", "Dual-Anchor", "SSI-Weight")
  design <- expand.grid(I_target = c(0.2, 0.5, 0.8), K_true = c(5, 10, 30))
  design <- merge(design, ranges, by = c("K_true","I_target"))
  design <- design[order(design$K_true, design$I_target), ]
  design$id <- seq_len(nrow(design))
  cells <- merge(means, design[c("K_true","I_target","id","n_all_five")],
                 by = c("K_true","I_target","n_all_five"))
  stopifnot(nrow(design) == 9, nrow(cells) == 45,
            all(table(cells$id, factor(cells$prior, levels = priors)) == 1))
  for (i in design$id) {
    d <- cells[cells$id == i, ]
    percent_range <- function(x) 100 * (max(x) - min(x)) / min(x)
    stopifnot(abs(percent_range(d$CB_ISEL) - design$CB_ISEL_range_pct[i]) < 1e-9,
              abs(percent_range(d$site_PM_RMSE) - design$site_PM_RMSE_range_pct[i]) < 1e-9)
  }

  width <- 6.0
  height <- 4.92
  ink <- "#17242C"
  muted <- "#45545D"
  light_rule <- "#DFE4E6"
  prior_col <- c("#343B40", "#966923", "#0072A4", "#BC4D25", "#00795D")
  prior_pch <- c(16, 0, 17, 5, 15)
  cb_col <- "#694682"
  rmse_col <- "#34434C"
  truth <- 0.25 * sqrt(99 / 100)
  a_domain <- c(.244, .33)
  a_span <- c(1.05, 3.39)
  b_span <- c(3.99, 5.60)
  a_x <- function(v) a_span[1] + diff(a_span) * (v - a_domain[1]) / diff(a_domain)
  b_x <- function(v) b_span[1] + diff(b_span) * v / 7
  row_y <- 0.83 + .353 * (design$id - 1) + .11 * ((design$id - 1) %/% 3)
  first_y <- row_y[1] - .17
  axis_y <- tail(row_y, 1) + .205
  xinch <- function(x) unit(x, "inches")
  yinch <- function(y) unit(height - y, "inches")
  label <- function(x, y, text, size = 8, colour = ink, just = "centre", face = "plain") {
    grid.text(text, xinch(x), yinch(y), just = c(just, "centre"),
              gp = gpar(col = colour, fontsize = size, fontface = face, fontfamily = "sans"))
  }
  line <- function(x0,y0,x1,y1,colour=light_rule,lwd=.6,lty=1) {
    grid.segments(xinch(x0), yinch(y0), xinch(x1), yinch(y1),
                  gp = gpar(col = colour, lwd = lwd, lty = lty, lineend = "butt"))
  }
  symbol <- function(x,y,pch,colour,size=4.5) {
    grid.points(xinch(x), yinch(y), pch=pch, size=unit(size,"points"),
                gp=gpar(col=colour,fill=if (pch==21) "white" else colour,lwd=.95))
  }
  draw_figure <- function() {
    grid.newpage()
    label(a_span[1]-.04,.145,"A   Spread of site effects",13,just="left",face="bold")
    label(b_span[1]-.34,.145,"B   Sensitivity to the prior",13,just="left",face="bold")
    label(a_span[1]-.04,.385,"Posterior mean of the finite-site SD",9.5,muted,"left")
    label(b_span[1]-.34,.385,"Relative range of mean error (%)",9.5,muted,"left")
    label(.145,.59,expression(italic(K)^"*"),10)
    label(.47,.59,expression(italic(I)),10)
    label(.82,.59,expression(italic(n)),10)
    for (j in c(3L,6L)) {
      middle <- (row_y[j] + row_y[j+1L]) / 2
      line(.04,middle,5.96,middle,"#D1D9DC",.65)
    }
    a_ticks <- c(.25,.27,.29,.31,.33)
    b_ticks <- c(0,2,4,6)
    line(a_x(a_ticks),first_y,a_x(a_ticks),axis_y,"#E8ECED",.55)
    line(b_x(b_ticks),first_y,b_x(b_ticks),axis_y,"#E8ECED",.55)
    line(a_x(truth),first_y,a_x(truth),axis_y,"#606D76",1.15,"42")
    label(.145,row_y[c(2,5,8)],c("5","10","30"),10.5,face="bold")
    label(.47,row_y,sprintf("%.1f", design$I_target),9.5)
    label(.82,row_y,as.character(design$n_all_five),9.5,muted)
    # A small vertical offset separates priors at nearly identical SDs.
    offsets <- c(-.13,-.065,0,.065,.13)
    for (p in seq_along(priors)) {
      d <- cells[cells$prior == priors[p], ]
      symbol(a_x(d$posterior_mean_sd),row_y[d$id]+offsets[p],prior_pch[p],prior_col[p],6.1)
    }
    rmse <- design$site_PM_RMSE_range_pct
    cb <- design$CB_ISEL_range_pct
    line(b_x(rmse),row_y,b_x(cb),row_y,"#B9B1C2",1.7)
    symbol(b_x(rmse),row_y,21,rmse_col,7.7)
    symbol(b_x(cb),row_y,18,cb_col,9.8)
    label(b_x(rmse)-.087,row_y,sprintf("%.2f",rmse),9.5,rmse_col,"right")
    label(b_x(cb)+.098,row_y,sprintf("%.2f",cb),9.5,cb_col,"left")
    line(a_span[1],axis_y,a_span[2],axis_y,"#97A1A5",.7)
    line(b_span[1],axis_y,b_span[2],axis_y,"#97A1A5",.7)
    line(a_x(a_ticks),axis_y,a_x(a_ticks),axis_y+.045,"#7C898F",.7)
    line(b_x(b_ticks),axis_y,b_x(b_ticks),axis_y+.045,"#7C898F",.7)
    label(a_x(a_ticks),axis_y+.145,sprintf("%.2f",a_ticks),9.5,muted)
    label(b_x(b_ticks),axis_y+.145,as.character(b_ticks),9.5,muted)
    # Figure keys are kept with their respective panels.
    key_x <- c(.14,.14,1.29,1.29,2.60)
    key_y <- c(4.50,4.74,4.50,4.74,4.50)
    for (p in seq_along(priors)) {
      symbol(key_x[p],key_y[p],prior_pch[p],prior_col[p],7.7)
      label(key_x[p]+.105,key_y[p],priors[p],9.5,just="left")
    }
    line(2.50,4.74,2.69,4.74,"#606D76",1.15,"42")
    label(2.73,4.74,"True SD",9.5,just="left")
    symbol(3.92,4.50,18,cb_col,10.3)
    label(4.05,4.50,"CB distribution (ISEL)",9.5,just="left")
    symbol(3.92,4.74,21,rmse_col,8.2)
    label(4.05,4.74,"Site effects (RMSE)",9.5,just="left")
  }
  pdf(file.path(scratch, "panel-ab.pdf"), width=width, height=height, family="Helvetica", useDingbats=FALSE)
  draw_figure()
  invisible(dev.off())

  counts <- c(5L, 10L, 30L)
  info <- c(.2, .5, .8)
  ranks <- c(50, 95, 99)
  cells <- subset(all_results, percentile %in% ranks)
  stopifnot(identical(sort(unique(cells$K_true)), counts),
            identical(sort(unique(cells$I_target)), info),
            all(xtabs(~ K_true + I_target + percentile, cells) == 1),
            all(is.finite(cells$mean_abs_shift)),
            all(cells$mean_abs_shift >= abs(cells$mean_shift) - 1e-12),
            all(cells$mean_abs_shift <= .04))
  n <- unique(cells[c("K_true", "I_target", "n")])
  stopifnot(nrow(n) == 9, sum(n$n) == 1681)

  ink <- "#17242C"
  muted <- "#45545D"
  cols <- c("#34434C", "#00796C", "#AD492B")
  styles <- c("32", "51", "solid")
  width <- 6
  height <- 2.86
  inch <- function(x) unit(x, "inches")
  ypos <- function(y) inch(height - y)
  txt <- function(x, y, label, size, colour = ink, just = "centre", bold = FALSE) {
    grid.text(label, inch(x), ypos(y), just = c(just, "centre"),
              gp = gpar(col = colour, fontsize = size, fontfamily = "Helvetica",
                        fontface = if (bold) "bold" else "plain"))
  }
  seg <- function(x0, y0, x1, y1, colour, weight, style = "solid") {
    grid.segments(inch(x0), ypos(y0), inch(x1), ypos(y1),
                  gp = gpar(col = colour, lwd = weight, lty = style, lineend = "butt"))
  }
  mark <- function(x, y, j) {
    # The open circle remains visible around the small triangle where estimates coincide.
    grid.points(inch(x), ypos(y), pch = c(21, 17, 18)[j],
                size = unit(c(9.9, 6.8, 8.4)[j], "points"),
                gp = gpar(col = cols[j], fill = if (j == 1) "white" else cols[j],
                          lwd = 1.15))
  }
  draw_c <- function() {
    grid.newpage()
    txt(.045, .17, "C   Change in CB percentiles", 13, just = "left", bold = TRUE)
    txt(.045, .40, "Mean absolute change from Gamma(1, 1) to Dual-Anchor (effect units)",
        9.5, muted, "left")
    key_x <- c(3.16, 4.35, 5.23)
    for (j in seq_along(ranks)) {
      seg(key_x[j], .17, key_x[j] + .245, .17, cols[j], 1.5, styles[j])
      mark(key_x[j] + .1225, .17, j)
      txt(key_x[j] + .30, .17, c("Median", "95th", "99th")[j], 9.5, just = "left")
    }
    top <- .90
    bottom <- 2.035
    ticks <- seq(0, .04, by = .01)
    y <- function(s) bottom - (bottom - top) * s / .04
    starts <- c(.49, 2.37, 4.25)
    span <- 1.60
    for (k in seq_along(counts)) {
      left <- starts[k]
      right <- left + span
      x <- left + c(.09, span / 2, span - .09)
      txt(left + span / 2, .70, bquote(italic(K)^"*" == .(counts[k])), 10.5)
      seg(left, y(ticks), right, y(ticks), "#DFE4E6", .70)
      seg(left, bottom, right, bottom, "#98A3A7", .80)
      if (k == 1) txt(left - .08, y(ticks), sprintf("%.2f", ticks), 9.5, muted, "right")
      seg(x, bottom, x, bottom + .045, "#7C898F", .7)
      txt(x, bottom + .15, formatC(info, format = "f", digits = 1), 9.5, muted)
      series <- lapply(ranks, function(rank) {
        d <- subset(cells, K_true == counts[k] & percentile == rank)
        y(d$mean_abs_shift[match(info, d$I_target)])
      })
      for (j in seq_along(ranks)) {
        v <- series[[j]]
        seg(head(x, -1), head(v, -1), tail(x, -1), tail(v, -1), cols[j], 1.5, styles[j])
      }
      for (j in seq_along(ranks)) mark(x, series[[j]], j)
    }
    txt(3.14, 2.39, expression("Site informativeness, " * italic(I)), 10)
    txt(.045, 2.56, "A and B require all five fits to converge.", 9, muted, "left")
    txt(.045, 2.74, sprintf("C requires both fits to converge (%d to %d replicates per design).",
                            min(n$n), max(n$n)), 9, muted, "left")
  }

  c_pdf <- file.path(scratch, "panel-c.pdf")
  pdf(c_pdf, width = width, height = height, family = "Helvetica", useDingbats = FALSE,
      title = "Prior sensitivity at three percentiles of the finite-site effect distribution")
  draw_c()
  invisible(dev.off())

  # Assemble the two vector strips using the same media box as the 085 artwork.
  tex <- c("\\documentclass[border=0pt]{standalone}", "\\usepackage{graphicx}",
           "\\begin{document}",
           "\\vbox{\\offinterlineskip\\hbox{\\includegraphics[width=432bp]{panel-ab.pdf}}%",
           "\\hbox{\\includegraphics[width=432bp]{panel-c.pdf}}}%",
           "\\end{document}")
  writeLines(tex, file.path(scratch, "figure-4-abc.tex"))
  run <- function(command, args) {
    if (Sys.which(command) == "") stop("Missing tool: ", command)
    log <- system2(command, args, stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(log, "status"))) stop(paste(log, collapse = "\n"))
  }
  old_dir <- setwd(scratch)
  on.exit(setwd(old_dir), add = TRUE, after = FALSE)
  run("pdflatex", c("-interaction=nonstopmode", "-halt-on-error", "figure-4-abc.tex"))
  run("pdftocairo", c("-png", "-singlefile", "-r", "180", "figure-4-abc.pdf", "figure-4"))
  stopifnot(file.copy("figure-4-abc.pdf", file.path(out,"main-figure-4.pdf"), overwrite=TRUE),
            file.copy("figure-4.png", file.path(out,"main-figure-4.png"), overwrite=TRUE))
}
