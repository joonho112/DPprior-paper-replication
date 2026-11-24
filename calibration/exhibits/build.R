#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Build Tables 1, 2, E.1, G.1, Figures 1–3, G.1 and the executable OSM G.2 example.
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1L]
.script_dir <- dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE)))
source(file.path(.script_dir, "..", "..", "common", "R", "bootstrap.R"))
source(rp_path("exhibits", "helpers.R"))
setwd(.replication_root)
for (script in c("worked.R", "intervals.R")) {
  source(rp_path("calibration", "exhibits", script), local = new.env(parent = environment()))
}

questions <- data.frame(
  Quantity = c("E(K_J), Var(K_J)", "P(K_J = k)", "P(W_SB > 0.5), P(W_SB > 0.9)",
               "P(W_max > t)", "rho = sum(w_h^2)"),
  Question = c("How many clusters do I expect among these J units, and how sure am I?",
               "What is the chance of exactly k clusters?",
               "If I pick a unit at random, could it fall in a component holding more than half (more than 90%) of the mass?",
               "Could the largest component hold more than a fraction t of the mass?",
               "If I pick two units at random, will they share a mixture component?"),
  Report = c("About X clusters; 80% interval [L, U].", "Mode and central interval of K_J.",
             "X% chance of belonging to a majority-mass (near-universal-mass) population component.",
             "Bounds from Proposition 1 or the exact value.", "X% chance of same-cluster membership.")
)
ex_table("main-table-1", questions, sources = "calibration/exhibits/build.R",
         tier = "definition", note = "Partition and population-weight functionals in Table 1.")

confidence <- ex_read("calibration", "confidence_levels.csv", "data-frozen")[c("setting", "VIF", "target_variance", "a", "b")]
moments <- vapply(seq_len(nrow(confidence)), function(i) ex_moments(100, confidence$a[i], confidence$b[i]), numeric(2))
confidence$mean_K <- moments[1, ]
confidence$variance_K <- moments[2, ]
confidence$mean_alpha <- confidence$a / confidence$b
confidence$tail_05 <- ex_tail(confidence$a, confidence$b, .5)
confidence$tail_09 <- ex_tail(confidence$a, confidence$b, .9)
confidence$p_alpha_lt_01 <- pgamma(.1, shape = confidence$a, rate = confidence$b)
display <- data.frame(Setting = confidence$setting, "Target variance" = confidence$target_variance,
                      "Shape, rate" = sprintf("(%.2f, %.2f)", confidence$a, confidence$b),
                      "E(alpha)" = ex_f(confidence$mean_alpha, 2),
                      "P(W_SB > 0.5)" = ex_f(confidence$tail_05, 2),
                      "P(W_SB > 0.9)" = ex_f(confidence$tail_09, 2),
                      "P(alpha < 0.1)" = ex_f(confidence$p_alpha_lt_01, 3), check.names = FALSE)
ex_table("main-table-2", confidence, display, "data-frozen/calibration/confidence_levels.csv",
         tier = "recalculated", note = "Gamma shape and rate at full precision determine both count moments and the weight tails.")
e1 <- ex_read("calibration-examples", "intervals.csv")
roles <- c("count-calibrated" = "TSMM", "count-weight compromise" = "Dual-Anchor", "weight-bound sensitivity" = "Dual-Anchor (hard)")
display <- data.frame(Application = e1$application, Role = unname(roles[e1$prior_role]),
                      Mean = ex_f(e1$mean_count, 3), Variance = ex_f(e1$variance_count, 3),
                      "Stated-interval mass" = ex_f(e1$interval_mass, 3), check.names = FALSE)
ex_table("osm-table-E.1", e1, display, c("data-frozen/calibration-examples/application-priors.csv", "calibration/exhibits/intervals.R"),
         tier = "recalculated", note = "Count probabilities use each application's stated interval and the original unrounded shape and rate.")
g1 <- ex_read("calibration-examples", "worked-priors.csv")
display <- data.frame(Prior = g1$prior, a = ex_f(g1$a, 4), b = ex_f(g1$b, 4),
                      "E(K_50)" = ex_f(g1$E_K, 3), "Var(K_50)" = ex_f(g1$Var_K, 3),
                      "P(W_SB > 0.5)" = ex_f(g1$tail, 3), check.names = FALSE)
ex_table("osm-table-G.1", g1, display, c("data-derived/calibration-examples/worked-priors.csv", "data-frozen/calibration-examples/G2-example.R"),
         tier = "fresh G.2 calibration", note = "50 units, target count mean 5 and variance 10.")

for (script in c("figure1.R", "figure2.R", "figureG1.R")) {
  source(rp_path("calibration", "exhibits", script), local = new.env(parent = environment()))
}

# Compile the workflow diagram from its editable TeX source.
workflow <- rp_path("data-frozen", "calibration-examples", "workflow.tex")
scratch <- tempfile("workflow-")
dir.create(scratch)
draw_workflow <- function() {
  original_dir <- setwd(scratch)
  on.exit(setwd(original_dir))
  stopifnot(file.copy(workflow, "workflow.tex"))
  status <- system2("pdflatex", c("-interaction=nonstopmode", "-halt-on-error", "workflow.tex"), stdout = "typeset.log", stderr = "typeset-errors.log")
  if (status != 0L) stop(paste(readLines("typeset.log"), collapse = "\n"))
  stopifnot(system2("pdftocairo", c("-png", "-r", "180", "-singlefile", "workflow.pdf", "workflow")) == 0L)
}
draw_workflow()
files <- file.path(rp_path("outputs", "exhibits"), paste0("main-figure-3.", c("pdf", "png")))
stopifnot(all(file.copy(file.path(scratch, paste0("workflow.", c("pdf", "png"))), files, overwrite = TRUE)))
unlink(scratch, recursive = TRUE)
nodes <- grep("^\\\\node", readLines(workflow), value = TRUE)
titles <- regmatches(nodes, regexec("textbf\\{([1-6])\\. ([^}]+)\\}", nodes))
titles <- titles[lengths(titles) == 3L]
frame <- do.call(rbind, lapply(titles, function(x) data.frame(step = as.integer(x[2]), label = x[3])))
stopifnot(identical(frame$step, 1:6))
ex_meta("main-figure-3", files, frame, "data-frozen/calibration-examples/workflow.tex", "editable manuscript schematic",
        "Six reporting steps, a 0.40 decision threshold and a separate hard-bound sensitivity.")
