#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Execute the 104 OSM G.2 listing verbatim and record its full frontier.
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
.script_dir <- dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE)))
source(file.path(.script_dir, "..", "..", "common", "R", "bootstrap.R"))
setwd(.replication_root)
stopifnot(as.character(packageVersion("DPprior")) == "2.0.0")
source("data-frozen/methods104/G2-example.R", local = TRUE)
stopifnot(nrow(selected$curve) == 29L, identical(selected$selected_lambda, 0.3),
          selected$selected_lambda == max(selected$curve$lambda[selected$curve$soft_policy_eligible]),
          all(selected$curve$verified), hard$gate$ok)
fits <- list(TSMM = fit, "Dual-Anchor" = primary, "Dual-Anchor (hard)" = hard$fit)
rows <- lapply(names(fits), function(role) {
  f <- fits[[role]]
  a <- f$parameters$a
  b <- f$parameters$b
  moments <- DPprior::K_moments(spec$J, a, b, M = 160L, M_verify = 320L, strict = TRUE)
  data.frame(prior = role, a = a, b = b, E_K = moments[["mean"]], Var_K = moments[["var"]],
             tail = DPprior::prob_wsb_exceeds(.5, a, b), tail_90 = DPprior::prob_wsb_exceeds(.9, a, b))
})
rp_write_csv(do.call(rbind, rows), rp_output_path("data-derived", "methods104", "worked-priors.csv"))
rp_write_csv(selected$curve, rp_output_path("data-derived", "methods104", "worked-grid.csv"))
rp_write_json(list(spec = spec, package = as.character(packageVersion("DPprior")),
                   listing_sha256 = rp_sha256("data-frozen/methods104/G2-example.R"),
                   fit_nodes = 160L, verify_nodes = 320L, tsmm_tail = p_majority,
                   trigger = policy$dual$trigger_above, soft_target = policy$dual$soft_target,
                   selected_lambda = selected$selected_lambda, hard_gate = hard$gate),
              rp_output_path("outputs", "v104", "G2-example.json"))
