#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Compare the retained Rasch fits
#
# Retain the six fits that passed the original diagnostics and compare score-
# group means with the main-arm Vague fit. Frequency weighting reconstructs
# the reported shifts without exporting individual responses or posterior
# rows.
#
# Run: Rscript rasch/02_posterior_summaries.R
# Reads: frozen fit registry, posterior summaries, score groups and
# frequencies.
# Writes: posterior_reportable.csv, score_contrasts.csv and
# excluded_replicates.csv.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'inputs.R'))
rasch_check_exports()

# Identify the original retained fits
registry <- rasch_read('prior_and_fit_registry', c('arm', 'role'))
post <- rasch_read('posterior_summary', c('arm', 'role'))
scores <- rasch_read('score_groups', c('arm', 'role', 'r'))
counts <- rasch_read('score_counts', 'raw_score')
stopifnot(
  nrow(registry) == 10L, length(unique(registry$prior_id)) == 6L,
  sum(registry$gate_pass) == 6L, sum(!registry$gate_pass) == 4L, sum(registry$attempt == 3L) == 5L
)
post <- post[post$gate_pass, ]

# Use the main Vague fit as the common score-group reference
reference <- scores[scores$arm == 'main' & scores$role == 'vague', ]
reference <- reference[order(reference$r), ]
stopifnot(identical(reference$r, 0:13), identical(reference$n_r, counts$n))

# Compute score-group changes and frequency-weighted summaries
rows <- summaries <- list()
for (i in seq_len(nrow(post))) {
  f <- post[i, ]
  x <- scores[scores$arm == f$arm & scores$role == f$role, ]
  x <- x[order(x$r), ]
  stopifnot(identical(x$r, reference$r), identical(x$n_r, reference$n_r))
  delta <- x$EAP_r - reference$EAP_r
  # Monte Carlo uncertainty of the estimated difference, not person uncertainty.
  # Retain the manuscript's independent-run propagation rule.
  mcse <- sqrt(x$mcse_EAP_r^2 + reference$mcse_EAP_r^2)
  ratio <- x$width_r / reference$width_r
  rows[[i]] <- data.frame(
    arm = f$arm, role = f$role, reference_role = 'vague', r = x$r, n_r = x$n_r,
    EAP_r = x$EAP_r, EAP_r_reference = reference$EAP_r, delta_r = delta, mcse_delta_r = mcse,
    delta_over_mcse = abs(delta) / mcse, printable_mc_rule = mcse < abs(delta) / 3,
    width_r = x$width_r, width_r_reference = reference$width_r, width_ratio_r = ratio
  )
  j <- which.max(abs(delta))
  summaries[[i]] <- data.frame(
    arm = f$arm,
    role = f$role,
    rmsd_vs_reference = sqrt(sum(x$n_r * delta^2) / 500),
    max_abs_delta_r = abs(delta[j]),
    max_abs_delta_r_mcse = mcse[j],
    width_ratio_mean = sum(x$n_r * ratio) / 500
  )
}
contrasts <- do.call(rbind, rows)
changes <- do.call(rbind, summaries)

# Compare the reconstructed contrasts with the publication export
source(rp_path('simulation', 'inputs.R', must_exist = TRUE))
expected <- rp_read_csv(rp_path('verification',
  'expected',
  'rasch_score_contrasts.csv',
  must_exist = TRUE
))
ids <- paste(post$arm, post$role)
expected <- expected[paste(expected$arm, expected$role) %in% ids, ]
checks <- sim_compare(contrasts, expected, c('arm', 'role', 'r'), 1e-8)
reference_metrics <- rp_read_csv(rp_path('verification',
  'expected',
  'rasch_posterior_metrics.csv',
  must_exist = TRUE
))
reference_metrics <- reference_metrics[
  paste(reference_metrics$arm, reference_metrics$role) %in% ids,
  names(changes)
]
more <- sim_compare(changes, reference_metrics, c('arm', 'role'), 1e-8)
checks$artifact <- 'score_contrasts'
more$artifact <- 'weighted_changes'
checks <- rbind(checks, more)
rp_write_csv(
  checks,
  rp_output_path('outputs', 'verification', 'rasch_posterior_checks.csv')
)
if (!all(checks$passed)) stop('Rasch score-weighted reconstruction differs.', call. = FALSE)
for (n in names(changes)[-(1:2)]) post[[n]] <- changes[[n]][match(paste(post$arm, post$role), paste(changes$arm, changes$role))]

# Check the Count-25 sensitivity separately from main Dual-Anchor
sens <- post[post$arm == 'sens' & post$role == 'tsmm', ]
stopifnot(
  nrow(sens) == 1L, abs(sens$E_K - 11.72) <= .005,
  abs(sens$population_largest_atom_weight_mean - .687) <= .0005
)
# The .012-logit statement is main-arm Dual-Anchor versus Vague, not Count-25.
main_dual <- post[post$arm == 'main' & post$role == 'dasoft', ]
stopifnot(abs(main_dual$rmsd_vs_reference - .012) <= .0005)

# Save passing fits and keep excluded repeats visible
rp_write_csv(post, rp_output_path('data-derived', 'rasch', 'posterior_reportable.csv'))
rp_write_csv(contrasts, rp_output_path('data-derived', 'rasch', 'score_contrasts.csv'))
rp_write_csv(
  registry[!registry$gate_pass, ],
  rp_output_path('data-derived', 'rasch', 'excluded_replicates.csv')
)
cat('Six passing fits; 84 score contrasts; four unpassed replicate diagnostics retained separately.\n')
