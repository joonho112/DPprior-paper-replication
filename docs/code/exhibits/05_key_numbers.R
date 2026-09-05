#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Collect the selected numerical claims
#
# Calculate the quantities used in the declared manuscript comparisons. This
# ledger is built from analysis frames and policy values, not by copying the
# expected answers. Preserve signed changes unless the stated claim concerns
# their magnitude.
#
# Run after the exhibit build: Rscript exhibits/05_key_numbers.R
# Writes: outputs/key_numbers.csv; verification/check_exhibits.R compares it
# with expectations.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
read <- function(
  track,
  name,
  area = 'data-derived'
) rp_read_csv(rp_path(area, track, paste0(name, '.csv'), must_exist = TRUE))

# Store recalculated values and their source descriptions by claim ID
vals <- list()
sources <- list()
put <- function(
  id,
  value,
  source
) {
  stopifnot(length(value) == 1, is.finite(value))
  vals[[id]] <<- as.numeric(value)
  sources[[id]] <<- source
}

# Simulation: planned fits, exclusions and displayed condition sets
s <- read('simulation', 'fit_status', 'data-frozen')
c <- read('simulation', 'cell_summary')
p <- read('simulation', 'headline_primary')
m <- read('simulation', 'misspecification_medium')
sim <- list(
  planned = nrow(s),
  prefit_excluded = sum(s$status_category == 'dependency_rejection'),
  attempted = sum(s$runnable),
  diagnostic_failed = sum(s$status_category == 'final_diagnostic_failure'),
  converged = sum(s$scientific_eligible),
  main_cells = nrow(p),
  main_planned = sum(p$n_manifest),
  main_converged = sum(p$n_scientific_eligible),
  main_min_n = min(p$n_scientific_eligible),
  main_max_n = max(p$n_scientific_eligible),
  misspec_grid = nrow(m),
  misspec_available = sum(m$n_scientific_eligible > 0),
  misspec_unfitted = sum(m$n_runnable == 0),
  misspec_displayed = nrow(read('simulation', 'misspecification_medium_balanced')),
  misspec_osm_rows = nrow(read('simulation', 'misspecification_balanced')),
  design_size_cells = {
    z <- read('simulation', 'design_size_balanced')
    sum(!(z$prior == 'DORO-Unif' & is.finite(z$r) & z$r != 1))
  },
  unequal_weight_cells = nrow(read('simulation', 'unequal_weights')),
  prefit_cells = sum(c$n_runnable == 0)
)
for (n in names(sim)) put(
  paste0('sim-', n),
  sim[[n]],
  'simulation fit status and reaggregated design frames'
)

# Simulation: grand-mean contrasts and the K*=5 low-information example
q <- read('simulation', 'headline_paired')
for (v in c('n1', 'n100')) for (metric in c('K', 'collapse')) {
  right <- if (v == 'n1') 'N(0,1)' else 'N(0,100 variance)'
  measure <- if (metric == 'K') 'E_K' else 'p_K1'
  z <- q[q$left_grand_mean == 'N(0,0.01)' & q$right_grand_mean == right &
    q$metric == measure, ]
  stopifnot(nrow(z) == 45)
  put(
    paste0('sim-grandmean_', metric, '_', v),
    mean(z$mean_delta),
    'data-derived/simulation/headline_paired.csv; equal average of 45 cell means'
  )
}
for (role in c('tsmm', 'dual')) for (metric in c('collapse', 'mean')) {
  z <- p[p$K_true == 5 & p$I_target == .2 & p$prior == if (role == 'tsmm') 'TSMM' else 'Dual-Anchor', ]
  put(
    paste0('sim-lowI_K5_', role, '_', metric),
    z[[if (metric == 'collapse') 'mean_p_K1' else 'mean_E_K']],
    'data-derived/simulation/headline_primary.csv'
  )
}

# Prior elicitation: worked parameters and policy thresholds
c <- read('calibration', 'count_calibration', 'data-frozen')
c <- c[c$accepted_for_analysis, ]
g <- read('calibration', 'dual_anchor_grid', 'data-frozen')
sel <- g[g$selected, ]
h <- read('calibration', 'hard_sensitivity', 'data-frozen')
for (role in c('tsmm', 'dual', 'hard')) for (param in c('shape', 'rate')) {
  z <- switch(role,
    tsmm = c,
    dual = sel,
    hard = h
  )
  put(
    paste('worked', role, param, sep = '-'),
    z[[if (param == 'shape') 'a' else 'b']],
    'data-frozen/calibration; verified optional recalculation'
  )
}
policy <- yaml::read_yaml(rp_path('calibration', 'policy.yml'))
for (n in c('soft_lambda', 'grid_points', 'trigger', 'soft_target', 'hard_bound')) put(
  paste0('policy-', n),
  switch(n,
    soft_lambda = sel$lambda,
    grid_points = nrow(g),
    trigger = policy$dual$trigger_above,
    soft_target = policy$dual$soft_target,
    hard_bound = policy$dual$hard_bound
  ),
  'calibration/policy.yml and verified 29-point grid'
)

# Applications: primary and sensitivity summaries for the same prior
a <- read('applications', 'enriched')
getapp <- function(
  app,
  role = 'Dual-Soft-Selected',
  spec = 'primary_scale_aligned',
  metric
) {
  z <- a[a$app == app & a$prior_label == role & a$analysis_spec == spec, ]
  stopifnot(nrow(z) == 1)
  z[[metric]]
}
for (app in c('star', 'bd')) {
  put(paste0(app, '-J'), getapp(app, metric = 'J'), 'data-derived/applications/enriched.csv')
  for (role in c('vague', 'tsmm', 'dual')) put(
    paste0(app, '-', role, '_collapse'),
    getapp(app,
      switch(role,
        vague = 'Vague',
        tsmm = 'TSMM',
        dual = 'Dual-Soft-Selected'
      ),
      metric = 'p_K1'
    ),
    'data-derived/applications/enriched.csv'
  )
  put(
    paste0(app, '-dual_K'),
    getapp(app, metric = 'E_K'),
    'data-derived/applications/enriched.csv'
  )
}
for (spec in c('wide', 'hc3')) for (metric in c('K', 'collapse')) put(
  paste0('star-', spec, '_dual_', metric),
  getapp('star',
    spec = if (spec == 'wide') 'wide_scale_sensitivity' else 'star_hc3_uncertainty',
    metric = if (metric == 'K') 'E_K' else 'p_K1'
  ),
  'data-derived/applications/enriched.csv'
)
put(
  'bd-dual_population_majority',
  getapp('bd', metric = 'p_population_largest_atom_weight_gt_05'),
  'data-derived/applications/enriched.csv'
)
put(
  'bd-dual_alpha_functional',
  getapp('bd', metric = 'alpha_only_prior_form_p_first_size_biased_weight_gt_05'),
  'data-derived/applications/enriched.csv'
)

# Rasch: conditional precision and score-group changes
w <- read('rasch', 'wle_metrics')
w <- w[w$role == 'Dual-Anchor', ]
scores <- read('rasch', 'wle_scores')
scores <- scores[scores$role == 'Dual-Anchor', ]
r <- read('rasch', 'posterior_reportable')
registry <- read('rasch', 'prior_and_fit_registry', 'data-frozen')
items <- read('rasch', 'item_counts', 'data-frozen')
delta <- read('rasch', 'score_contrasts')
delta <- delta[delta$arm == 'main' & delta$role == 'dasoft' & delta$r == 13, ]
rv <- list(
  N = sum(scores$n),
  items = nrow(items),
  perfect_scores = scores$n[scores$raw_score == 13],
  distinct_priors = nrow(r),
  labeled_fits = nrow(registry),
  retained_draws = unique(r$n_draws),
  WLE_reliability = w$wle_reliability,
  WLE_mean_error_variance = w$average_error_variance,
  WLE_variance = w$wle_variance,
  perfect_score_SE = scores$standard_error[scores$raw_score == 13],
  count25_K = r$E_K[r$arm == 'sens'],
  count25_population_weight = r$population_largest_atom_weight_mean[r$arm == 'sens'],
  dual_vague_person_RMSD = r$rmsd_vs_reference[r$arm == 'main' & r$role == 'dasoft'],
  score13_shift = abs(delta$delta_r),
  score13_shift_MCSE = delta$mcse_delta_r
)
for (n in names(rv)) put(
  paste0('rasch-', n),
  rv[[n]],
  'rasch WLE, score-group and reportable-fit frames'
)

# Use the claim list only to order and check coverage; expected values are
# compared in a separate verification script
contract <- rp_read_csv(rp_path('provenance', 'claim-contract.csv'), key = 'claim_id')
stopifnot(setequal(names(vals), contract$claim_id))
result <- data.frame(
  claim_id = contract$claim_id,
  value = vapply(contract$claim_id, function(k) vals[[k]], numeric(1)),
  source = vapply(contract$claim_id, function(k) sources[[k]], character(1))
)
rp_write_csv(result, rp_output_path('outputs', 'key_numbers.csv'))
cat(
  nrow(result),
  'predeclared key numbers recalculated; expected comparison is separate.\n'
)
