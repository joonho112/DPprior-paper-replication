# Author: JoonHo Lee (jlee296@ua.edu)
# Build the school-study application exhibits
#
# Combine STAR, writing-to-learn and Rasch prior/posterior rows for the main
# comparison. The supplemental displays preserve first-stage and model-scale
# distinctions, including the public-use OCRS example.
#
# Called by exhibits/00_build_all.R.
# Builds: main Tables 4-5, Figure 7 and OSM Tables H.1-H.7.
# Posterior summaries come from the retained fits; no models are fitted here.

source(rp_path('exhibits', 'helpers.R'))

# Read the main application fits and their prior settings
a <- ex_read('applications', 'primary.csv')
a <- a[a$app %in% c('star', 'bd'), ]
r <- ex_read('rasch', 'posterior_reportable.csv')
r <- r[r$arm == 'main', ]
r$label <- ex_rasch_role(r)
priors <- ex_read('rasch', 'priors.csv', 'data-frozen')
priors <- priors[priors$arm == 'main' & priors$role %in% r$role, ]

# Table 4: align school-study and Rasch prior summaries
pa <- data.frame(
  Data = a$application_short_label,
  Prior = a$display_label,
  a = a$manifest_a,
  b = a$manifest_b,
  E_K = a$manifest_prior_EK,
  q05 = a$manifest_prior_K05,
  q95 = a$manifest_prior_K95,
  tail_05 = ex_tail(a$manifest_a, a$manifest_b, .5),
  tail_09 = ex_tail(a$manifest_a, a$manifest_b, .9)
)
pr <- data.frame(
  Data = 'Vocabulary checklist',
  Prior = unname(ex_rasch_labels[priors$role]),
  a = priors$a,
  b = priors$b,
  E_K = priors$E_K,
  q05 = priors$q05,
  q95 = priors$q95,
  tail_05 = priors$wsb_gt_05,
  tail_09 = priors$wsb_gt_09
)
prior <- rbind(pa, pr)
stopifnot(nrow(prior) == 15)
dis <- data.frame(
  Data = prior$Data,
  Prior = prior$Prior,
  'Gamma (shape, rate)' = paste0('(', ex_f(prior$a, 2), ', ', ex_f(prior$b, 2), ')'),
  'Prior count [90%]' = ex_ci(prior$E_K, prior$q05, prior$q95, 2),
  'P(W_SB > .5)' = ex_f(prior$tail_05),
  'P(W_SB > .9)' = ex_f(prior$tail_09),
  check.names = FALSE
)
dis[prior$a > 1e5, 'Gamma (shape, rate)'] <- 'alpha approximately 2'
ex_table('main-table-4',
  prior,
  dis,
  sources = c('data-derived/applications/primary.csv', 'data-frozen/rasch/priors.csv'),
  note = 'Gamma parameters use shape and rate. DORO-Unif and the Gamma(1,3) literature default from Paganin et al. (2023) are external references. The hard solution is a sensitivity analysis; the Rasch hard prior is effectively fixed at alpha=2.'
)

# Table 5: align posterior counts, weights and unit-mean changes
post <- rbind(
  data.frame(
    Data = a$application_short_label,
    Prior = a$display_label,
    E_K = a$E_K,
    q025 = a$q025_K,
    q975 = a$q975_K,
    p_K1 = a$p_K1,
    observed_share = a$observed_largest_share_mean,
    population_weight = a$population_largest_atom_weight_mean,
    RMSD = a$rmse_unit_shift_from_vague
  ),
  data.frame(
    Data = 'Vocabulary checklist',
    Prior = r$label,
    E_K = r$E_K,
    q025 = r$q025_K,
    q975 = r$q975_K,
    p_K1 = r$p_K1,
    observed_share = r$observed_largest_share_mean,
    population_weight = r$population_largest_atom_weight_mean,
    RMSD = r$rmsd_vs_reference
  )
)
dis <- data.frame(
  Data = post$Data,
  Prior = post$Prior,
  'Posterior count [95%]' = ex_ci(post$E_K, post$q025, post$q975, 2),
  'P(K=1)' = ex_f(post$p_K1),
  'Observed share' = ex_f(post$observed_share),
  'Population weight' = ex_f(post$population_weight),
  'Unit-mean RMSD' = ex_f(post$RMSD, 4),
  check.names = FALSE
)
ex_table('main-table-5',
  post,
  dis,
  sources = c(
    'data-derived/applications/primary.csv',
    'data-derived/rasch/posterior_reportable.csv'
  ),
  note = 'All 15 fits passed the original diagnostic criteria. RMSD compares unit posterior means with Vague within each application; Rasch units are logits. Observed shares and population weights are distinct quantities.'
)
# Nine panels; all role labels remain readable and external references retain
# their own names. Only count panels show intervals (as in the manuscript).

# Figure 7: compare three quantities within each application
plots <- list()
datalevels <- unique(post$Data)
for (i in seq_along(datalevels)) for (k in c('E_K', 'observed_share', 'population_weight')) {
  z <- post[post$Data == datalevels[i], ]
  z$role <- factor(z$Prior, levels = rev(ex_roles[ex_roles %in% z$Prior]))
  p <- ggplot(z, aes(.data[[k]], role, colour = Prior, shape = Prior)) +
    geom_point(size = 2.2) +
    scale_colour_manual(values = palette_roles) +
    scale_shape_manual(values = shape_roles) +
    theme_v3(legend = 'none') +
    labs(x = NULL, y = if (k == 'E_K') datalevels[i] else NULL)
  if (k == 'E_K') p <- p + geom_segment(aes(x = q025, xend = q975, yend = role), linewidth = .35) +
    scale_x_continuous(limits = c(0, max(post$q975) + .5))
  else p <- p + geom_vline(xintercept = .5, linetype = 3, colour = 'grey60') +
    scale_x_continuous(limits = c(0, 1)) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  if (i == 1) p <- p +
    labs(title = switch(k,
      E_K = 'A. Occupied clusters',
      observed_share = 'B. Observed share',
      population_weight = 'C. Population weight'
    ))
  plots[[length(plots) + 1]] <- p
}
ex_figure('main-figure-7',
  wrap_plots(plots, ncol = 3),
  post,
  c(
    'data-derived/applications/primary.csv',
    'data-derived/rasch/posterior_reportable.csv'
  ),
  width = 9,
  height = 7.2,
  note = 'Count means and 95% intervals; mean largest observed shares and population atom weights. Literature default is Paganin et al. (2023), Gamma(1,3). Population measures include unoccupied mass.'
)

# OSM H.1-H.2: normal benchmarks and additional posterior summaries
normal <- ex_read('applications', 'normal_benchmark.csv', 'data-frozen')
ex_table('osm-table-H.1',
  normal,
  normal[c(
    'application',
    'J',
    'pooled_effect',
    'pooled_ci_lower',
    'pooled_ci_upper',
    'tau2_REML',
    'I2_percent',
    'prediction_lower',
    'prediction_upper'
  )],
  sources = 'data-frozen/applications/normal_benchmark.csv',
  note = 'Frozen normal random-effects benchmarks. Optional public-data runners re-estimate them from prepared inputs; OCRS uses the public-use descriptive complete-case school contrasts.'
)
ex_table('osm-table-H.2',
  a,
  a[c(
    'application_short_label',
    'display_label',
    'p_population_largest_atom_weight_gt_05',
    'alpha_only_prior_form_p_first_size_biased_weight_gt_05',
    'interval_excludes_zero_count',
    'J'
  )],
  sources = 'data-derived/applications/primary.csv',
  note = 'Largest population atom includes unoccupied mass. E(.5^alpha | y) is an alpha-only functional, not posterior population dominance. Intervals use strict equal-tail exclusion of zero.'
)

# OSM H.3: retain extrema over all checked diagnostic quantities
d <- ex_read('applications', 'diagnostic_table.csv')
d <- d[d$analysis_spec == 'primary_scale_aligned', ]
ex_table('osm-table-H.3',
  d,
  d[c(
    'application_short_label',
    'display_label',
    'final_attempt',
    'max_rhat',
    'min_ess_bulk',
    'min_ess_tail'
  )],
  sources = 'data-derived/applications/diagnostic_table.csv',
  note = 'Extrema over every checked quantity, not alpha or K alone. All 15 primary fits pass their original gates.'
)

# OSM H.4: compare model-scale and first-stage sensitivity
s <- ex_read('applications', 'sensitivity.csv')
ex_table('osm-table-H.4',
  s,
  s[c(
    'comparison',
    'application_short_label',
    'display_label',
    'delta_E_K',
    'delta_p_K1',
    'rmse_unit_mean_shift',
    'interval_decision_changed_count',
    'unit_denominator'
  )],
  sources = 'data-derived/applications/sensitivity.csv',
  note = 'Sensitivity minus primary, within the same application/prior. Wider model scale and STAR HC3 change inputs beyond the concentration hyperprior.'
)

# OSM H.5: display the public-use OCRS analysis
o <- ex_read('applications', 'ocrs_posterior.csv')
dis <- data.frame(
  Prior = o$display_label,
  'Gamma (shape, rate)' = paste0('(', ex_f(o$manifest_a, 2), ', ', ex_f(o$manifest_b, 2), ')'),
  'Posterior count [95%]' = ex_ci(o$E_K, o$q025_K, o$q975_K, 2),
  'P(K=1)' = ex_f(o$p_K1),
  'RMSD vs Vague' = ex_f(o$rmse_unit_shift_from_vague, 4),
  check.names = FALSE
)
ex_table('osm-table-H.5',
  o,
  dis,
  sources = 'data-derived/applications/ocrs_posterior.csv',
  note = 'OCRS is publicly available through AIR/ICPSR project 202181, version 1. These 24-school results describe the available complete-case sample and do not reproduce the original adjusted trial estimand. Raw input preparation and optional refitting are documented in DATA_ACCESS.md.'
)

# OSM H.6-H.7: preserve the original joint-posterior profile intervals
s <- ex_read('applications', 'star_location.csv')
s <- s[s$display_label %in% c('Vague', 'Dual-Anchor'), ]
ex_table('osm-table-H.6',
  s,
  s[c(
    'level',
    'unit_count',
    'observed_effect_mean',
    'display_label',
    'posterior_profile_mean',
    'posterior_profile_q025',
    'posterior_profile_q975'
  )],
  sources = 'data-derived/applications/star_location.csv',
  note = 'Location profiles preserve joint posterior group intervals; they are not reconstructed by averaging marginal school interval endpoints. Vague and Dual-Anchor appear as paired rows per location.'
)
b <- ex_read('applications', 'bd_moderator.csv')
ex_table('osm-table-H.7',
  b,
  b[c(
    'display_label',
    'moderator',
    'posterior_unadjusted_slope_mean',
    'posterior_unadjusted_slope_q025',
    'posterior_unadjusted_slope_q975'
  )],
  sources = 'data-derived/applications/bd_moderator.csv',
  note = 'Descriptive, unadjusted posterior slopes for grade and intervention length. Original joint-draw intervals are preserved; these are not a new meta-regression.'
)
