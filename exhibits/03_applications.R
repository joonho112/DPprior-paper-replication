# Author: JoonHo Lee (jlee296@ua.edu)
# Build the school-study application exhibits
#
# Rebuild the inherited 104 OSM application Tables H.1, H.2 and H.5–9.
# Source this module after the derived school/study runners and shared bootstrap.
source(rp_path('exhibits', 'helpers.R'))
a <- ex_read('applications', 'primary.csv')
a <- a[a$app %in% c('star','bd'), ]

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

# OSM H.5: retain extrema over all checked diagnostic quantities
d <- ex_read('applications', 'diagnostic_table.csv')
d <- d[d$analysis_spec == 'primary_scale_aligned', ]
ex_table('osm-table-H.5',
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

# OSM H.6: compare model-scale and first-stage sensitivity
s <- ex_read('applications', 'sensitivity.csv')
sensitivity_display <- data.frame(
  Comparison = ifelse(s$comparison == 'primary_vs_wide_scale', 'Wide scale', 'STAR HC3'),
  Application = s$application_short_label, Hyperprior = s$display_label,
  'Change in mean count' = sprintf('%+.3f', s$delta_E_K),
  'Change in one-cluster probability' = sprintf('%+.3f', s$delta_p_K1),
  'Unit-mean RMSD' = sprintf('%.4f', s$rmse_unit_mean_shift),
  'Changed intervals / J' = sprintf('%d/%d', s$interval_decision_changed_count, s$unit_denominator),
  check.names = FALSE
)
ex_table('osm-table-H.6',
  s,
  sensitivity_display,
  sources = 'data-derived/applications/sensitivity.csv',
  note = 'Sensitivity minus primary, within the same application/prior. Wider model scale and STAR HC3 change inputs beyond the concentration hyperprior.'
)

# OSM H.7: display the public-use OCRS analysis
o <- ex_read('applications', 'ocrs_posterior.csv')
dis <- data.frame(
  Prior = o$display_label,
  'Gamma (shape, rate)' = paste0('(', ex_f(o$manifest_a, 2), ', ', ex_f(o$manifest_b, 2), ')'),
  'Posterior count [95%]' = ex_ci(o$E_K, o$q025_K, o$q975_K, 2),
  'P(K=1)' = ex_f(o$p_K1),
  'RMSD vs Vague' = ex_f(o$rmse_unit_shift_from_vague, 4),
  check.names = FALSE
)
ex_table('osm-table-H.7',
  o,
  dis,
  sources = 'data-derived/applications/ocrs_posterior.csv',
  note = 'OCRS is publicly available through AIR/ICPSR project 202181, version 1. These 24-school results describe the available complete-case sample and do not reproduce the original adjusted trial estimand. Raw input preparation and optional refitting are documented in DATA_ACCESS.md.'
)

# OSM H.8-H.9: preserve the original joint-posterior profile intervals
s <- ex_read('applications', 'star_location.csv')
s <- s[s$display_label %in% c('Vague', 'Dual-Anchor'), ]
ex_table('osm-table-H.8',
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
ex_table('osm-table-H.9',
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
