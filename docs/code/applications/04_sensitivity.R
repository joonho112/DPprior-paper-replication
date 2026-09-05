#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Compare scale and first-stage sensitivity fits
#
# Match each sensitivity fit to the same application and prior under the
# primary specification. Report sensitivity minus primary, including shifts in
# unit posterior means and interval decisions.
#
# Run: Rscript applications/04_sensitivity.R
# Reads: checked frozen fit and unit summaries.
# Writes: sensitivity.csv and sensitivity_long.csv (20 matched comparisons).

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'summary_helpers.R'))
source(file.path(.script_dir, 'summary_inputs.R'))

# Load the primary and sensitivity summaries
verified <- app_read_summaries()
enriched <- appv2_pub_enrich_fits(verified)
unit_stability <- appv2_pub_build_unit_stability(verified$unit, enriched)

# Define the posterior quantities to compare
metrics <- c(
  'E_K', 'alpha_mean', 'alpha_only_prior_form_p_first_size_biased_weight_gt_05',
  'median_K',
  'observed_largest_share_mean',
  'p_K1',
  'p_K_le2',
  'p_observed_largest_share_gt_05',
  'p_population_largest_atom_weight_gt_05', 'p_population_largest_occupied_weight_gt_05',
  'population_largest_atom_weight_mean', 'population_largest_occupied_weight_mean',
  'q025_K', 'q975_K', 'residual_unoccupied_mass_mean', 'sd_K'
)

# Match by application and prior
#
# Each sensitivity row must have one primary reference. Differences below
# always use sensitivity minus reference.
compare_fits <- function(spec, label) {
  refs <- enriched[enriched$analysis_spec == 'primary_scale_aligned', ]
  sens <- enriched[enriched$analysis_spec == spec, ]
  rows <- list()
  for (i in seq_len(nrow(sens))) {
    s <- sens[i, ]
    r <- refs[refs$app == s$app & refs$prior_label == s$prior_label, ]
    stopifnot(nrow(r) == 1L)
    for (metric in metrics) rows[[length(rows) + 1L]] <- data.frame(
      comparison = label, app = r$app,
      prior_label = r$prior_label, display_label = r$display_label, prior_role = r$prior_role,
      scientific_role = r$scientific_role,
      reference_spec = r$analysis_spec,
      sensitivity_spec = s$analysis_spec,
      metric = metric, reference_value = r[[metric]], sensitivity_value = s[[metric]],
      delta_sensitivity_minus_reference = s[[metric]] - r[[metric]]
    )
  }
  do.call(rbind, rows)
}

# Combine model-scale and first-stage comparisons
verified$wide <- compare_fits('wide_scale_sensitivity', 'primary_vs_wide_scale')
verified$hc3 <- compare_fits('star_hc3_uncertainty', 'star_primary_vs_hc3')
sensitivity_unit <- appv2_pub_build_sensitivity_unit_shifts(unit_stability$detail, enriched)
comparison_long <- rbind(verified$wide, verified$hc3)
comparison_wide <- rbind(
  appv2_pub_widen_comparison(verified$wide),
  appv2_pub_widen_comparison(verified$hc3)
)
sensitivity <- merge(
  comparison_wide, sensitivity_unit,
  by = c(
    "comparison", "app", "prior_label", "display_label",
    "reference_spec", "sensitivity_spec"
  ),
  all = TRUE, sort = FALSE, suffixes = c("", "_unit")
)
if (nrow(sensitivity) != 20L || anyNA(sensitivity$rmse_unit_mean_shift)) {
  appv2_pub_stop("20-row structural/unit sensitivity join failed")
}
sensitivity$publication_role <- appv2_pub_publication_role(sensitivity$prior_label)
sensitivity$application_scope <- appv2_pub_application_scope(sensitivity$app)
sensitivity$application_short_label <-
  appv2_pub_application_short_label(sensitivity$app)
sensitivity$fit_denominator <- 35L
sensitivity$admitted_fit_count <- 35L
sensitivity$excluded_fit_count <- 0L
sensitivity <- sensitivity[order(
  match(sensitivity$comparison, c("primary_vs_wide_scale", "star_primary_vs_hc3")),
  match(sensitivity$app, c("star", "bd", "ocrs")),
  sensitivity$prior_order
), , drop = FALSE]
row.names(sensitivity) <- NULL

# Save both the display frame and metric-level differences
rp_write_csv(
  sensitivity,
  rp_output_path('data-derived', 'applications', 'sensitivity.csv')
)
rp_write_csv(
  comparison_long,
  rp_output_path('data-derived', 'applications', 'sensitivity_long.csv')
)
cat('20 matched sensitivity rows rebuilt.\n')
