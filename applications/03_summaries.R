#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Rebuild the primary application summaries
#
# Join the retained fit summaries to unit stability and normal-model
# benchmarks. Keep observed allocation shares, population atom weights and
# alpha-only tail functionals as distinct quantities.
#
# Run: Rscript applications/03_summaries.R
# Reads: checked frozen application summaries; no raw source files or MCMC.
# Writes: primary.csv, enriched.csv, unit_stability.csv and unit_detail.csv.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'summary_helpers.R'))
source(file.path(.script_dir, 'summary_inputs.R'))

# Load and label the retained fits
verified <- app_read_summaries()
enriched <- appv2_pub_enrich_fits(verified)

# Calculate within-application unit changes
unit_stability <- appv2_pub_build_unit_stability(verified$unit, enriched)
# Primary-scale publication table --------------------------------------------
primary <- enriched[enriched$analysis_spec == "primary_scale_aligned", , drop = FALSE]
unit_position <- match(
  primary$execution_fit_key,
  unit_stability$summary$execution_fit_key
)
if (anyNA(unit_position)) appv2_pub_stop("primary unit-stability join failed")
unit_fields <- c(
  "unit_denominator", "posterior_mean_across_units",
  "unpooled_mean_across_units", "rmse_posterior_vs_unpooled",
  "mean_abs_posterior_vs_unpooled", "rmse_unit_shift_from_vague",
  "mean_abs_unit_shift_from_vague", "max_abs_unit_shift_from_vague",
  "correlation_with_vague_unit_means", "interval_positive_count",
  "interval_negative_count", "interval_excludes_zero_count",
  "interval_includes_zero_count",
  "interval_decision_changed_from_vague_count",
  "interval_exclusion_gained_from_vague_count",
  "interval_exclusion_lost_from_vague_count", "interval_rule"
)
for (field in unit_fields) {
  primary[[field]] <- unit_stability$summary[[field]][unit_position]
}

# Attach the separately estimated normal-model benchmark
normal_position <- match(primary$app, verified$normal$app)
if (anyNA(normal_position)) appv2_pub_stop("primary normal-RE join failed")
normal_fields <- c(
  "pooled_effect", "pooled_se", "pooled_ci_lower", "pooled_ci_upper",
  "tau2_REML", "I2_percent", "prediction_lower", "prediction_upper"
)
for (field in normal_fields) primary[[paste0("normal_re_", field)]] <-
  verified$normal[[field]][normal_position]
primary <- primary[order(
  match(primary$app, c("star", "bd", "ocrs")),
  primary$prior_order
), , drop = FALSE]
row.names(primary) <- NULL
main_applications <- primary[primary$app %in% c("star", "bd"), , drop = FALSE]
if (nrow(primary) != 15L || nrow(main_applications) != 10L ||
  any(table(primary$app) != 5L) ||
  any(table(main_applications$app) != 5L)) {
  appv2_pub_stop("primary publication table is not the 15-row/10-main grid")
}

# Write the publication and supporting unit frames
for (n in c('primary', 'enriched')) rp_write_csv(
  get(n),
  rp_output_path('data-derived', 'applications', paste0(n, '.csv'))
)
rp_write_csv(
  unit_stability$summary,
  rp_output_path('data-derived', 'applications', 'unit_stability.csv')
)
rp_write_csv(
  unit_stability$detail,
  rp_output_path('data-derived', 'applications', 'unit_detail.csv')
)
cat('35 fits / 16000 draws per fit; 15 primary rows and unit stability rebuilt.\n')
