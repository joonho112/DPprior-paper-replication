#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Rebuild diagnostic and supplemental application tables
#
# Use each fit's final accepted diagnostic attempt. Preserve the original
# joint-posterior location and moderator intervals; averaging marginal school
# intervals would not recover those group intervals.
#
# Run after 03_summaries.R: Rscript applications/06_supplemental.R
# Writes: diagnostic_table.csv, star_location.csv, bd_moderator.csv and
# ocrs_posterior.csv.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'summary_helpers.R'))
source(file.path(.script_dir, 'summary_inputs.R'))

# Load the retained fit and profile summaries
verified <- app_read_summaries()
enriched <- appv2_pub_enrich_fits(verified)
unit_stability <- appv2_pub_build_unit_stability(verified$unit, enriched)
# Final diagnostic rollup remains fit-denominated. ---------------------------
diagnostic_rows <- list()
for (key in enriched$execution_fit_key) {
  fit <- enriched[enriched$execution_fit_key == key, , drop = FALSE]
  rows <- verified$diagnostic[
    verified$diagnostic$execution_fit_key == key &
      verified$diagnostic$diagnostic_attempt == fit$final_attempt, ,
    drop = FALSE
  ]
  attempts <- verified$attempt[
    verified$attempt$execution_fit_key == key, ,
    drop = FALSE
  ]
  if (!nrow(rows) || !nrow(attempts) || !all(appv2_pub_flag(rows$gate_pass))) {
    appv2_pub_stop(sprintf("diagnostic rollup is incomplete for %s", key))
  }
  diagnostic_rows[[length(diagnostic_rows) + 1L]] <- data.frame(
    execution_fit_key = key, app = fit$app,
    application_short_label = fit$application_short_label,
    application_scope = fit$application_scope,
    analysis_spec = fit$analysis_spec, prior_order = fit$prior_order,
    prior_label = fit$prior_label, display_label = fit$display_label,
    publication_role = fit$publication_role, final_attempt = fit$final_attempt,
    attempted_schedule_count = nrow(attempts), diagnostic_variable_count = nrow(rows),
    max_rhat = max(rows$rhat), min_ess_bulk = min(rows$ess_bulk),
    min_ess_tail = min(rows$ess_tail), all_final_gates_pass = TRUE,
    retained_partition_draws = fit$retained_partition_draws,
    fit_denominator = 35L, admitted_fit_count = 35L, excluded_fit_count = 0L,
    stringsAsFactors = FALSE
  )
}
diagnostic_table <- do.call(rbind, diagnostic_rows)
row.names(diagnostic_table) <- NULL
# Descriptive moderators retain their non-causal/non-cluster caveats. --------
star_location <- verified$star_location
star_location$publication_role <- appv2_pub_publication_role(
  star_location$prior_label
)
star_location$application_scope <- appv2_pub_application_scope("star")
star_location$unit_denominator <- star_location$J
star_location$fit_denominator <- 35L
star_location$admitted_fit_count <- 35L
star_location$excluded_fit_count <- 0L
bd_moderator <- verified$bd_moderator
bd_moderator$publication_role <- appv2_pub_publication_role(bd_moderator$prior_label)
bd_moderator$application_scope <- appv2_pub_application_scope("bd")
bd_moderator$fit_denominator <- 35L
bd_moderator$admitted_fit_count <- 35L
bd_moderator$excluded_fit_count <- 0L

# Save diagnostics and the original joint-posterior profiles
for (n in c('diagnostic_table', 'star_location', 'bd_moderator')) rp_write_csv(
  get(n),
  rp_output_path('data-derived', 'applications', paste0(n, '.csv'))
)

# Extract the five-prior OCRS supplemental comparison
primary <- rp_read_csv(rp_path('data-derived', 'applications', 'primary.csv', must_exist = TRUE))
ocrs <- primary[primary$app == 'ocrs', ]
stopifnot(nrow(ocrs) == 5L)
rp_write_csv(ocrs, rp_output_path('data-derived', 'applications', 'ocrs_posterior.csv'))
rp_write_json(
  list(
    OCRS_source = 'AIR/ICPSR public-use V1', posterior_rows = nrow(ocrs),
    summary_regenerated = TRUE, posterior_refit_performed = FALSE,
    raw_to_input_command = 'Rscript applications/05_prepare_ocrs.R --input PATH',
    profiles = 'Frozen joint-posterior summaries with rebuilt role and denominator metadata'
  ),
  rp_output_path('outputs', 'applications', 'supplemental_receipt.json')
)
cat('35 diagnostic rows; STAR/BD profiles and five OCRS posterior rows prepared.\n')
