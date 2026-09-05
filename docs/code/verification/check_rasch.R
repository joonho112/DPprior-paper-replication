#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Check Rasch scoring, count and density reconstruction
#
# Verify the fixed-item WLE calculation and the reconstructed score-group,
# posterior and density summaries. Report the scope explicitly: successful
# aggregate checks do not certify a new posterior refit.
#
# Run after rasch/01_wle.R through 03_density.R.
# Run: Rscript verification/check_rasch.R
# Reads frozen records and generated checks; writes a Rasch verification
# receipt.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(rp_path('rasch', 'inputs.R', must_exist = TRUE))
rasch_check_exports()

# Read score, item and diagnostic records
counts <- rasch_read('score_counts', 'raw_score')
items <- rasch_read('item_counts', 'item')
endorsed <- rasch_read('item_endorsements', 'key')
registry <- rasch_read('prior_and_fit_registry', c('arm', 'role'))

# Reconstruct the score moments and KR-20 benchmark
N <- sum(counts$n)
score_mean <- sum(counts$raw_score * counts$n) / N
score_variance <- sum(counts$n * (counts$raw_score - score_mean)^2) / (N - 1)
# Preserve the manuscript's KR-20 convention: unbiased total-score variance
# and p(1-p) item terms, as explicitly defined in its preparation script.
kr20 <- 13 / 12 * (1 - sum(items$proportion * (1 - items$proportion)) / score_variance)
stopifnot(
  abs(kr20 - .771) <= .0005, nrow(endorsed) == 16L, sum(endorsed$excluded) == 3L,
  all(abs(items$proportion - endorsed$sample_value[match(items$item, endorsed$key)]) < 1e-12)
)
# Expanding anonymous frequencies for quantiles creates no respondent linkage.
q <- quantile(rep(counts$raw_score, counts$n), c(.25, .5, .75), names = FALSE, type = 7)
stopifnot(
  identical(q, c(9, 10, 12)), counts$n[counts$raw_score == 13] == 63L,
  counts$n[counts$raw_score == 0] == 3L, sum(registry$original_two_schedule_deviation) == 5L
)

# Require the independent WLE and posterior-summary checks to pass
for (name in c('wle_checks', 'rasch_posterior_checks')) {
  checks <- rp_read_csv(rp_path('outputs', 'verification', paste0(name, '.csv'), must_exist = TRUE))
  if (!all(checks$passed)) stop('Failed calculation receipt: ', name, call. = FALSE)
}

# Check the retained density/count results
density <- rp_read_csv(rp_path('outputs',
  'verification',
  'rasch_density_checks.csv',
  must_exist = TRUE
))
stopifnot(
  nrow(density) == 6L, all(density$original_mass_check_pass),
  all(abs(density$count_pmf_mass - 1) < 1e-10)
)

# Record the tested scope without claiming a new refit
rp_write_json(
  list(
    N = N, items = 13, raw_score_quantiles = q, KR20 = kr20,
    distinct_prior_ids = length(unique(registry$prior_id)), labeled_fits = nrow(registry),
    passing_fits = sum(registry$gate_pass), excluded_replicates = sum(!registry$gate_pass),
    third_schedule_deviations = sum(registry$original_two_schedule_deviation),
    new_calculation_receipts_verified = TRUE, historical_746_claim_ledger_reexecuted = FALSE
  ),
  rp_output_path('outputs', 'rasch', 'verification_receipt.json')
)
cat('Rasch aggregate facts and new WLE/score/density receipts verified.\n')
