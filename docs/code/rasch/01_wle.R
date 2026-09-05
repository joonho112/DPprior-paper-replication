#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Recalculate conditional WLE reliability
#
# Solve the weighted-likelihood equation for 14 raw scores under each of six
# fixed item-difficulty vectors. Weight the resulting scores and squared
# standard errors by the observed frequencies to obtain sample reliability.
#
# Run: Rscript rasch/01_wle.R
# Reads: wle_item_difficulties.csv and score_counts.csv; no person rows.
# Writes: wle_scores.csv, wle_metrics.csv and independent comparison receipts.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'inputs.R'))
rasch_check_exports()
source(rp_path('common', 'R', 'wle.R', must_exist = TRUE))

# Load fixed item difficulties and raw-score frequencies
items <- rasch_read('wle_item_difficulties', c('role', 'item'))
counts <- rasch_read('score_counts', 'raw_score')
stopifnot(identical(counts$raw_score, 0:13), sum(counts$n) == 500L, nrow(items) == 78L)

# Solve each score under each item-difficulty vector
roles <- unique(items$role)
scores <- metrics <- list()

# The same 14 score groups are used for every role. The centered item order is
# checked before solving, so a reordered beta vector cannot pass unnoticed.
for (role in roles) {
  x <- items[items$role == role, ]
  stopifnot(
    identical(x$item_order, 1:13),
    abs(sum(x$posterior_mean_difficulty)) < 1e-8
  )
  direct <- t(vapply(
    counts$raw_score,
    function(r) wle_score(x$posterior_mean_difficulty, r),
    numeric(3)
  ))
  stopifnot(all(is.finite(direct)), max(abs(direct[, 'residual'])) < 1e-8)
  scores[[role]] <- data.frame(
    role = role,
    raw_score = counts$raw_score,
    n = counts$n,
    wle = direct[, 'wle'],
    standard_error = direct[, 'standard_error'], warm_equation_residual = direct[, 'residual']
  )
  metrics[[role]] <- cbind(
    data.frame(role = role),
    as.data.frame(as.list(wle_reliability(direct[, 'wle'], direct[, 'standard_error'], counts$n)))
  )
}

# Combine the score rows and reliability summaries
scores <- do.call(rbind, scores)
metrics <- do.call(rbind, metrics)
rp_write_csv(scores, rp_output_path('data-derived', 'rasch', 'wle_scores.csv'))
rp_write_csv(metrics, rp_output_path('data-derived', 'rasch', 'wle_metrics.csv'))
source(rp_path('simulation', 'inputs.R', must_exist = TRUE))

# Compare with independently recorded scoring results
checks <- rbind(
  transform(
    sim_compare(
      scores,
      rp_read_csv(rp_path('verification', 'expected', 'rasch_wle_scores.csv', must_exist = TRUE)),
      c('role', 'raw_score'),
      1e-8
    ),
    artifact = 'scores'
  ),
  transform(
    sim_compare(
      metrics,
      rp_read_csv(rp_path('verification',
        'expected',
        'rasch_wle_metrics.csv',
        must_exist = TRUE
      )),
      'role',
      1e-8
    ),
    artifact = 'metrics'
  )
)
rp_write_csv(checks, rp_output_path('outputs', 'verification', 'wle_checks.csv'))
if (!all(checks$passed)) stop('WLE verification failed.', call. = FALSE)

# Check the reported reliability and perfect-score precision
primary <- metrics[metrics$role == 'Dual-Anchor', ]
perfect <- scores[scores$role == 'Dual-Anchor' & scores$raw_score == 13, ]
stopifnot(
  abs(primary$wle_reliability - .7121) <= .00005,
  abs(perfect$standard_error - 1.733) <= .0005
)
rp_write_json(
  list(
    scope = 'Conditional WLE scoring and reliability from fixed item estimates; no item refit',
    role_count = length(roles),
    score_solutions = nrow(scores),
    people = sum(counts$n),
    variance_denominator = 499,
    primary = primary, perfect_score_SE = perfect$standard_error, all_comparisons_passed = TRUE
  ),
  rp_output_path('outputs', 'rasch', 'wle_receipt.json')
)
print(metrics, row.names = FALSE)
