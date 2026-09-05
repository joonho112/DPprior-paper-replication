#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Rebuild the simulation cell summaries
#
# Start from all planned conditions, then account for the fits that could not
# be calibrated or did not pass diagnostics. This keeps the denominator
# visible even when a condition has no usable estimates.
#
# Run: Rscript simulation/01_reaggregate.R
# Reads: checked frozen simulation exports.
# Writes: data-derived/simulation/cell_summary.csv and comparison receipts.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'aggregation.R'))
source(file.path(.script_dir, 'inputs.R'))

# Load verified fit metrics and the planned denominators
x <- sim_read_inputs()

# Aggregate conditions and reconcile every exclusion
result <- simv2_synthesis_build_cell_summary(
  x$fits,
  x$manifest,
  x$denominators,
  x$calibration
)
stopifnot(
  nrow(result) == 322L, sum(result$n_manifest) == 64400L,
  sum(result$n_dependency_excluded) == 2800L, sum(result$n_diagnostic_excluded) == 4449L,
  sum(result$n_scientific_eligible) == 57151L, sum(result$n_other_excluded) == 0L,
  sum(result$n_manifest) == sum(result$n_dependency_excluded + result$n_diagnostic_excluded +
    result$n_scientific_eligible)
)

# Compare with the original publication export
expected <- rp_read_csv(rp_path('verification',
  'expected',
  'simulation_cell_summary.csv',
  must_exist = TRUE
))
checks <- sim_compare(result, expected, c('suite', 'conceptual_cell_key'))
rp_write_csv(
  checks,
  rp_output_path('outputs', 'verification', 'simulation_cell_checks.csv')
)
if (!all(checks$passed)) stop('Cell summary differs from manuscript reference.', call. = FALSE)

# Write the analysis frame only after comparison passes
rp_write_csv(result, rp_output_path('data-derived', 'simulation', 'cell_summary.csv'))
rp_write_json(
  list(
    planned = sum(result$n_manifest), runnable = sum(result$n_runnable),
    retained = sum(result$n_scientific_eligible),
    calibration_excluded = sum(result$n_dependency_excluded),
    diagnostic_excluded = sum(result$n_diagnostic_excluded), cells = nrow(result),
    compared_columns = nrow(checks), all_columns_passed = all(checks$passed), mcmc_run = FALSE
  ),
  rp_output_path('outputs', 'simulation', 'reaggregation_receipt.json')
)
cat(nrow(result), 'cells reaggregated;', nrow(checks), 'columns match.\n')
