#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Compare the prepared public application inputs
#
# Check school/study input rows and the recalculated normal benchmarks against
# their frozen references. This test concerns source preparation, not new
# posterior convergence.
#
# Run after preparing STAR, writing-to-learn and OCRS inputs.
# Command: Rscript verification/check_application_inputs.R
# Writes: application input and benchmark comparison receipts.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(rp_path('applications', 'data_loaders.R', must_exist = TRUE))

# Compare each prepared school/study input
for (name in c(
  'star_multilevel',
  'star_hc3',
  'bangert_drowns_primary',
  'ocrs_descriptive_primary'
)) {
  d <- rp_read_csv(rp_path('data-derived',
    'applications',
    paste0(name, '.csv'),
    must_exist = TRUE
  ))
  app_input_parity(d, name)
}
source(rp_path('simulation', 'inputs.R', must_exist = TRUE))

# Combine the recalculated normal benchmarks
actual <- rbind(
  rp_read_csv(rp_path('data-derived',
    'applications',
    'normal_re_summary.csv',
    must_exist = TRUE
  )),
  rp_read_csv(rp_path('data-derived',
    'applications',
    'ocrs_normal_re_summary.csv',
    must_exist = TRUE
  ))
)

# Check against the original benchmark and report discrepancies
expected <- rp_read_csv(rp_path('data-frozen',
  'applications',
  'normal_benchmark.csv',
  must_exist = TRUE
))
checks <- sim_compare(actual, expected, 'application', tolerance = 1e-8)
rp_write_csv(
  checks,
  rp_output_path('outputs', 'verification', 'application_benchmark_checks.csv')
)
if (!all(checks$passed)) stop('Fresh normal-RE benchmarks differ.', call. = FALSE)
cat('Four input variants and all three fresh normal-RE benchmarks match.\n')
