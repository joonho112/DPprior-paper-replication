#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Check the rebuilt manuscript results
#
# Verify the independent expected-file hashes before running the simulation,
# application, Rasch and exhibit comparisons. This entrypoint checks an
# existing build; it does not generate replacement expected values.
#
# Run after exhibits/00_build_all.R: Rscript verify_reproduction.R
# Writes: outputs/verification/reproduction.json.
# The known Table 2 rounding difference remains a separate author-review item.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, 'common', 'R', 'bootstrap.R'))

# Require unchanged independent expected files
expected <- rp_read_csv(rp_path('provenance', 'expected-hashes.csv'), key = 'path')
for (i in seq_len(nrow(expected))) stopifnot(rp_sha256(file.path(.replication_root, expected$path[i])) == expected$sha256[i])

# Run the simulation, application, Rasch and exhibit checks
checks <- c(
  'check_simulation.R',
  'check_applications.R',
  'check_rasch.R',
  'check_exhibits.R'
)
for (script in checks) {
  result <- system2(
    file.path(R.home('bin'), 'Rscript'),
    shQuote(rp_path('verification', script, must_exist = TRUE))
  )
  if (result != 0L) stop('Independent check failed: ', script, call. = FALSE)
}

# Record verification scope and the remaining manuscript discrepancy
rp_write_json(
  list(
    schema = 'quick-verification/1', passed = TRUE, checks = checks,
    expected_manifest_sha256 = rp_sha256(rp_path('provenance', 'expected-hashes.csv')),
    production_refit_performed = FALSE,
    known_manuscript_discrepancies = 'provenance/manuscript-discrepancies.csv'
  ),
  rp_output_path('outputs', 'verification', 'reproduction.json')
)
cat('Independent quick-path checks passed. Known manuscript discrepancies and untested production refits remain separate.\n')
