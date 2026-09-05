#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Check reconstructed application summaries
#
# Compare primary, sensitivity, diagnostic and supplemental frames with the
# original publication exports. Match the declared keys before comparing
# columns so an incorrectly joined row cannot pass by position.
#
# Run after the application summary runners: Rscript
# verification/check_applications.R
# Reads generated frames and verification/expected/; writes comparison
# results.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(rp_path('simulation', 'inputs.R', must_exist = TRUE)) # shared numerical comparator only
registry <- jsonlite::read_json(rp_path('provenance', 'application-posterior-export.json', must_exist = TRUE),
  simplifyVector = TRUE
)$expected
files <- c(
  primary = 'primary', sensitivity = 'sensitivity', diagnostic = 'diagnostic_table',
  star_profiles = 'star_location',
  bd_profiles = 'bd_moderator',
  unit_stability = 'unit_stability'
)

# Compare the declared primary and supplemental frames
checks <- lapply(seq_len(nrow(registry)), function(i) {
  z <- registry[i, ]
  p <- file.path(.replication_root, z$expected)
  stopifnot(identical(rp_sha256(p), z$sha256))
  expected <- rp_read_csv(p)
  actual <- rp_read_csv(rp_path('data-derived',
    'applications',
    paste0(files[[z$name]], '.csv'),
    must_exist = TRUE
  ))
  key <- switch(z$name,
    sensitivity = c('comparison', 'app', 'prior_label'),
    star_profiles = c('execution_fit_key', 'moderator', 'level'),
    bd_profiles = c('execution_fit_key', 'moderator'),
    'execution_fit_key'
  )
  result <- sim_compare(actual, expected, key, tolerance = 1e-10)
  result$artifact <- z$name
  result
})
result <- do.call(rbind, checks)

# Save the comparison record
rp_write_csv(
  result,
  rp_output_path('outputs', 'verification', 'application_summary_checks.csv')
)
if (!all(result$passed)) stop('Application summary verification failed.', call. = FALSE)
cat(nrow(result), 'application column checks passed.\n')
