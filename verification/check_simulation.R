#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Check simulation summaries against original exports
#
# Compare the rebuilt condition and paired-comparison frames with independent
# references. Keep missing estimates, exclusion counts and the selected
# trajectory sets in the checks, not only posterior means.
#
# Run after the six simulation runners: Rscript
# verification/check_simulation.R
# Writes: outputs/verification/simulation checks and receipt.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(rp_path('simulation', 'inputs.R', must_exist = TRUE))
registry <- jsonlite::read_json(rp_path('provenance', 'simulation-frame-expected.json', must_exist = TRUE),
  simplifyVector = TRUE
)

# Compare all declared simulation frames
checks <- lapply(seq_len(nrow(registry)), function(i) {
  z <- registry[i, ]
  expected_path <- file.path(.replication_root, z$expected)
  stopifnot(identical(rp_sha256(expected_path), z$source_sha256))
  actual <- rp_read_csv(file.path(.replication_root, z$generated))
  expected <- rp_read_csv(expected_path)
  # Additional public explanatory columns are permitted; every original field
  # and row must still match. No expected values enter the generated frame.
  if (!all(names(expected) %in% names(actual))) stop('Missing original frame fields.', call. = FALSE)
  result <- sim_compare(actual[names(expected)], expected, c('suite', 'conceptual_cell_key'))
  result$artifact <- z$name
  result
})
result <- do.call(rbind, checks)

# Save the comparison record
rp_write_csv(
  result,
  rp_output_path('outputs', 'verification', 'simulation_frame_checks.csv')
)
if (!all(result$passed)) stop('Simulation frame verification failed.', call. = FALSE)
cat(nrow(result), 'frame-column checks passed.\n')
