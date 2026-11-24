#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Compare the existing November build with independent references and its receipts.
script <- sub('^--file=', '', grep('^--file=', commandArgs(FALSE), value = TRUE)[1])
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', script, fixed = TRUE)))
source(file.path(.script_dir, 'common', 'R', 'bootstrap.R'))
setwd(.replication_root)
proof <- rp_output_path('outputs', 'verification', 'reproduction.json')
if (file.exists(proof)) unlink(proof)
expected <- rp_read_csv(rp_path('provenance', 'expected-hashes.csv'), key = 'path')
for (i in seq_len(nrow(expected))) {
  if (!identical(rp_sha256(file.path(.replication_root, expected$path[i])), expected$sha256[i]))
    stop('Independent reference changed: ', expected$path[i], call. = FALSE)
}
checks <- c('verification/check_inputs.py', 'verification/check_simulation.R',
            'verification/check_applications.R', 'verification/check_rasch.R',
            'verification/check_retained.R', 'verification/check_application_grids.py',
            'verification/v104/methods.py', 'verification/v104/simulation.py',
            'verification/v104/applications.py', 'verification/check_exhibits.R')
for (path in checks) {
  executable <- if (endsWith(path, '.R')) file.path(R.home('bin'), 'Rscript') else Sys.which('python3')
  if (system2(executable, shQuote(file.path(.replication_root, path))) != 0L)
    stop('Verification failed at ', path, call. = FALSE)
}
rp_write_json(list(schema='v104-verification/1', passed=TRUE, artifacts=62L, checks=checks,
                   expected_manifest_sha256=rp_sha256(rp_path('provenance', 'expected-hashes.csv')),
                   registry_sha256=rp_sha256(rp_path('outputs', 'exhibit-registry.csv')),
                   calculation_scope='Reconstruction from the frozen inputs; refitting is a separate path.',
                   comparison_notes='provenance/manuscript-discrepancies.csv'), proof)
cat('Verified 62 November exhibits. See outputs/verification/reproduction.json.\n')
