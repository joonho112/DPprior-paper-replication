#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Rebuild all current tables and figures
#
# First reconstruct the numerical summaries, then render the four exhibit
# groups and calculate the selected key numbers. Each child process must
# finish successfully before the next stage begins.
#
# Run: Rscript exhibits/00_build_all.R
# Reads: frozen public summaries through the track-specific runners.
# Writes: 41 exhibits, their numeric frames, key numbers and an output
# registry.
# This entrypoint never starts MCMC or downloads source data.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))

# Run each numerical stage in its own R process
run_script <- function(relative) {
  p <- file.path(.replication_root, relative)
  code <- system2(file.path(R.home('bin'), 'Rscript'), shQuote(p))
  if (code != 0L) stop('Rebuild failed at ', relative, call. = FALSE)
}

# Reconstruct simulation, application and Rasch summaries
for (script in c(
  'simulation/01_reaggregate.R', 'simulation/02_primary_and_paired.R',
  'simulation/03_misspecification.R',
  'simulation/04_design_size.R',
  'simulation/05_unequal_weights.R',
  'simulation/06_fixed_lambda.R',
  'applications/03_summaries.R',
  'applications/04_sensitivity.R',
  'applications/06_supplemental.R',
  'rasch/01_wle.R',
  'rasch/02_posterior_summaries.R',
  'rasch/03_density.R'
)) run_script(script)

# Render the four exhibit groups with separate working environments
for (module in c('01_methods.R', '02_simulation.R', '03_applications.R', '04_rasch.R')) {
  cat('\nBuilding ', module, '\n', sep = '')
  source(file.path(.script_dir, module), local = new.env(parent = globalenv()))
}

# Calculate the key-number ledger and index all exhibit receipts
run_script('exhibits/05_key_numbers.R')
contract <- rp_read_csv(rp_path('provenance', 'artifact-contract.csv'))
records <- lapply(contract$artifact_id, function(id) {
  p <- rp_path('outputs', 'exhibit-receipts', paste0(id, '.json'), must_exist = TRUE)
  x <- jsonlite::read_json(p, simplifyVector = TRUE)
  data.frame(
    artifact_id = id,
    tier = x$tier,
    rows = x$rows,
    receipt_sha256 = rp_sha256(p),
    status = 'built',
    note = x$note
  )
})
rp_write_csv(do.call(rbind, records), rp_output_path('outputs', 'exhibit-registry.csv'))
cat('Built ',
  length(records),
  ' current manuscript/OSM artifacts. Consult tier and discrepancy records before claiming numerical parity.\n',
  sep = ''
)
