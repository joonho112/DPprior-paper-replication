#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Internal build inside the transaction's empty output tree.
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '',
  grep('^--file=', commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
setwd(.replication_root)
run <- function(path) {
  status <- system2(file.path(R.home('bin'), 'Rscript'), shQuote(file.path(.replication_root, path)))
  if (status != 0L) stop('Rebuild failed at ', path, call. = FALSE)
}
for (path in c('simulation/01_reaggregate.R', 'simulation/02_primary_and_paired.R',
  'simulation/03_misspecification.R', 'simulation/04_design_size.R', 'simulation/05_unequal_weights.R',
  'simulation/06_fixed_lambda.R', 'applications/03_summaries.R', 'applications/04_sensitivity.R',
  'applications/06_supplemental.R', 'rasch/01_wle.R', 'rasch/02_posterior_summaries.R',
  'rasch/03_density.R', 'calibration/v104/build.R')) run(path)
for (module in c('02_simulation.R', '03_applications.R', '04_rasch.R'))
  source(file.path(.script_dir, module), local = new.env(parent = globalenv()))
for (path in c('simulation/v104/build.R', 'applications/v104/build.R', 'exhibits/05_key_numbers.R'))
  run(path)
