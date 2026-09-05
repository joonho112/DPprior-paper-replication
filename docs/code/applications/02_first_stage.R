#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Rebuild the STAR first stage and normal benchmarks
#
# Recalculate primary and HC3 school contrasts and compare each with the
# frozen analysis input. Fit the normal random-effects benchmarks to STAR and
# writing-to-learn after input agreement is established.
#
# Run: Rscript applications/02_first_stage.R
# Requires the public-data dependency profile.
# Writes: school inputs, uncertainty summaries and normal-RE results.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'data_loaders.R'))
source(file.path(.script_dir, 'first_stage.R'))

# Estimate and check both STAR first-stage specifications
x <- app_star_first_stage()
for (n in c('star_multilevel', 'star_hc3')) {
  app_input_parity(x$variants[[n]], n)
  rp_write_csv(
    x$variants[[n]],
    rp_output_path('data-derived', 'applications', paste0(n, '.csv'))
  )
}
rp_write_csv(
  x$uncertainty,
  rp_output_path('data-derived', 'applications', 'star_uncertainty.csv')
)

# Recalculate the two normal random-effects benchmarks
bd <- app_load_bd()
bench <- list(
  star = app_normal_benchmark(x$variants$star_multilevel, 'star'),
  bangert_drowns = app_normal_benchmark(bd, 'bangert_drowns')
)
for (n in c('summary', 'units')) rp_write_csv(
  do.call(rbind, lapply(bench, `[[`, n)),
  rp_output_path('data-derived', 'applications', paste0('normal_re_', n, '.csv'))
)
cat('STAR primary/HC3 inputs match; normal RE benchmarks recalculated.\n')
