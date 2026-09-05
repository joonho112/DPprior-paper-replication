#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Prepare the main simulation and paired contrasts
#
# Select the primary N(0,1) grand-mean branch and attach its retained-fit
# denominators. Compare alternative grand-mean priors only on shared retained
# replicates with identical generated data. N(0,100) denotes variance 100.
#
# Run after 01_reaggregate.R: Rscript simulation/02_primary_and_paired.R
# Writes: data-derived/simulation/headline_*.csv and paired comparison checks.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'aggregation.R'))
source(file.path(.script_dir, 'inputs.R'))

# Load the reconstructed cells and underlying retained fits
x <- sim_read_inputs()
cells <- rp_read_csv(rp_path('data-derived', 'simulation', 'cell_summary.csv', must_exist = TRUE))

# Select the main grand-mean branch
branches <- simv2_synthesis_headline_table(cells)
primary <- branches[branches$grand_mean_label == 'N(0,1)', ]
fits <- x$fits[x$fits$is_headline & x$fits$grand_mean_label == 'N(0,1)', ]
stopifnot(
  nrow(primary) == 45L, sum(primary$n_manifest) == 9000L,
  sum(primary$n_scientific_eligible) == 8607L, nrow(fits) == 8607L
)

# Attach condition denominators to the fit-level plotting frame
pos <- match(
  paste(fits$suite, fits$conceptual_cell_key),
  paste(cells$suite, cells$conceptual_cell_key)
)
for (n in c(
  'n_manifest', 'n_runnable', 'n_scientific_eligible', 'n_dependency_excluded',
  'n_diagnostic_excluded',
  'eligible_fraction_runnable',
  'retention_below_90pct'
)) fits[[n]] <- cells[[n]][pos]

# Compute contrasts on matching retained replicates
paired <- simv2_synthesis_grand_mean_contrasts(x$fits)
expected <- rp_read_csv(rp_path('verification',
  'expected',
  'simulation_paired_contrasts.csv',
  must_exist = TRUE
))
checks <- sim_compare(
  paired,
  expected,
  c('conceptual_cell_key', 'left_grand_mean', 'right_grand_mean', 'metric')
)
rp_write_csv(
  checks,
  rp_output_path('outputs', 'verification', 'simulation_paired_checks.csv')
)
if (!all(checks$passed)) stop('Paired contrasts differ.', call. = FALSE)

# Save the branch, fit and contrast frames
for (n in c('branches', 'primary', 'fits', 'paired')) rp_write_csv(
  get(n),
  rp_output_path('data-derived', 'simulation', paste0('headline_', n, '.csv'))
)
cat('Primary 45 cells / 8607 retained fits; paired contrasts:', nrow(paired), 'rows.\n')
