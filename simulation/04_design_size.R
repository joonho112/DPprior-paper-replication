#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Compare complete trajectories across study sizes
#
# Retain conditions observed at all four values of J so a changing set of
# conditions does not drive the comparison. Save the J=200 rows separately to
# make their retained-fit count easy to inspect.
#
# Run after 01_reaggregate.R: Rscript simulation/04_design_size.R
# Writes: design_size_balanced.csv and design_size_J200.csv.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'aggregation.R'))

# Read the condition summaries
cells <- rp_read_csv(rp_path('data-derived', 'simulation', 'cell_summary.csv', must_exist = TRUE))

# Require the same comparison across all four study sizes
balanced <- simv2_synthesis_balanced_design_size(cells)
stopifnot(all(vapply(
  split(balanced$J, paste(balanced$prior, balanced$K_true, balanced$I_target)),
  function(x) setequal(x, c(25, 50, 100, 200)), logical(1)
)))

# Save the J=200 denominator alongside the trajectory frame
all_J200 <- cells[cells$suite == 'fallback' & cells$J == 200, ]
rp_write_csv(
  balanced,
  rp_output_path('data-derived', 'simulation', 'design_size_balanced.csv')
)
rp_write_csv(
  all_J200,
  rp_output_path('data-derived', 'simulation', 'design_size_J200.csv')
)
cat(
  'Complete design trajectories:',
  nrow(balanced),
  'rows; J=200 retained fits:',
  sum(all_J200$n_scientific_eligible),
  '\n'
)
