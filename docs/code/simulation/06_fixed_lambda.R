#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Compare selected and fixed trade-off weights
#
# Place the policy-selected lambda beside the prespecified .5 and .9
# sensitivity settings. The fixed settings do not undergo a new selection
# step.
#
# Run after 01_reaggregate.R: Rscript simulation/06_fixed_lambda.R
# Writes: data-derived/simulation/fixed_lambda.csv.

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

# Keep selected and fixed lambda settings together
result <- simv2_synthesis_lambda_table(cells)
fixed <- result[result$design == 'D', ]
stopifnot(nrow(fixed) == 4L, setequal(fixed$dual_lambda, c(.5, .9)))

# Save the two-by-three sensitivity grid
rp_write_csv(result, rp_output_path('data-derived', 'simulation', 'fixed_lambda.csv'))
cat('Two informativeness levels x three lambda choices:', nrow(result), 'rows.\n')
