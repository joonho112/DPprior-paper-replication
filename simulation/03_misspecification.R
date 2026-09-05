#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Prepare the count-misspecification comparisons
#
# Keep the full medium-confidence grid, its unavailable cells and the
# completed trajectories as separate outputs. A missing calibrated prior is
# recorded as an exclusion, never as a zero-valued posterior result.
#
# Run after 01_reaggregate.R: Rscript simulation/03_misspecification.R
# Writes: data-derived/simulation/misspecification_*.csv.
# The OSM table uses the balanced trajectories, not the entire 60-cell grid.

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

# Keep the complete medium-confidence audit grid
scope <- cells$suite == 'fallback' & cells$design == 'A' & cells$I_target %in% c(.2, .8) & (
  (cells$prior %in% c('TSMM', 'Dual-Anchor') & cells$VIF == 2.5 & cells$r %in% c(.5, 1, 2)) |
    (cells$prior == 'DORO-Unif' & cells$r %in% c(.5, 1, 2)) | cells$prior == 'Vague')
medium <- cells[scope, ]
blocked <- cells[cells$n_runnable == 0L, ]
stopifnot(
  nrow(medium) == 60L, sum(medium$scientific_results_available) == 56L,
  sum(medium$n_runnable == 0L) == 4L, nrow(blocked) == 14L,
  all(is.na(blocked$mean_E_K))
)

# Explain the cells without runnable priors
cal <- rp_read_csv(rp_path('data-frozen', 'simulation', 'calibration.csv', must_exist = TRUE),
  key = 'calibration_id'
)
for (n in c('calibration_outcome', 'rejection_code', 'boundary_class'))
  blocked[[n]] <- cal[[n]][match(blocked$prior_calibration_id, cal$calibration_id)]

# Select complete three-point trajectories for comparison
balanced <- simv2_synthesis_balanced_misspecification(cells)
medium_balanced <- balanced[balanced$VIF == 2.5, ]
stopifnot(all(vapply(split(balanced$r, paste(
  balanced$prior, balanced$K_true,
  balanced$I_target, balanced$VIF
)), function(x) setequal(x, c(.5, 1, 2)), logical(1))))

# Save audit and display frames separately
for (n in c('medium', 'blocked', 'balanced', 'medium_balanced')) rp_write_csv(
  get(n),
  rp_output_path('data-derived', 'simulation', paste0('misspecification_', n, '.csv'))
)
cat('Medium-confidence grid: 56/60 cells with estimates; 4 typed exclusions.\n')
