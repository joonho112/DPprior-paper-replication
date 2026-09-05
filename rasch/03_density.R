#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Prepare the model-density and count summaries
#
# Check the stored posterior mean model densities, pointwise intervals and
# count PMFs. These curves describe the fitted population model; they are not
# a kernel density of shrunken person posterior means.
#
# Run after 02_posterior_summaries.R: Rscript rasch/03_density.R
# Writes: density.csv, pmf.csv, gaussian.csv and numerical mass checks.
# Original posterior bands are retained, not recalculated from individual
# draws.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'inputs.R'))
rasch_check_exports()

# Load the passing fits and their density/count summaries
post <- rp_read_csv(
  rp_path('data-derived', 'rasch', 'posterior_reportable.csv', must_exist = TRUE),
  key = c('arm', 'role')
)
density <- rasch_read('model_density', c('arm', 'role', 'x'))
pmf <- rasch_read('count_pmf', c('arm', 'role', 'K'))
gaussian <- rasch_read('gaussian_density', 'x')
diag <- rasch_read('gaussian_diagnostics', c('attempt', 'group', 'variable'))
ids <- paste(post$arm, post$role)
stopifnot(
  setequal(paste(density$arm, density$role), ids),
  setequal(paste(pmf$arm, pmf$role), ids),
  all(diag$gate_pass[diag$gated]), !any(diag$group == 'theta')
)

# Check each density grid and the associated count law
rows <- lapply(seq_len(nrow(post)), function(i) {
  f <- post[i, ]
  d <- density[density$arm == f$arm & density$role == f$role, ]
  d <- d[order(d$x), ]
  p <- pmf[pmf$arm == f$arm & pmf$role == f$role, ]
  stopifnot(
    all(is.finite(as.matrix(d[c('x', 'mean', 'lower', 'upper')]))),
    all(d$mean >= 0), all(d$lower >= 0), all(d$upper >= d$lower), all(diff(d$x) > 0),
    abs(sum(p$prob) - 1) < 1e-10, all(p$prob >= 0), isTRUE(f$mass_check_pass)
  )

  # Integrate adjacent grid values with the trapezoidal rule.
  integral <- sum(diff(d$x) * (head(d$mean, -1) + tail(d$mean, -1)) / 2)
  # Pointwise credible bands need not contain the posterior mean everywhere;
  # their lower and upper endpoints must be ordered and nonnegative.
  stopifnot(
    abs(integral - f$density_mean_integral_trapezoid) < 1e-8,
    nrow(d) == f$density_grid_n, abs(sum(p$K * p$prob) - f$E_K) < 1e-8
  )
  data.frame(
    arm = f$arm, role = f$role, grid_points = nrow(d), trapezoid_integral = integral,
    reported_mass_outside = f$mass_outside_grid_mean,
    original_mass_check_pass = f$mass_check_pass,
    count_pmf_mass = sum(p$prob), count_pmf_mean = sum(p$K * p$prob)
  )
})
checks <- do.call(rbind, rows)
stopifnot(
  all(is.finite(as.matrix(gaussian))), all(gaussian$lower >= 0),
  all(gaussian$upper >= gaussian$lower), all(diff(gaussian$x) > 0)
)

# Check the Gaussian comparison on the same population-density interpretation
gmass <- sum(diff(gaussian$x) * (head(gaussian$mean, -1) + tail(gaussian$mean, -1)) / 2)
stopifnot(abs(gmass - 1) < .01)

# Save the checked curves and the scope of reconstruction
for (n in c('density', 'pmf', 'gaussian')) rp_write_csv(
  get(n),
  rp_output_path('data-derived', 'rasch', paste0(n, '.csv'))
)
rp_write_csv(checks, rp_output_path('outputs', 'verification', 'rasch_density_checks.csv'))
rp_write_json(
  list(
    density_estimand = 'Posterior mean mixing/model density with original pointwise uncertainty bands',
    person_EAP_density = FALSE, passing_DP_fits = 6, gaussian_grid_integral = gmass,
    posterior_refit_performed = FALSE, pointwise_intervals_recomputed_from_draws = FALSE
  ),
  rp_output_path('outputs', 'rasch', 'density_receipt.json')
)
print(checks, row.names = FALSE)
