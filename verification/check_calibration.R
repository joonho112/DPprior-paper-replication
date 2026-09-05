#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Check the worked prior calculations
#
# Verify the count target, weight tails, Dual-Anchor choice and hard-
# constraint example against the recorded numerical tolerances. Also confirm
# that deliberately invalid fit objects are rejected.
#
# Run after calibration/01_count_calibration.R through 04_hard_and_failures.R.
# Command: Rscript verification/check_calibration.R
# Writes: outputs/verification/ calibration check results.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))

# Load the independent numerical reference
expected <- rp_read_csv(
  rp_path('verification', 'expected', 'calibration-paper.csv', must_exist = TRUE),
  key = 'prior'
)
count <- rp_read_csv(rp_path('data-derived',
  'calibration',
  'count_calibration.csv',
  must_exist = TRUE
))
grid <- rp_read_csv(rp_path('data-derived',
  'calibration',
  'dual_anchor_grid.csv',
  must_exist = TRUE
))
hard <- rp_read_csv(rp_path('data-derived',
  'calibration',
  'hard_sensitivity.csv',
  must_exist = TRUE
))
c <- count[count$stage == 'TSMM exact', ]
d <- grid[grid$selected, ]
stopifnot(nrow(c) == 1L, nrow(d) == 1L, nrow(hard) == 1L)

# Align the three prior solutions for comparison
actual <- rbind(
  data.frame(
    prior = 'TSMM',
    a = c$a,
    b = c$b,
    mean_K = c$mean_K,
    variance_K = c$variance_K,
    size_biased_tail = (c$b / (c$b + log(2)))^c$a
  ),
  data.frame(
    prior = 'Dual-Anchor',
    a = d$a,
    b = d$b,
    mean_K = d$mu_K,
    variance_K = d$var_K,
    size_biased_tail = d$achieved_weight
  ),
  data.frame(
    prior = 'Dual-Anchor (hard)', a = hard$a, b = hard$b, mean_K = hard$prior_EK,
    variance_K = hard$prior_varK, size_biased_tail = hard$p_wsb_gt_05
  )
)

# Collect discrepancies using the recorded tolerances
checks <- list()
add <- function(name, observed, expected, tol) {
  checks[[length(checks) + 1L]] <<- data.frame(
    check = name,
    observed = observed,
    expected = expected,
    tolerance = tol, passed = is.finite(observed) && abs(observed - expected) <= tol
  )
}
for (i in seq_len(nrow(expected))) for (n in names(expected)[-1])
  add(
    paste(expected$prior[i], n, sep = ':'), actual[[n]][i], expected[[n]][i],
    if (n %in% c('a', 'b')) 5e-5 else 5e-4
  )
# Conditional CRP counts are a sum of Bernoulli variables. Integrating their
# first two raw moments against a Gamma density verifies the quadrature result
# using a different numerical algorithm (adaptive integration, no DPprior call).
for (i in seq_len(nrow(actual))) {
  a <- actual$a[i]
  b <- actual$b[i]
  J <- 50L
  moments <- function(alpha) {
    p <- alpha / (alpha + seq_len(J - 1L))
    m <- 1 + sum(p)
    c(m, m * m + sum(p * (1 - p)))
  }
  first <- integrate(Vectorize(function(x) moments(x)[1] * dgamma(x, a, rate = b)),
    0,
    Inf,
    rel.tol = 1e-9
  )$value
  second <- integrate(Vectorize(function(x) moments(x)[2] * dgamma(x, a, rate = b)),
    0,
    Inf,
    rel.tol = 1e-9
  )$value
  add(paste(actual$prior[i], 'independent_mean', sep = ':'), first, actual$mean_K[i], 1e-6)
  add(
    paste(actual$prior[i], 'independent_variance', sep = ':'),
    second - first^2,
    actual$variance_K[i],
    1e-6
  )
}

# Check the selection rule and all three loss components
add('grid_size', nrow(grid), 29, 0)
add('selected_lambda', d$lambda, .30, 1e-12)
add('largest_eligible_lambda', d$lambda, max(grid$lambda[grid$soft_policy_eligible]), 0)
for (pair in list(
  c('K_loss', 'recomputed_count_loss'),
  c('weight_loss', 'recomputed_weight_loss'),
  c('total_loss', 'recomputed_total_loss')
)) {
  use <- is.finite(grid[[pair[1]]]) & is.finite(grid[[pair[2]]])
  add(
    paste('loss', pair[1], sep = ':'),
    max(abs(grid[[pair[1]]][use] - grid[[pair[2]]][use])),
    0,
    1e-8
  )
}
w <- rp_read_csv(rp_path('data-derived',
  'calibration',
  'weight_diagnostics.csv',
  must_exist = TRUE
))
add('weight_bounds', sum(w$largest_population_weight_tail >= w$proposition_lower &
  w$largest_population_weight_tail <= w$proposition_upper), 2, 0)
failures <- rp_read_csv(rp_path('data-derived',
  'calibration',
  'failure_checks.csv',
  must_exist = TRUE
))
add(
  'failed_results_rejected',
  sum(failures$accepted[failures$case != 'hard_active_constraint_allowed']),
  0,
  0
)

# Write the full check table before stopping on any failure
result <- do.call(rbind, checks)
rp_write_csv(result, rp_output_path('outputs', 'verification', 'calibration_checks.csv'))
print(result, row.names = FALSE)
if (!all(result$passed)) stop('Calibration verification failed; see receipt.', call. = FALSE)
