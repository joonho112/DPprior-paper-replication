#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Check population-weight implications
#
# Evaluate the size-biased and largest-population-weight tails of the accepted
# count prior. These probabilities concern population mass, not the fraction
# of observed units in an occupied cluster.
#
# Run after 01_count_calibration.R: Rscript
# calibration/02_weight_diagnostics.R
# Reads: data-derived/calibration/count_calibration.csv.
# Writes: data-derived/calibration/weight_diagnostics.csv.
# The largest-weight integral used here is valid for thresholds at least .5.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'policy_helpers.R'))

# Load the accepted count-only parameters
counts <- rp_read_csv(
  rp_path('data-derived', 'calibration', 'count_calibration.csv', must_exist = TRUE),
  required = c('stage', 'a', 'b'), key = 'stage'
)
z <- counts[counts$stage == 'TSMM exact', ]
stopifnot(nrow(z) == 1L)

# Evaluate tails at the two stated thresholds
rows <- lapply(c(.5, .9), function(t) {
  a <- z$a
  b <- z$b
  # Gamma(shape=a, rate=b), integrated over alpha. Using x=-log(1-w)
  # avoids the endpoint singularity in the density on the weight scale.
  tail <- exp(a * (log(b) - log(b - log1p(-t))))
  largest <- integrate(function(x) a * b^a / (b + x)^(a + 1) / (-expm1(-x)),
    lower = -log1p(-t), upper = Inf, rel.tol = 1e-10
  )
  package_tail <- DPprior::prob_wsb_exceeds(t, a, b)
  stopifnot(
    abs(tail - package_tail) < 1e-12, largest$value >= tail - 1e-9,
    largest$value <= min(1, tail / t) + 1e-9
  )
  data.frame(
    threshold = t, a = a, b = b, size_biased_tail = tail,
    largest_population_weight_tail = largest$value,
    integration_abs_error = largest$abs.error,
    proposition_lower = tail, proposition_upper = min(1, tail / t),
    package_tail_difference = package_tail - tail
  )
})

# Save the weight diagnostics
result <- do.call(rbind, rows)
rp_write_csv(
  result,
  rp_output_path('data-derived', 'calibration', 'weight_diagnostics.csv')
)
print(result, row.names = FALSE)
