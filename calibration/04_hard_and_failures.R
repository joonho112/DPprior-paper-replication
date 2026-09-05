#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Evaluate the hard constraint and rejection rules
#
# Fit the separate hard .25 sensitivity and exercise the acceptance checks
# with deliberately altered copies. A failed candidate stays a failure; it is
# not replaced with default hyperparameters.
#
# Run: Rscript calibration/04_hard_and_failures.R
# Reads: calibration/policy.yml and the worked count target.
# Writes: hard_sensitivity.csv, failure_checks.csv and confidence_mapping.csv.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'policy_helpers.R'))

# Fit the hard-constraint sensitivity
policy <- pcv_load_policy(rp_path('calibration', 'policy.yml', must_exist = TRUE))
spec <- list(spec_id = 'worked-J50', J = 50L, mu_K = 5, var_K = 10)
k_fit <- pcv_fit_k_target(spec, policy)
hard <- pcv_fit_hard_sensitivity(k_fit, spec, policy)
if (!isTRUE(hard$gate$ok)) stop(paste(hard$gate$reasons, collapse = ';'), call. = FALSE)
f <- hard$fit
summary <- pcv_prior_summary(spec$J, f$parameters$a, f$parameters$b)
rp_write_csv(
  cbind(data.frame(
    prior = 'Dual-Anchor (hard)',
    a = f$parameters$a,
    b = f$parameters$b,
    boundary_class = hard$gate$boundary$class
  ), as.data.frame(summary)),
  rp_output_path('data-derived', 'calibration', 'hard_sensitivity.csv')
)
rp_write_json(
  list(
    status = f$status, verified = f$verified, constraint = f$constraint,
    gate = hard$gate, verification = f$verification
  ),
  rp_output_path('outputs', 'calibration', 'hard_receipt.json')
)
# These deliberately corrupted copies test acceptance behavior, not the optimizer.

# Check rejection of invalid fit objects
cases <- list(
  missing_result = NULL, unverified = k_fit, approximate = k_fit,
  nonpositive_parameters = k_fit, unclassified_boundary = k_fit
)
cases$unverified$verified <- FALSE
cases$approximate$provenance$approximation$active <- TRUE
cases$nonpositive_parameters$parameters$a <- 0
cases$unclassified_boundary$status <- 'boundary'
checks <- lapply(names(cases), function(n) {
  g <- pcv_gate_fit(cases[[n]], policy)
  data.frame(case = n, accepted = g$ok, reason = paste(g$reasons, collapse = ';'))
})
checks[[length(checks) + 1L]] <- data.frame(
  case = 'hard_active_constraint_allowed',
  accepted = hard$gate$ok, reason = hard$gate$boundary$class
)
# Restrict this separate failure test to lambda=1: the count-only candidate
# cannot leave the .40 risk band, so the selector must return NULL.

# Check the case with no eligible soft candidate
no_policy <- policy
no_policy$dual$lambda_grid <- 1
no_eligible <- pcv_soft_curve_select(k_fit, spec, no_policy)
stopifnot(is.null(no_eligible$fit), !any(no_eligible$curve$soft_policy_eligible))
checks[[length(checks) + 1L]] <- data.frame(
  case = 'no_eligible_candidate', accepted = FALSE,
  reason = no_eligible$rejection_code
)
checks <- do.call(rbind, checks)
stopifnot(!any(checks$accepted[checks$case != 'hard_active_constraint_allowed']))
rp_write_csv(checks, rp_output_path('data-derived', 'calibration', 'failure_checks.csv'))
# Confidence labels map to stated count variances, not to posterior certainty.

# Record how confidence labels map to count variance
confidence <- data.frame(
  confidence = c('high', 'medium', 'low'), VIF = c(1.5, 2.5, 5),
  target_mean = 5, target_variance = c(1.5, 2.5, 5) * (5 - 1)
)
rp_write_csv(
  confidence,
  rp_output_path('data-derived', 'calibration', 'confidence_mapping.csv')
)
print(checks, row.names = FALSE)
