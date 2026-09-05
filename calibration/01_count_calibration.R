#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Recalculate the count-only worked example
#
# For J=50, target a count mean of 5 and variance of 10. We keep the A1
# starting value beside the exact TSMM solution so readers can see why
# initialization and an accepted calibration are different stages.
#
# Run: Rscript calibration/01_count_calibration.R
# Reads: calibration/policy.yml and the pinned DPprior package.
# Writes: data-derived/calibration/count_calibration.csv, Newton trace and
# receipt.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'policy_helpers.R'))
if (length(commandArgs(trailingOnly = TRUE))) stop('This worked example takes no arguments.', call. = FALSE)

# Specify the count judgments
policy <- pcv_load_policy(rp_path('calibration', 'policy.yml', must_exist = TRUE))
spec <- list(spec_id = 'worked-J50', J = 50L, mu_K = 5, var_K = 10)

# Compare the starting value with exact calibration
initializer <- DPprior::DPprior_a1(J = spec$J, mu_K = spec$mu_K, var_K = spec$var_K)
fit <- pcv_fit_k_target(spec, policy)
# The public high-level API chooses a verification order twice M. Confirm the
# recorded order explicitly rather than passing an unsupported ... argument.
stopifnot(fit$diagnostics$M == 160L, fit$diagnostics$M_verify >= 320L)
params <- rbind(
  data.frame(
    stage = 'A1 initializer',
    a = initializer$parameters$a,
    b = initializer$parameters$b
  ),
  data.frame(stage = 'TSMM exact', a = fit$parameters$a, b = fit$parameters$b)
)

# Evaluate both priors at the same quadrature orders
rows <- lapply(seq_len(nrow(params)), function(i) {
  z <- params[i, ]
  moments <- DPprior::K_moments(spec$J, z$a, z$b, M = 160L, M_verify = 320L, strict = TRUE)
  data.frame(
    stage = z$stage, J = spec$J, target_mean = spec$mu_K, target_variance = spec$var_K,
    a = z$a, b = z$b, mean_K = moments[["mean"]], variance_K = moments[["var"]],
    mean_error = moments[["mean"]] - spec$mu_K,
    variance_error = moments[["var"]] - spec$var_K,
    fit_order = 160L, verify_order = 320L,
    accepted_for_analysis = z$stage == 'TSMM exact', stringsAsFactors = FALSE
  )
})

# Save the numerical comparison and optimizer trace
result <- do.call(rbind, rows)
rp_write_csv(
  result,
  rp_output_path('data-derived', 'calibration', 'count_calibration.csv')
)
rp_write_csv(
  fit$computation$trace,
  rp_output_path('data-derived', 'calibration', 'newton_trace.csv')
)
rp_write_json(
  list(
    spec = spec, status = fit$status, usable = fit$usable, verified = fit$verified,
    independent_verification = fit$verification,
    policy_sha256 = rp_sha256(rp_path('calibration', 'policy.yml'))
  ),
  rp_output_path('outputs', 'calibration', 'count_receipt.json')
)
print(result, row.names = FALSE)
