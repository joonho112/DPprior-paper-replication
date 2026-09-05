#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Select the Dual-Anchor prior
#
# Recalculate all 29 candidates on the prespecified lambda grid. The .40
# threshold triggers adjustment; .25 is the soft target. Select the largest
# verified eligible lambda, retaining as much emphasis on the count judgments
# as the rule permits.
#
# Run: Rscript calibration/03_dual_anchor.R
# Reads: calibration/policy.yml; uses the J=50, mean=5, variance=10 example.
# Writes: data-derived/calibration/dual_anchor_grid.csv and the selection
# receipt.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'policy_helpers.R'))

# Specify the target and fixed selection rule
policy <- pcv_load_policy(rp_path('calibration', 'policy.yml', must_exist = TRUE))
spec <- list(spec_id = 'worked-J50', J = 50L, mu_K = 5, var_K = 10)
k_fit <- pcv_fit_k_target(spec, policy)

# Fit and classify the complete lambda grid
selected <- pcv_soft_curve_select(k_fit, spec, policy)
curve <- selected$curve
stopifnot(nrow(curve) == 29L, identical(as.numeric(curve$lambda), policy$dual$lambda_grid))
# Independently expose the fixed normalization used in every candidate's loss.
# A missing candidate stays missing; it is never replaced by a successful row.

# Use one fixed normalization throughout the grid; the loss components then
# retain the meaning defined in the policy.
curve$count_scale_mean <- max(spec$mu_K, 1)
curve$count_scale_variance <- max(spec$var_K, 1)
curve$recomputed_count_loss <- ((curve$mu_K - spec$mu_K) / curve$count_scale_mean)^2 +
  ((curve$var_K - spec$var_K) / curve$count_scale_variance)^2
curve$recomputed_weight_loss <- (curve$achieved_weight - policy$dual$soft_target)^2
curve$recomputed_total_loss <- curve$lambda * curve$recomputed_count_loss +
  (1 - curve$lambda) * curve$recomputed_weight_loss

# Save all candidates before extracting the selected fit
rp_write_csv(curve, rp_output_path('data-derived', 'calibration', 'dual_anchor_grid.csv'))
if (is.null(selected$fit)) stop(selected$rejection_code, call. = FALSE)
f <- selected$fit
rp_write_json(
  list(
    spec = spec, selected_lambda = selected$selected_lambda,
    selection_reason = selected$selection_reason, parameters = f$parameters,
    status = f$status, verified = f$verified, verification = f$verification,
    policy_sha256 = rp_sha256(rp_path('calibration', 'policy.yml'))
  ),
  rp_output_path('outputs', 'calibration', 'dual_anchor_receipt.json')
)
print(curve[curve$selected, ], row.names = FALSE)
