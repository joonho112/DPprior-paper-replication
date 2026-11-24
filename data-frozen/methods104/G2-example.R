library(DPprior)
source("calibration/policy_helpers.R")
policy <- pcv_load_policy("calibration/policy.yml")
spec <- list(spec_id = "worked_J50_mu5_v10",
             J = 50L, mu_K = 5, var_K = 10)

fit <- DPprior_fit(J = spec$J, mu_K = spec$mu_K, var_K = spec$var_K,
                  method = "A2-MN", M = 160,
                  check_diagnostics = TRUE)
stopifnot(identical(fit$status, "converged"),
          isTRUE(fit$usable), isTRUE(fit$verified))
p_majority <- prob_wsb_exceeds(
  0.5, fit$parameters$a, fit$parameters$b)

if (p_majority > policy$dual$trigger_above) {
  selected <- pcv_soft_curve_select(fit, spec, policy)
  stopifnot(is.na(selected$rejection_code))
  primary <- selected$fit
  print(selected$curve)
} else {
  primary <- fit
}
primary$parameters[c("a", "b")]

hard <- pcv_fit_hard_sensitivity(fit, spec, policy)
print(hard$gate)
stopifnot(isTRUE(hard$gate$ok))
