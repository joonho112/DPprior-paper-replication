#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Recalculate the confidence settings in Table 2
#
# Hold J=100 and the expected count at 5 while changing the stated count
# variance. Keep exact shape/rate estimates when calculating weight tails;
# rounding the parameters first can change a displayed probability.
#
# Run: Rscript calibration/05_confidence_levels.R
# Reads: calibration/policy.yml and the four VIF settings below.
# Writes: data-derived/calibration/confidence_levels.csv.
# The extra Gamma(1,1) row is an external comparison, not a calibrated
# setting.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'policy_helpers.R'))

# Hold the target mean fixed and vary its uncertainty
policy <- pcv_load_policy(rp_path('calibration', 'policy.yml', must_exist = TRUE))
rows <- lapply(c(1.25, 1.5, 2.5, 5), function(vif) {
  f <- pcv_fit_k_target(
    list(spec_id = paste0('confidence-', vif), J = 100L, mu_K = 5, var_K = 4 * vif),
    policy
  )
  a <- f$parameters$a
  b <- f$parameters$b
  data.frame(setting = paste0('VIF = ', vif), VIF = vif, target_variance = 4 * vif, a = a, b = b)
})

# Add the external default and calculate weight implications
x <- rbind(
  do.call(rbind, rows),
  data.frame(
    setting = 'Gamma(1, 1) default',
    VIF = NA,
    target_variance = NA,
    a = 1,
    b = 1
  )
)
x$mean_alpha <- x$a / x$b
x$tail_05 <- (x$b / (x$b - log(.5)))^x$a
x$tail_09 <- (x$b / (x$b - log(.1)))^x$a
x$p_alpha_lt_01 <- pgamma(.1, x$a, rate = x$b)

# Save full-precision results before printing
rp_write_csv(
  x,
  rp_output_path('data-derived', 'calibration', 'confidence_levels.csv')
)
print(x, row.names = FALSE)
