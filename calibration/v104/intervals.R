#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Recompute all nine OSM E.1 rows from unrounded Gamma parameters.
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)[1]
.script_dir <- dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", file_arg), fixed = TRUE)))
source(file.path(.script_dir, "..", "..", "common", "R", "bootstrap.R"))
priors <- read.csv(rp_path("data-frozen", "methods104", "application-priors.csv"))
stopifnot(nrow(priors) == 9L, !anyDuplicated(priors[c("application", "prior_role")]),
          as.character(packageVersion("DPprior")) == "2.0.0")
log_stirling <- DPprior::compute_log_stirling(max(priors$J))

# Independent calculation conditional on alpha. At observation i the
# probability of creating a cluster is alpha/(alpha+i-1). Higher counts
# cannot subsequently return to the stated interval.
conditional_mass <- function(alpha, J, lower, upper) {
  prob <- matrix(0, length(alpha), upper)
  prob[, 1L] <- 1
  for (i in 2:J) {
    p_new <- alpha / (alpha + i - 1)
    prob <- (1 - p_new) * prob + p_new * cbind(0, prob[, -upper, drop = FALSE])
  }
  rowSums(prob[, lower:upper, drop = FALSE])
}
summaries <- lapply(seq_len(nrow(priors)), function(i) {
  r <- priors[i, ]
  a <- r$gamma_shape
  b <- r$gamma_rate
  J <- r$J
  k <- 0:J
  in_interval <- k >= r$stated_interval_lower & k <= r$stated_interval_upper
  pmf <- DPprior::pmf_K_marginal(J, a, b, log_stirling, M = 160L, M_verify = 320L, strict = TRUE)
  verified_pmf <- attr(pmf, ".marginal_verification_pmf")
  stopifnot(length(verified_pmf) == length(pmf))
  mass <- sum(pmf[in_interval])
  verify <- sum(verified_pmf[in_interval])
  adaptive <- integrate(function(u) conditional_mass(qgamma(u, shape = a, rate = b), J,
                                                      r$stated_interval_lower, r$stated_interval_upper),
                        0, 1, rel.tol = 1e-8, abs.tol = 1e-10, subdivisions = 1000L)
  stopifnot(abs(mass - verify) < 1e-8, abs(mass - adaptive$value) < 1e-7)
  mean <- sum(k * pmf)
  data.frame(r, mean_count = mean, variance_count = sum((k - mean)^2 * pmf),
             interval_mass = mass, interval_mass_M320 = verify,
             independent_adaptive_mass = adaptive$value)
})
rp_write_csv(do.call(rbind, summaries), rp_output_path("data-derived", "methods104", "intervals.csv"))
