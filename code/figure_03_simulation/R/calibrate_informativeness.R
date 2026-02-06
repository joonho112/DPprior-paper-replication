# ==============================================================================
# calibrate_informativeness.R — Calibrate sample sizes to achieve target
#                                informativeness levels
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Paper:    "Design-Conditional Prior Elicitation for DP Mixtures"
# Author:   JoonHo Lee
#
# Description:
#   Informativeness I = Var(tau) / (Var(tau) + GM(se^2)), where GM is the
#   geometric mean of within-site sampling variances. This module calibrates
#   the mean sample size (n_mean) so that E[I_realized] ≈ I_target for each
#   informativeness level in the simulation grid (e.g., I = 0.2, 0.5, 0.8).
#
# Functions:
#   gamma_shape_rate_from_mean_cv() — Derive Gamma parameters from mean and CV
#   sample_n_j()                    — Sample site-level sample sizes
#   geometric_mean()                — Compute geometric mean
#   compute_informativeness()       — Compute I from sigma_tau^2 and se^2 vector
#   estimate_mean_I()               — Monte Carlo estimate of E[I] for given n_mean
#   analytic_n_mean_guess()         — Analytic initial guess for n_mean
#   calibrate_n_mean()              — Grid search for optimal n_mean
#   run_calibration_table()         — Run full calibration for all I targets
#
# Dependencies: base R
# Called by:    scripts/step1_calibrate_I.R
# ==============================================================================

gamma_shape_rate_from_mean_cv <- function(mean, cv) {
  stopifnot(mean > 0, cv > 0)
  shape <- 1 / (cv^2)
  rate <- shape / mean
  list(shape = shape, rate = rate)
}

sample_n_j <- function(J, n_mean, cv = 0.5, n_min = 5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  pars <- gamma_shape_rate_from_mean_cv(n_mean, cv)
  n_raw <- rgamma(J, shape = pars$shape, rate = pars$rate)
  n_trunc <- pmax(n_raw, n_min)
  n_int <- as.integer(round(n_trunc))
  n_int[n_int < n_min] <- n_min
  n_int
}

geometric_mean <- function(x) {
  stopifnot(all(x > 0))
  exp(mean(log(x)))
}

compute_informativeness <- function(sigma_tau2, se2_vec) {
  gm <- geometric_mean(se2_vec)
  sigma_tau2 / (sigma_tau2 + gm)
}

estimate_mean_I <- function(J, n_mean, sigma_tau, cv, n_min, kappa,
                            n_sims = 2000, seed = 123) {
  set.seed(seed)
  sigma_tau2 <- sigma_tau^2
  I_vals <- numeric(n_sims)

  for (s in seq_len(n_sims)) {
    n_j <- sample_n_j(J, n_mean, cv = cv, n_min = n_min)
    se2 <- kappa / n_j          # se^2 = kappa / n_j
    I_vals[s] <- compute_informativeness(sigma_tau2, se2)
  }

  list(mean_I = mean(I_vals), sd_I = sd(I_vals), I_vals = I_vals)
}

analytic_n_mean_guess <- function(I_target, sigma_tau, cv, kappa = 4) {
  sigma2 <- sigma_tau^2
  gm_se2_target <- sigma2 * (1 - I_target) / I_target
  gm_n_target <- kappa / gm_se2_target

  shape <- 1 / (cv^2)
  gm_factor <- exp(digamma(shape)) / shape  # GM-to-mean ratio for Gamma
  mean_guess <- gm_n_target / gm_factor
  max(mean_guess, 1)
}

calibrate_n_mean <- function(I_target, J, sigma_tau, cv, n_min, kappa,
                             n_sims = 2000, seed = 123,
                             grid_mult_low = 0.5, grid_mult_high = 2.0,
                             grid_size = 31, tol_abs = 0.01) {

  n0 <- analytic_n_mean_guess(I_target, sigma_tau, cv, kappa)
  n0 <- max(n0, n_min + 1)

  mult_grid <- exp(seq(log(grid_mult_low), log(grid_mult_high), length.out = grid_size))
  cand <- n0 * mult_grid

  best <- NULL
  for (i in seq_along(cand)) {
    est <- estimate_mean_I(J, cand[i], sigma_tau, cv, n_min, kappa, n_sims, seed)
    diff <- abs(est$mean_I - I_target)
    if (is.null(best) || diff < best$diff) {
      best <- list(n_mean = cand[i], mean_I = est$mean_I, sd_I = est$sd_I, diff = diff)
    }
    if (diff <= tol_abs) break
  }

  # Widen search if first pass did not converge
  if (best$diff > tol_abs) {
    mult_grid2 <- exp(seq(log(grid_mult_low / 2), log(grid_mult_high * 2), length.out = grid_size))
    cand2 <- n0 * mult_grid2
    for (i in seq_along(cand2)) {
      est <- estimate_mean_I(J, cand2[i], sigma_tau, cv, n_min, kappa, n_sims, seed)
      diff <- abs(est$mean_I - I_target)
      if (diff < best$diff) {
        best <- list(n_mean = cand2[i], mean_I = est$mean_I, sd_I = est$sd_I, diff = diff)
      }
      if (diff <= tol_abs) break
    }
  }

  best
}

run_calibration_table <- function(cfg) {
  J       <- as.integer(cfg$design$J)
  I_grid  <- as.numeric(cfg$design$I_grid)

  sigma_tau <- as.numeric(cfg$dgp$sigma_tau)
  cv        <- as.numeric(cfg$dgp$n_cv)
  n_min     <- as.integer(cfg$dgp$n_min)
  kappa     <- as.numeric(cfg$dgp$kappa)

  n_sims <- as.integer(cfg$calibration$n_sims)
  seed   <- as.integer(cfg$seeds$calibration)

  grid_mult_low  <- as.numeric(cfg$calibration$grid_mult_low)
  grid_mult_high <- as.numeric(cfg$calibration$grid_mult_high)
  grid_size      <- as.integer(cfg$calibration$grid_size)
  tol_abs        <- as.numeric(cfg$calibration$tol_abs)

  out <- data.frame(
    I_target  = I_grid,
    J         = J,
    sigma_tau = sigma_tau,
    n_cv      = cv,
    n_min     = n_min,
    kappa     = kappa,
    n_sims    = n_sims,
    seed      = seed,
    n_mean    = NA_real_,
    mean_I    = NA_real_,
    sd_I      = NA_real_,
    abs_error = NA_real_
  )

  for (i in seq_along(I_grid)) {
    best <- calibrate_n_mean(
      I_target = I_grid[i],
      J = J, sigma_tau = sigma_tau,
      cv = cv, n_min = n_min, kappa = kappa,
      n_sims = n_sims, seed = seed,
      grid_mult_low = grid_mult_low,
      grid_mult_high = grid_mult_high,
      grid_size = grid_size,
      tol_abs = tol_abs
    )
    out$n_mean[i]    <- best$n_mean
    out$mean_I[i]    <- best$mean_I
    out$sd_I[i]      <- best$sd_I
    out$abs_error[i] <- best$diff
  }

  out
}
