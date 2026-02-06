# ==============================================================================
# fit_dpmixture_nimble.R — Fit DP mixture model via NIMBLE CRP sampler
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Paper:    "Design-Conditional Prior Elicitation for DP Mixtures"
# Author:   JoonHo Lee
#
# Description:
#   Fits a Dirichlet Process mixture model using NIMBLE's CRP (Chinese
#   Restaurant Process) sampler. The model is:
#     z[1:J] ~ dCRP(alpha, size = J)
#     alpha  ~ Gamma(a, b)
#   where (a, b) are the hyperprior parameters calibrated by DPprior.
#
#   Key design choice (v4): The NIMBLE model and MCMC are compiled ONCE per
#   R worker process and cached in .DP_NIMBLE_CACHE. Subsequent calls only
#   update data and initial values on the compiled objects, avoiding repeated
#   compilation overhead in large parallel simulations.
#
# Functions:
#   assert_nimble_available()      — Check nimble is installed and loaded
#   rinvgamma_simple()             — Sample from inverse-gamma distribution
#   mode_int()                     — Integer mode of a vector
#   compute_K_from_z_matrix()      — Count unique clusters per MCMC draw
#   compute_rhat_scalar()          — Gelman-Rubin R-hat for convergence check
#   make_inits_dp()                — Generate initial values for MCMC
#   make_dp_code()                 — Define NIMBLE model code
#   get_compiled_dp_runner()       — Build or retrieve cached compiled model
#   fit_dp_mixture()               — Main fitting function (public API)
#
# Dependencies: nimble
# Called by:    sim_runner.R, sim_runner_parallel.R
# ==============================================================================

# ---- Package availability ----
assert_nimble_available <- function() {
  suppressPackageStartupMessages({
    ok <- require("nimble", quietly = TRUE, character.only = TRUE)
  })
  if (!ok) {
    stop("Package 'nimble' is required. Install with install.packages('nimble').")
  }
}

# ---- Utilities ----
rinvgamma_simple <- function(n, shape, scale) {
  1 / rgamma(n, shape = shape, rate = 1 / scale)
}

mode_int <- function(x) {
  x <- as.integer(x)
  tab <- table(x)
  as.integer(names(tab)[which.max(tab)])
}

compute_K_from_z_matrix <- function(z_mat) {
  apply(z_mat, 1, function(row) length(unique(row)))
}

compute_rhat_scalar <- function(chains) {
  chains <- lapply(chains, as.numeric)
  m <- length(chains)
  if (m < 2) return(NA_real_)

  lens <- vapply(chains, length, integer(1))
  n <- min(lens)
  if (n < 2) return(NA_real_)

  chains <- lapply(chains, function(x) x[seq_len(n)])
  chain_means <- vapply(chains, mean, numeric(1))
  chain_vars  <- vapply(chains, stats::var, numeric(1))

  W <- mean(chain_vars)
  B <- n * stats::var(chain_means)
  var_hat <- ((n - 1) / n) * W + (B / n)
  sqrt(var_hat / W)
}

make_inits_dp <- function(J) {
  list(
    tau_j      = rnorm(J, 0, 1),
    tau_G0     = rnorm(J, 0, 1),
    sigma2_G0  = rinvgamma_simple(J, shape = 2, scale = 1),
    z          = sample(seq_len(min(10L, J)), size = J, replace = TRUE),
    alpha      = 1,
    tau        = 0,
    sigma2_tau = 1,
    s_shape    = 1,
    s_rate     = 1
  )
}

# ---- Compiled runner cache (per R process) ----
.DP_NIMBLE_CACHE <- new.env(parent = emptyenv())

make_dp_code <- function() {
  nimble::nimbleCode({
    for (j in 1:J) {
      tau_j_hat[j] ~ dnorm(tau_j[j], sd = se_j[j])
      tau_j[j]     ~ dnorm(tau_clust[j], var = sigma2_clust[j])
      tau_clust[j]    <- tau_G0[z[j]]
      sigma2_clust[j] <- sigma2_G0[z[j]]
    }

    for (j in 1:J) {
      tau_G0[j]    ~ dnorm(tau, var = sigma2_tau)
      sigma2_G0[j] ~ dinvgamma(s_shape, s_rate)
    }

    z[1:J] ~ dCRP(alpha, size = J)

    # a and b are treated as data nodes so we can update the hyperprior
    # parameters without recompiling the model.
    alpha ~ dgamma(a, b)

    tau        ~ dnorm(0, 100)
    sigma2_tau ~ dunif(0, 10)
    s_shape    ~ dinvgamma(2, 1)
    s_rate     ~ dinvgamma(2, 1)
  })
}

get_compiled_dp_runner <- function(J, monitors, showCompilerOutput = FALSE) {
  assert_nimble_available()

  monitors <- as.character(monitors)
  key <- paste0("J=", as.integer(J), "|mon=", paste(monitors, collapse = ","))

  if (exists(key, envir = .DP_NIMBLE_CACHE, inherits = FALSE)) {
    return(get(key, envir = .DP_NIMBLE_CACHE, inherits = FALSE))
  }

  # Placeholder data (overwritten for each fit)
  data0  <- list(tau_j_hat = rep(0, J), se_j = rep(1, J), a = 1.0, b = 1.0)
  consts <- list(J = as.integer(J))
  inits0 <- make_inits_dp(J)
  code   <- make_dp_code()

  Rmodel <- nimble::nimbleModel(
    code = code, constants = consts, data = data0, inits = inits0, check = TRUE
  )
  conf  <- nimble::configureMCMC(Rmodel, monitors = monitors)
  Rmcmc <- nimble::buildMCMC(conf)

  # Compile once — the expensive step we avoid repeating
  Cmodel <- nimble::compileNimble(Rmodel, showCompilerOutput = isTRUE(showCompilerOutput))
  Cmcmc  <- nimble::compileNimble(Rmcmc, project = Cmodel,
                                   showCompilerOutput = isTRUE(showCompilerOutput))

  runner <- list(
    J = as.integer(J), monitors = monitors, code = code,
    Rmodel = Rmodel, Cmodel = Cmodel, Cmcmc = Cmcmc
  )

  assign(key, runner, envir = .DP_NIMBLE_CACHE)
  runner
}

# ---- Main fitting function (public API) ----
fit_dp_mixture <- function(df_sim, a, b, mcmc_cfg, seed = NULL,
                           verbose = FALSE, K_true = NULL) {
  assert_nimble_available()

  J         <- nrow(df_sim)
  tau_j_hat <- df_sim$tau_hat
  se_j      <- sqrt(df_sim$se2)

  monitors <- as.character(mcmc_cfg$monitors)
  nchains  <- as.integer(mcmc_cfg$nchains)
  niter    <- as.integer(mcmc_cfg$niter)
  nburnin  <- as.integer(mcmc_cfg$nburnin)
  thin     <- as.integer(mcmc_cfg$thin)
  if (is.na(nchains) || nchains < 1L) nchains <- 1L

  # Build or reuse the compiled runner
  runner <- get_compiled_dp_runner(J = J, monitors = monitors, showCompilerOutput = FALSE)

  # Update data in the compiled model
  runner$Cmodel$tau_j_hat <- tau_j_hat
  runner$Cmodel$se_j      <- se_j
  runner$Cmodel$a         <- as.numeric(a)
  runner$Cmodel$b         <- as.numeric(b)

  t0 <- proc.time()[3]

  K_all        <- c()
  alpha_all    <- c()
  alpha_chains <- list()
  z_cols_cache <- NULL

  for (ch in seq_len(nchains)) {
    seed_ch <- if (is.null(seed)) NULL else as.integer(seed + 1000L * ch)

    if (!is.null(seed_ch)) set.seed(seed_ch)
    inits <- make_inits_dp(J)
    runner$Cmodel$setInits(inits)
    runner$Cmodel$calculate()

    sm <- nimble::runMCMC(
      runner$Cmcmc,
      niter = niter, nburnin = nburnin, thin = thin,
      setSeed = seed_ch, progressBar = FALSE,
      samplesAsCodaMCMC = FALSE, summary = FALSE
    )
    sm <- as.matrix(sm)

    if ("alpha" %in% colnames(sm)) {
      alpha_vec <- sm[, "alpha"]
      alpha_all <- c(alpha_all, alpha_vec)
      alpha_chains[[length(alpha_chains) + 1L]] <- alpha_vec
    }

    if (is.null(z_cols_cache)) {
      z_cols_cache <- grep("^z\\[", colnames(sm), value = TRUE)
      if (length(z_cols_cache) == 0) stop("Monitor 'z' required to compute posterior K.")
    }
    z_mat <- sm[, z_cols_cache, drop = FALSE]
    K_all <- c(K_all, compute_K_from_z_matrix(z_mat))

    rm(sm, z_mat, inits)
    invisible(gc())
  }

  runtime_sec <- as.numeric(proc.time()[3] - t0)

  if (length(K_all) == 0) stop("No K samples extracted (empty chains?)")
  post_EK      <- mean(K_all)
  post_medianK <- stats::median(K_all)
  post_sdK     <- stats::sd(K_all)
  post_modeK   <- mode_int(K_all)
  post_probK1  <- mean(K_all == 1L)
  q <- stats::quantile(K_all, probs = c(0.025, 0.5, 0.975), names = FALSE)

  post_probK_eq_true  <- if (!is.null(K_true)) mean(K_all == as.integer(K_true)) else NA_real_
  post_probK_within1  <- if (!is.null(K_true)) mean(abs(K_all - as.integer(K_true)) <= 1L) else NA_real_
  alpha_rhat <- if (length(alpha_chains) >= 2) compute_rhat_scalar(alpha_chains) else NA_real_

  list(
    runtime_sec         = runtime_sec,
    post_EK             = post_EK,
    post_medianK        = post_medianK,
    post_sdK            = post_sdK,
    post_modeK          = post_modeK,
    post_probK1         = post_probK1,
    post_q025           = q[1],
    post_q500           = q[2],
    post_q975           = q[3],
    post_probK_eq_true  = post_probK_eq_true,
    post_probK_within1  = post_probK_within1,
    n_draws             = length(K_all),
    alpha_mean          = if (length(alpha_all) > 0) mean(alpha_all) else NA_real_,
    alpha_sd            = if (length(alpha_all) > 0) stats::sd(alpha_all) else NA_real_,
    alpha_rhat          = alpha_rhat
  )
}
