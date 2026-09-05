# Author: JoonHo Lee (jlee296@ua.edu)
# Fit the synthetic-study DP mixture
#
# Use the manuscript's NIMBLE model, four initialization profiles and recorded
# chain seeds. Derive partition summaries from the saved allocations so count
# and observed dominance summaries refer to the same draws.
#
# Called by refit/simulation/run_one_fit.R; shared initialization also
# supports applications.
# Inputs: unit estimates/variances, prior parameters and chain schedule.
# Returns chain draws and derived summaries; diagnostic decisions are
# separate.

simv2_model_monitors <- function() c(
  "alpha", "z", "tau", "sigma2_tau", "s_shape", "s_rate",
  "tau_G0", "sigma2_G0", "tau_j"
)
simv2_model_version <- "simulation-v2-model/2"
if (!exists(".SIMV2_NIMBLE_CACHE", inherits = FALSE)) {
  .SIMV2_NIMBLE_CACHE <- new.env(parent = emptyenv())
}

# Convert the initial values to a valid cluster-label representation with the
# requested number of clusters.
simv2_canonical_partition <- function(values, K) {
  J <- length(values)
  K <- max(1L, min(as.integer(K), J))
  if (K == 1L) return(rep.int(1L, J))
  ranks <- rank(values, ties.method = "first")
  pmin(K, as.integer(ceiling(ranks * K / J)))
}

# Choose one of the four fixed chain-start profiles used to probe different
# initial partitions.
simv2_initialization_profile <- function(chain) {
  chain <- as.integer(chain)
  if (length(chain) != 1L || is.na(chain) || !chain %in% 1:4) {
    stop("Initialization profile requires chain in 1:4", call. = FALSE)
  }
  data.frame(
    chain = chain,
    requested_K = c(1L, 5L, 10L, 20L)[[chain]],
    alpha = c(0.25, 1, 3, 8)[[chain]],
    stringsAsFactors = FALSE
  )
}

# Create the model initial values for a requested chain and study size.
simv2_make_inits <- function(J, df = NULL, chain = 2L) {
  J <- as.integer(J)
  if (length(J) != 1L || is.na(J) || J < 2L) {
    stop("Initialization requires J >= 2", call. = FALSE)
  }
  if (is.null(df)) {
    df <- data.frame(
      tau_hat = seq(-0.5, 0.5, length.out = J),
      se2 = rep(1, J), se = rep(1, J)
    )
  }
  if (!is.data.frame(df) || nrow(df) != J ||
    !all(c("tau_hat", "se2") %in% names(df))) {
    stop("Initialization data must contain J rows of tau_hat and se2", call. = FALSE)
  }
  tau_hat <- as.numeric(df$tau_hat)
  se2 <- as.numeric(df$se2)
  if (any(!is.finite(tau_hat)) || any(!is.finite(se2)) || any(se2 <= 0)) {
    stop("Initialization data contain invalid values", call. = FALSE)
  }
  profile <- simv2_initialization_profile(chain)
  z <- simv2_canonical_partition(tau_hat, profile$requested_K)
  active <- sort(unique(z))
  grand_sd <- max(stats::sd(tau_hat), 0.1)
  tau_G0 <- stats::rnorm(J, mean(tau_hat), grand_sd)
  residual_variance <- max(stats::var(tau_hat) - mean(se2), 0.02)
  sigma2_G0 <- rep(residual_variance, J)
  for (cluster in active) {
    members <- z == cluster
    tau_G0[[cluster]] <- mean(tau_hat[members])
    within <- if (sum(members) > 1L) stats::var(tau_hat[members]) else 0
    sigma2_G0[[cluster]] <- max(within - mean(se2[members]), 0.01)
  }
  list(
    tau_j = tau_hat, tau_G0 = tau_G0, sigma2_G0 = sigma2_G0, z = z,
    alpha = profile$alpha, tau = mean(tau_hat),
    sigma2_tau = min(residual_variance, 9), s_shape = 1, s_rate = 1
  )
}

# Declare the same DP normal-mixture likelihood and hyperpriors used in the
# manuscript computation.
simv2_nimble_code <- function() {
  nimble::nimbleCode({
    for (j in 1:J) {
      tau_j_hat[j] ~ dnorm(tau_j[j], sd = se_j[j])
      tau_j[j] ~ dnorm(tau_clust[j], var = sigma2_clust[j])
      tau_clust[j] <- tau_G0[z[j]]
      sigma2_clust[j] <- sigma2_G0[z[j]]
    }
    for (j in 1:J) {
      tau_G0[j] ~ dnorm(tau, var = sigma2_tau)
      sigma2_G0[j] ~ dinvgamma(s_shape, s_rate)
    }
    z[1:J] ~ dCRP(alpha, size = J)
    alpha ~ dgamma(a, b)
    tau ~ dnorm(grand_mean_mean, var = grand_mean_variance)
    sigma2_tau ~ dunif(0, 10)
    s_shape ~ dinvgamma(2, 1)
    s_rate ~ dinvgamma(2, 1)
  })
}

# Build and cache a compiled NIMBLE runner for a given J so repeated fits of
# that size can reuse the compiled model.
simv2_get_nimble_runner <- function(J) {
  simv2_require_namespace("nimble")
  # NIMBLE's model parser resolves built-in distribution symbols (including
  # dCRP) through the attached package environment, not only its namespace.
  suppressPackageStartupMessages(library(nimble))
  key <- paste0(simv2_model_version, "|J=", as.integer(J))
  if (exists(key, envir = .SIMV2_NIMBLE_CACHE, inherits = FALSE)) {
    return(get(key, envir = .SIMV2_NIMBLE_CACHE, inherits = FALSE))
  }
  monitors <- simv2_model_monitors()
  data0 <- list(
    tau_j_hat = rep(0, J), se_j = rep(1, J), a = 1, b = 1,
    grand_mean_mean = 0, grand_mean_variance = 100
  )
  model <- nimble::nimbleModel(
    simv2_nimble_code(),
    constants = list(J = as.integer(J)),
    data = data0, inits = simv2_make_inits(J), check = FALSE
  )
  configuration <- nimble::configureMCMC(model, monitors = monitors, print = FALSE)
  mcmc <- nimble::buildMCMC(configuration)
  compiled_model <- nimble::compileNimble(model, showCompilerOutput = FALSE)
  compiled_mcmc <- nimble::compileNimble(
    mcmc,
    project = compiled_model, showCompilerOutput = FALSE
  )
  runner <- list(
    J = J, model = compiled_model, mcmc = compiled_mcmc,
    monitors = monitors, cache_key = key
  )
  assign(key, runner, envir = .SIMV2_NIMBLE_CACHE)
  runner
}

# Locate indexed draw columns in numerical index order rather than
# lexicographic order.
simv2_grep_columns <- function(samples, prefix) {
  grep(paste0("^", prefix, "\\["), colnames(samples))
}

# Compute cluster count and related summaries from allocation and
# latent-effect draws.
simv2_derived_chain_draws <- function(samples) {
  samples <- as.matrix(samples)
  z_columns <- simv2_grep_columns(samples, "z")
  atom_columns <- simv2_grep_columns(samples, "tau_G0")
  atom_variance_columns <- simv2_grep_columns(samples, "sigma2_G0")
  outcome_columns <- simv2_grep_columns(samples, "tau_j")
  required <- c("alpha", "tau", "sigma2_tau", "s_shape", "s_rate")
  if (!all(required %in% colnames(samples)) || !length(z_columns) ||
    !length(atom_columns) ||
    !length(atom_variance_columns) ||
    !length(outcome_columns)) {
    stop("NIMBLE output is missing predeclared v2 monitors", call. = FALSE)
  }
  z <- round(samples[, z_columns, drop = FALSE])
  atoms <- samples[, atom_columns, drop = FALSE]
  atom_variances <- samples[, atom_variance_columns, drop = FALSE]
  outcomes <- samples[, outcome_columns, drop = FALSE]
  active_summary <- t(vapply(seq_len(nrow(samples)), function(i) {
    active <- sort(unique(z[i, ]))
    if (any(active < 1L) || any(active > ncol(atoms))) {
      stop("Active CRP label falls outside monitored atom support", call. = FALSE)
    }
    active_atoms <- atoms[i, active]
    active_variances <- atom_variances[i, active]
    c(
      K = length(active), active_atom_mean = mean(active_atoms),
      active_atom_sd = if (length(active_atoms) > 1L) stats::sd(active_atoms) else 0,
      active_atom_variance_mean = mean(active_variances)
    )
  }, numeric(4)))
  data.frame(
    alpha = samples[, "alpha"], K = active_summary[, "K"],
    grand_mean = samples[, "tau"], base_variance = samples[, "sigma2_tau"],
    base_shape = samples[, "s_shape"], base_rate = samples[, "s_rate"],
    active_atom_mean = active_summary[, "active_atom_mean"],
    active_atom_sd = active_summary[, "active_atom_sd"],
    active_atom_variance_mean = active_summary[, "active_atom_variance_mean"],
    outcome_mean = rowMeans(outcomes),
    outcome_sd = apply(outcomes, 1L, stats::sd),
    stringsAsFactors = FALSE
  )
}

# Run the declared chain seeds and sampling schedule, then return the
# monitored and derived draws for diagnostic checking.
simv2_fit_dp_chains <- function(df, a, b, grand_mean_mean,
                                grand_mean_variance, chain_seeds,
                                n_iter, n_warmup, thin = 1L) {
  if (length(chain_seeds) != 4L || anyDuplicated(chain_seeds)) {
    stop("Exactly four distinct manifest-assigned chain seeds are required",
      call. = FALSE
    )
  }
  if (n_iter <= n_warmup || n_warmup < 0L || thin < 1L) {
    stop("Invalid MCMC iteration settings", call. = FALSE)
  }
  J <- nrow(df)
  runner <- simv2_get_nimble_runner(J)
  runner$model$tau_j_hat <- as.numeric(df$tau_hat)
  runner$model$se_j <- if ("se" %in% names(df)) as.numeric(df$se) else sqrt(df$se2)
  runner$model$a <- as.numeric(a)
  runner$model$b <- as.numeric(b)
  runner$model$grand_mean_mean <- as.numeric(grand_mean_mean)
  runner$model$grand_mean_variance <- as.numeric(grand_mean_variance)
  chains <- vector("list", 4L)
  runtime <- numeric(4L)
  tau_site_sum <- numeric(J)
  tau_site_sum2 <- numeric(J)
  tau_site_draws <- 0L
  initialization <- vector("list", 4L)
  for (chain in seq_len(4L)) {
    seed <- as.integer(chain_seeds[[chain]])
    set.seed(seed,
      kind = "Mersenne-Twister", normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
    inits <- simv2_make_inits(J, df = df, chain = chain)
    runner$model$setInits(inits)
    initial_log_probability <- runner$model$calculate()
    if (!is.finite(initial_log_probability)) {
      stop(sprintf("Chain %d initialization has non-finite log probability", chain),
        call. = FALSE
      )
    }
    initialization[[chain]] <- data.frame(
      chain = chain,
      mode = "data-informed-overdispersed-v1",
      initial_K = length(unique(inits$z)),
      initial_alpha = as.numeric(inits$alpha),
      initial_tau = as.numeric(inits$tau),
      initial_sigma2_tau = as.numeric(inits$sigma2_tau),
      initial_log_probability = as.numeric(initial_log_probability),
      stringsAsFactors = FALSE
    )
    started <- proc.time()[[3L]]
    samples <- nimble::runMCMC(
      runner$mcmc,
      niter = as.integer(n_iter), nburnin = as.integer(n_warmup),
      thin = as.integer(thin), setSeed = seed, progressBar = FALSE,
      samplesAsCodaMCMC = FALSE, summary = FALSE
    )
    runtime[[chain]] <- proc.time()[[3L]] - started
    tau_columns <- simv2_grep_columns(samples, "tau_j")
    tau_samples <- as.matrix(samples)[, tau_columns, drop = FALSE]
    tau_site_sum <- tau_site_sum + colSums(tau_samples)
    tau_site_sum2 <- tau_site_sum2 + colSums(tau_samples^2)
    tau_site_draws <- tau_site_draws + nrow(tau_samples)
    chains[[chain]] <- simv2_derived_chain_draws(samples)
    rm(samples, tau_samples)
    invisible(gc())
  }
  tau_posterior_mean <- tau_site_sum / tau_site_draws
  tau_posterior_sd <- sqrt(pmax(tau_site_sum2 / tau_site_draws -
    tau_posterior_mean^2, 0))
  list(
    chains = chains, tau_posterior_mean = tau_posterior_mean,
    tau_posterior_sd = tau_posterior_sd,
    runtime_by_chain_sec = runtime, runtime_sec = sum(runtime),
    initialization = do.call(rbind, initialization),
    model_version = simv2_model_version
  )
}
