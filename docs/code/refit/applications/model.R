# Author: JoonHo Lee (jlee296@ua.edu)
# Fit the school-study application model
#
# Use the four-chain initialization and diagnostic rules shared with the
# simulation. Retain allocation counts and unit draws so observed-partition
# dominance can be distinguished from population atom weights, including
# unoccupied mass.
#
# Called by refit/applications/run_one_fit.R after the shared simulation
# modules.
# Inputs: prepared school/study effects and variances, priors and sampling
# schedule.
# Scientific model functions are retained from the manuscript computation.

appv2_model_version <- "application-v2-model/1"
appv2_result_schema <- "application-v2-result/1"
if (!exists("simv2_make_inits", mode = "function")) {
  stop("Source simulation_v2/model_adapter.R before applications_v2/model.R",
    call. = FALSE
  )
}
if (!exists("simv2_modern_diagnostics", mode = "function")) {
  stop("Source simulation_v2/diagnostics.R before applications_v2/model.R",
    call. = FALSE
  )
}
if (!exists("simv2_substream_seed", mode = "function")) {
  stop("Source simulation_v2/keys.R before applications_v2/model.R", call. = FALSE)
}
if (!exists(".APPV2_NIMBLE_CACHE", inherits = FALSE)) {
  .APPV2_NIMBLE_CACHE <- new.env(parent = emptyenv())
}

# Specify the DP mixture for unit estimates with their known first-stage
# variances.
appv2_nimble_code <- function() {
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
    sigma2_tau ~ dunif(0, sigma2_tau_upper)
    s_shape ~ dinvgamma(2, 1)
    s_rate ~ dinvgamma(2, 1)
  })
}

# Compile and cache the application model at the requested number of units.
appv2_get_runner <- function(J) {
  simv2_require_namespace("nimble")
  suppressPackageStartupMessages(library(nimble))
  J <- as.integer(J)
  key <- paste(appv2_model_version, J, sep = "|J=")
  if (exists(key, envir = .APPV2_NIMBLE_CACHE, inherits = FALSE)) {
    return(get(key, envir = .APPV2_NIMBLE_CACHE, inherits = FALSE))
  }
  monitors <- simv2_model_monitors()
  data0 <- list(
    tau_j_hat = rep(0, J), se_j = rep(1, J), a = 1, b = 1,
    grand_mean_mean = 0, grand_mean_variance = 100,
    sigma2_tau_upper = 10
  )
  model <- nimble::nimbleModel(
    appv2_nimble_code(),
    constants = list(J = J), data = data0,
    inits = simv2_make_inits(J), check = FALSE
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
  assign(key, runner, envir = .APPV2_NIMBLE_CACHE)
  runner
}

# Choose evenly spaced retained draws up to the declared cap, preserving the
# same rule for each chain.
appv2_retention_index <- function(n, maximum) {
  n <- as.integer(n)
  maximum <- as.integer(maximum)
  if (n < 1L || maximum < 1L) stop("Invalid retained-draw request", call. = FALSE)
  if (n <= maximum) return(seq_len(n))
  unique(as.integer(round(seq(1, n, length.out = maximum))))
}

# Check one scalar across chains and return its convergence statistics and
# acceptance decision.
appv2_scalar_diagnostic <- function(one, variable, rhat_max, ess_bulk_min,
                                    ess_tail_min) {
  one <- as.matrix(one)
  values <- as.numeric(one)
  constant <- diff(range(values)) == 0
  if (constant) {
    out <- data.frame(
      variable = variable, mean = values[[1L]], sd = 0, rhat = 1,
      ess_bulk = length(values), ess_tail = length(values),
      mcse_mean = 0, mcse_sd = 0, constant = TRUE,
      stringsAsFactors = FALSE
    )
  } else {
    tail_ess <- posterior::ess_tail(one)
    if (!is.finite(tail_ess)) {
      limits <- stats::quantile(values, c(.05, .95), names = FALSE, type = 1)
      lower <- matrix(as.numeric(one <= limits[[1L]]), nrow = nrow(one))
      upper <- matrix(as.numeric(one >= limits[[2L]]), nrow = nrow(one))
      tail_ess <- min(posterior::ess_bulk(lower), posterior::ess_bulk(upper))
    }
    out <- data.frame(
      variable = variable, mean = mean(values), sd = stats::sd(values),
      rhat = posterior::rhat(one), ess_bulk = posterior::ess_bulk(one),
      ess_tail = tail_ess, mcse_mean = posterior::mcse_mean(one),
      mcse_sd = posterior::mcse_sd(one), constant = FALSE,
      stringsAsFactors = FALSE
    )
  }
  out$rhat_pass <- is.finite(out$rhat) & out$rhat <= rhat_max
  out$ess_bulk_pass <- is.finite(out$ess_bulk) & out$ess_bulk >= ess_bulk_min
  out$ess_tail_pass <- is.finite(out$ess_tail) & out$ess_tail >= ess_tail_min
  out$gate_pass <- out$rhat_pass & out$ess_bulk_pass & out$ess_tail_pass
  out
}

# Apply the scalar diagnostic checks to each unit's latent-effect draws.
appv2_unit_diagnostics <- function(tau_chains, rhat_max = 1.01,
                                   ess_bulk_min = 400, ess_tail_min = 400) {
  simv2_require_namespace("posterior")
  if (length(tau_chains) != 4L || length(unique(vapply(
    tau_chains, nrow, integer(1)
  ))) != 1L) stop("Four equal-length tau chains are required", call. = FALSE)
  J <- ncol(tau_chains[[1L]])
  if (any(vapply(tau_chains, ncol, integer(1)) != J)) {
    stop("Unit-effect chain dimensions differ", call. = FALSE)
  }
  do.call(rbind, lapply(seq_len(J), function(j) {
    one <- do.call(cbind, lapply(tau_chains, function(x) x[, j]))
    appv2_scalar_diagnostic(
      one, paste0("tau_j[", j, "]"), rhat_max, ess_bulk_min, ess_tail_min
    )
  }))
}

# Estimate pairwise co-clustering probabilities from allocation draws; label
# numbering does not affect membership equality.
appv2_coclustering <- function(z) {
  z <- as.matrix(z)
  J <- ncol(z)
  out <- diag(1, J)
  if (J > 1L) {
    for (i in seq_len(J - 1L)) {
      for (j in (i + 1L):J) {
        out[i, j] <- out[j, i] <- mean(z[, i] == z[, j])
      }
    }
  }
  out
}

# Count the occupied allocation groups in each draw for subsequent weight
# calculations.
appv2_count_matrix <- function(z) {
  z <- as.matrix(z)
  J <- ncol(z)
  t(vapply(seq_len(nrow(z)), function(i) {
    counts <- sort(tabulate(as.integer(z[i, ])), decreasing = TRUE)
    c(counts, integer(J - length(counts)))
  }, integer(J)))
}

# Sample population weights conditional on allocations and alpha, including
# residual unoccupied mass. Observed cluster shares alone would omit that
# mass.
appv2_population_weight_draws <- function(counts, alpha, seed) {
  counts <- as.matrix(counts)
  alpha <- as.numeric(alpha)
  seed <- suppressWarnings(as.integer(seed))
  if (!length(seed) || length(seed) != 1L || is.na(seed) || seed < 1L ||
    nrow(counts) != length(alpha) || any(!is.finite(alpha)) || any(alpha <= 0) ||
    any(!is.finite(counts)) || any(counts < 0) || any(counts != floor(counts)) ||
    any(rowSums(counts) < 1L)) {
    stop("Invalid allocation-count/alpha draws", call. = FALSE)
  }
  set.seed(seed,
    kind = "Mersenne-Twister", normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  n <- nrow(counts)
  occupied_largest <- numeric(n)
  residual_mass <- numeric(n)
  first_unoccupied_share_within_residual <- numeric(n)
  first_unoccupied_total_weight <- numeric(n)
  population_largest_atom_weight <- numeric(n)
  for (i in seq_len(n)) {
    active <- counts[i, counts[i, ] > 0]
    gamma_draws <- stats::rgamma(length(active) + 1L,
      shape = c(active, alpha[[i]]), rate = 1
    )
    weights <- gamma_draws / sum(gamma_draws)
    occupied_largest[[i]] <- max(weights[seq_along(active)])
    residual_mass[[i]] <- weights[[length(weights)]]
    residual_remaining <- residual_mass[[i]]
    unseen_largest <- 0
    first_share <- NA_real_
    first_total <- NA_real_
    # Conditional on counts and alpha, the unoccupied mass is itself a DP.
    # Draw its size-biased sticks until the remaining total mass cannot exceed
    # the largest unseen atom already observed.  This yields the actual largest
    # population atom, not the alpha-only prior-form tail.
    for (stick in seq_len(100000L)) {
      share <- stats::rbeta(1L, 1, alpha[[i]])
      atom <- residual_remaining * share
      if (stick == 1L) {
        first_share <- share
        first_total <- atom
      }
      unseen_largest <- max(unseen_largest, atom)
      residual_remaining <- residual_remaining * (1 - share)
      if (residual_remaining <= unseen_largest || residual_remaining < 1e-12) break
      if (stick == 100000L) stop("Residual stick expansion did not terminate",
        call. = FALSE
      )
    }
    first_unoccupied_share_within_residual[[i]] <- first_share
    first_unoccupied_total_weight[[i]] <- first_total
    population_largest_atom_weight[[i]] <- max(occupied_largest[[i]], unseen_largest)
  }
  data.frame(
    largest_currently_occupied_population_weight = occupied_largest,
    residual_unoccupied_population_mass = residual_mass,
    first_unoccupied_size_biased_share_within_residual =
      first_unoccupied_share_within_residual,
    first_unoccupied_size_biased_total_population_weight =
      first_unoccupied_total_weight,
    largest_population_atom_weight = population_largest_atom_weight,
    stringsAsFactors = FALSE
  )
}

# Run the four declared chains, retain the planned subset of draws and return
# unit, allocation, population-weight and diagnostic summaries.
appv2_fit_chains <- function(df, a, b, grand_mean_mean, grand_mean_variance,
                             sigma2_tau_upper, chain_seeds, n_iter, n_warmup,
                             thin = 1L, maximum_retained_per_chain = 4000L,
                             population_seed) {
  simv2_require_namespace("posterior")
  chain_seed_numeric <- suppressWarnings(as.numeric(chain_seeds))
  if (length(chain_seed_numeric) != 4L || anyNA(chain_seed_numeric) ||
    any(!is.finite(chain_seed_numeric)) || any(chain_seed_numeric < 1) ||
    any(chain_seed_numeric > 2147483646) ||
    any(chain_seed_numeric != floor(chain_seed_numeric)) ||
    anyDuplicated(chain_seed_numeric)) {
    stop("Exactly four distinct chain seeds are required", call. = FALSE)
  }
  chain_seeds <- as.integer(chain_seed_numeric)
  if (length(n_iter) != 1L || length(n_warmup) != 1L || length(thin) != 1L ||
    length(maximum_retained_per_chain) != 1L || !is.finite(n_iter) ||
    !is.finite(n_warmup) || !is.finite(thin) ||
    !is.finite(maximum_retained_per_chain) || n_iter != floor(n_iter) ||
    n_warmup != floor(n_warmup) || thin != floor(thin) ||
    maximum_retained_per_chain != floor(maximum_retained_per_chain) ||
    n_iter <= n_warmup || n_warmup < 0L || thin < 1L ||
    maximum_retained_per_chain < 1L) {
    stop("Invalid application MCMC/retention settings", call. = FALSE)
  }
  required <- c("tau_hat", "se2")
  if (!is.data.frame(df) || !all(required %in% names(df)) || nrow(df) < 2L ||
    any(!is.finite(df$tau_hat)) || any(!is.finite(df$se2)) || any(df$se2 <= 0)) {
    stop("Application data are invalid", call. = FALSE)
  }
  if (length(a) != 1L || length(b) != 1L || length(grand_mean_mean) != 1L ||
    length(grand_mean_variance) != 1L || length(sigma2_tau_upper) != 1L ||
    !is.finite(a) || !is.finite(b) || a <= 0 || b <= 0 ||
    !is.finite(grand_mean_mean) || !is.finite(sigma2_tau_upper) ||
    sigma2_tau_upper <= 0 || !is.finite(grand_mean_variance) ||
    grand_mean_variance <= 0) {
    stop("Application model scales must be positive", call. = FALSE)
  }
  J <- nrow(df)
  runner <- appv2_get_runner(J)
  runner$model$tau_j_hat <- as.numeric(df$tau_hat)
  runner$model$se_j <- sqrt(as.numeric(df$se2))
  runner$model$a <- as.numeric(a)
  runner$model$b <- as.numeric(b)
  runner$model$grand_mean_mean <- as.numeric(grand_mean_mean)
  runner$model$grand_mean_variance <- as.numeric(grand_mean_variance)
  runner$model$sigma2_tau_upper <- as.numeric(sigma2_tau_upper)
  derived_chains <- vector("list", 4L)
  tau_chains <- vector("list", 4L)
  retained_z <- vector("list", 4L)
  retained_tau <- vector("list", 4L)
  retained_alpha <- vector("list", 4L)
  runtime <- numeric(4L)
  initialization <- vector("list", 4L)
  for (chain in seq_len(4L)) {
    seed <- as.integer(chain_seeds[[chain]])
    set.seed(seed,
      kind = "Mersenne-Twister", normal.kind = "Inversion",
      sample.kind = "Rejection"
    )
    inits <- simv2_make_inits(J, df = df, chain = chain)
    inits$sigma2_tau <- min(inits$sigma2_tau, sigma2_tau_upper * .9)
    runner$model$setInits(inits)
    initial_log_probability <- runner$model$calculate()
    if (!is.finite(initial_log_probability)) {
      stop(sprintf("Chain %d initialization is non-finite", chain), call. = FALSE)
    }
    initialization[[chain]] <- data.frame(
      chain = chain, mode = "data-informed-overdispersed-v1",
      initial_K = length(unique(inits$z)), initial_alpha = inits$alpha,
      initial_tau = inits$tau, initial_sigma2_tau = inits$sigma2_tau,
      initial_log_probability = initial_log_probability,
      stringsAsFactors = FALSE
    )
    started <- proc.time()[["elapsed"]]
    samples <- nimble::runMCMC(
      runner$mcmc,
      niter = as.integer(n_iter), nburnin = as.integer(n_warmup),
      thin = as.integer(thin), setSeed = seed, progressBar = FALSE,
      samplesAsCodaMCMC = FALSE, summary = FALSE
    )
    runtime[[chain]] <- proc.time()[["elapsed"]] - started
    samples <- as.matrix(samples)
    z_columns <- simv2_grep_columns(samples, "z")
    tau_columns <- simv2_grep_columns(samples, "tau_j")
    derived_chains[[chain]] <- simv2_derived_chain_draws(samples)
    tau_chains[[chain]] <- samples[, tau_columns, drop = FALSE]
    keep <- appv2_retention_index(nrow(samples), maximum_retained_per_chain)
    retained_z[[chain]] <- round(samples[keep, z_columns, drop = FALSE])
    retained_tau[[chain]] <- samples[keep, tau_columns, drop = FALSE]
    retained_alpha[[chain]] <- samples[keep, "alpha"]
    rm(samples)
    invisible(gc())
  }
  base_diagnostics <- simv2_modern_diagnostics(derived_chains)
  unit_diagnostics <- appv2_unit_diagnostics(tau_chains)
  diagnostic_pass <- isTRUE(base_diagnostics$pass) && all(unit_diagnostics$gate_pass)
  z <- do.call(rbind, retained_z)
  tau <- do.call(rbind, retained_tau)
  alpha <- unlist(retained_alpha, use.names = FALSE)
  counts <- appv2_count_matrix(z)
  weights <- appv2_population_weight_draws(counts, alpha, population_seed)
  observed_largest_count <- counts[, 1L]
  observed_largest_share <- observed_largest_count / J
  K <- rowSums(counts > 0)
  unit_summary <- data.frame(
    unit = if ("unit" %in% names(df)) df$unit else seq_len(J),
    posterior_mean = colMeans(tau), posterior_sd = apply(tau, 2L, stats::sd),
    q025 = apply(tau, 2L, stats::quantile, probs = .025, names = FALSE),
    q50 = apply(tau, 2L, stats::quantile, probs = .5, names = FALSE),
    q975 = apply(tau, 2L, stats::quantile, probs = .975, names = FALSE),
    stringsAsFactors = FALSE
  )
  posterior_summary <- data.frame(
    retained_partition_draws = nrow(z), E_K = mean(K), median_K = stats::median(K),
    sd_K = stats::sd(K), q025_K = stats::quantile(K, .025, names = FALSE),
    q975_K = stats::quantile(K, .975, names = FALSE), p_K1 = mean(K == 1L),
    p_K_le2 = mean(K <= 2L), alpha_mean = mean(alpha),
    observed_largest_count_mean = mean(observed_largest_count),
    observed_largest_share_mean = mean(observed_largest_share),
    observed_largest_share_q025 = stats::quantile(observed_largest_share, .025,
      names = FALSE
    ),
    observed_largest_share_q975 = stats::quantile(observed_largest_share, .975,
      names = FALSE
    ),
    p_observed_largest_share_gt_05 = mean(observed_largest_share > .5),
    population_largest_occupied_weight_mean =
      mean(weights$largest_currently_occupied_population_weight),
    p_population_largest_occupied_weight_gt_05 =
      mean(weights$largest_currently_occupied_population_weight > .5),
    residual_unoccupied_mass_mean = mean(weights$residual_unoccupied_population_mass),
    first_unoccupied_share_within_residual_mean =
      mean(weights$first_unoccupied_size_biased_share_within_residual),
    p_first_unoccupied_total_population_weight_gt_05 =
      mean(weights$first_unoccupied_size_biased_total_population_weight > .5),
    population_largest_atom_weight_mean = mean(weights$largest_population_atom_weight),
    p_population_largest_atom_weight_gt_05 =
      mean(weights$largest_population_atom_weight > .5),
    alpha_only_prior_form_p_first_size_biased_weight_gt_05 = mean(.5^alpha),
    stringsAsFactors = FALSE
  )
  list(
    schema_version = "application-v2-fit-components/1",
    model_version = appv2_model_version, runtime_by_chain_sec = runtime,
    runtime_sec = sum(runtime), initialization = do.call(rbind, initialization),
    diagnostic_pass = diagnostic_pass, base_diagnostics = base_diagnostics,
    unit_diagnostics = unit_diagnostics, posterior_summary = posterior_summary,
    unit_summary = unit_summary, co_clustering = appv2_coclustering(z),
    retained = list(
      z = z, tau_j = tau, alpha = alpha, allocation_counts_sorted = counts,
      population_weights = weights
    )
  )
}
