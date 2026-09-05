# Author: JoonHo Lee (jlee296@ua.edu)
# Combine matching Rasch chains and reconstruct density
#
# Reconstruct the uncompiled model specification from the same checked data.
# Sampling a DP measure conditional on retained draws supplies population-
# density and atom-weight summaries; it does not refit the response model.
#
# Called by refit/rasch/combine_and_check.R.
# Requires four chains from the same fit, attempt, settings and analysis
# sample.
# Returns diagnostics, score-group summaries, model density and mass checks.

# Reconstruct one chain's conditional population density
rasch_chain_density <- function(chain, Y) {
  j <- chain$job
  set.seed(j$chain_seed,
    kind = 'Mersenne-Twister',
    normal.kind = 'Inversion',
    sample.kind = 'Rejection'
  )
  spec <- DPMirt::dpmirt_spec(Y,
    model = j$model,
    prior = j$prior,
    parameterization = j$parameterization,
    identification = j$identification, alpha_prior = c(a = j$a, b = j$b),
    base_measure = list(s2_mu = j$s2_mu, nu1 = j$nu1, nu2 = j$nu2), M = j$M
  )
  fit <- structure(list(
    config = chain$config, samples_raw = chain$samples_raw,
    compiled = list(spec = spec), location_shift = rep(0, nrow(chain$samples_raw)),
    scale_shift = rep(1, nrow(chain$samples_raw))
  ), class = 'dpmirt_fit')
  set.seed(j$density_seed,
    kind = 'Mersenne-Twister',
    normal.kind = 'Inversion',
    sample.kind = 'Rejection'
  )
  density <- DPMirt::dpmirt_dp_density(fit,
    grid = seq(-10, 14, length.out = 961),
    credible_interval = .95, apply_rescaling = TRUE, verbose = FALSE
  )
  stopifnot(
    all(is.finite(density$density_samples)),
    identical(dim(density$density_samples), c(nrow(chain$samples_raw), 961L))
  )
  density
}

# Require matching settings and distinct chain streams
rasch_validate_chain_set <- function(chains) {
  if (length(chains) != 4L) stop('Exactly four completed chain results are required.', call. = FALSE)
  ids <- vapply(chains, function(x) as.integer(x$job$chain), integer(1))
  if (!setequal(ids, 1:4) || anyDuplicated(ids)) stop('Chain IDs must be exactly 1:4.', call. = FALSE)
  chains <- chains[order(ids)]
  same <- c(
    'arm',
    'role',
    'attempt',
    'profile',
    'analysis_object_sha256',
    'a',
    'b',
    'M',
    's2_mu',
    'nu1',
    'nu2',
    'model',
    'prior',
    'identification',
    'parameterization',
    'niter',
    'nburnin',
    'thin',
    'thin2'
  )
  for (field in same) if (length(unique(lapply(chains, function(x) x$job[[field]]))) != 1L)
    stop('Cannot pool chains with different ', field, '.', call. = FALSE)
  for (field in c('chain_seed', 'density_seed'))
    if (anyDuplicated(vapply(chains, function(x) as.numeric(x$job[[field]]), numeric(1))))
      stop('Chain seeds must be distinct: ', field, call. = FALSE)
  for (field in c('samples_raw', 'samples2_raw')) {
    if (length(unique(lapply(chains, function(x) colnames(x[[field]])))) != 1L ||
      length(unique(vapply(chains, function(x) nrow(x[[field]]), integer(1)))) != 1L)
      stop('Chain draw counts or column orders differ.', call. = FALSE)
    if (any(!vapply(chains, function(x) all(is.finite(x[[field]])), logical(1)))) stop('Nonfinite chain draws.', call. = FALSE)
  }
  if (!all(vapply(chains, function(x) isTRUE(x$identification_checks_passed), logical(1))))
    stop('A chain did not pass identification/capacity checks.', call. = FALSE)
  chains
}

# Pool accepted chain inputs and calculate model summaries
rasch_combine <- function(chains, Y) {
  chains <- rasch_validate_chain_set(chains)
  suppressPackageStartupMessages(library(DPMirt))
  densities <- lapply(chains, rasch_chain_density, Y = Y)
  n <- nrow(chains[[1]]$samples_raw)
  groups <- split(seq_len(nrow(Y)), rowSums(Y))
  rows <- list()
  append_diagnostics <- function(matrices, group) {
    vars <- colnames(matrices[[1]])
    for (v in seq_along(vars)) {
      m <- do.call(cbind, lapply(matrices, function(x) x[, v]))
      rows[[length(rows) + 1L]] <<- diag_matrix(m, vars[v], group, TRUE)
    }
  }
  K <- lapply(chains, function(x) matrix(x$K, ncol = 1, dimnames = list(NULL, 'K')))
  maxshare <- lapply(chains, function(x) {
    z <- x$samples_raw[, grep('^zi\\[', colnames(x$samples_raw)), drop = FALSE]
    matrix(apply(z, 1, function(r) max(table(r)) / 500),
      ncol = 1,
      dimnames = list(NULL, 'maxshare')
    )
  })
  append_diagnostics(K, 'K')
  append_diagnostics(maxshare, 'maxshare')
  for (group in c('alpha', 'loglik', 'beta')) {
    pattern <- switch(group,
      alpha = '^alpha$',
      loglik = '^myLogLik$',
      beta = '^beta\\['
    )
    mats <- lapply(
      chains,
      function(x) x$samples_raw[, grep(pattern, colnames(x$samples_raw)), drop = FALSE]
    )
    if (!ncol(mats[[1]])) stop('Missing gated variables: ', group, call. = FALSE)
    append_diagnostics(mats, group)
  }

  # Average ability draws within raw-score groups. The reported comparison
  # uses these group summaries, not individual latent estimates.
  theta <- lapply(
    chains,
    function(x) x$samples2_raw[, grep('^eta\\[', colnames(x$samples2_raw)), drop = FALSE]
  )
  scores <- lapply(theta, function(t) {
    m <- sapply(groups, function(idx) rowMeans(t[, idx, drop = FALSE]))
    colnames(m) <- paste0('thetabar[', names(groups), ']')
    m
  })
  append_diagnostics(scores, 'thetabar')

  # Apply the density diagnostic only where the posterior mean density reaches
  # the original .01 threshold.
  density_samples <- lapply(densities, `[[`, 'density_samples')
  mean_density <- colMeans(do.call(rbind, density_samples))
  gate <- which(mean_density >= .01)
  if (!length(gate)) stop('No density points satisfy the diagnostic gate.', call. = FALSE)
  append_diagnostics(lapply(density_samples, function(x) {
    y <- x[, gate, drop = FALSE]
    colnames(y) <- paste0('density[', gate, ']')
    y
  }), 'density')
  diagnostics <- do.call(rbind, rows)

  # Check numerical grid mass against analytic Gaussian-mixture mass outside
  # the grid. A dense-looking curve alone is not evidence of adequate
  # coverage.
  mass <- vapply(densities, function(d) sum(d$density_mean) * .025, numeric(1))
  outside <- lapply(densities, function(d) vapply(d$dp_samples, function(m) {
    sum(m[, 1] * (pnorm(-10, m[, 2], sqrt(m[, 3])) + pnorm(14, m[, 2], sqrt(m[, 3]), lower.tail = FALSE))) / sum(m[, 1])
  }, numeric(1)))

  # Pool draw-level summaries only after the four-chain checks above.
  Kall <- unlist(K)
  shares <- unlist(maxshare)
  atommax <- unlist(lapply(
    densities,
    function(d) vapply(d$dp_samples, function(m) max(m[, 1]), numeric(1))
  ))
  score_all <- do.call(rbind, scores)
  dens_all <- do.call(rbind, density_samples)
  list(
    schema = 'public-rasch-combined/1', job = chains[[1]]$job,
    diagnostic_pass = all(diagnostics$gate_pass) && all(mass >= .999) && mean(unlist(outside)) <= .001,
    diagnostics = diagnostics,
    density_mass_by_chain = mass, analytic_outside_mass_mean = mean(unlist(outside)),
    posterior_summary = data.frame(
      n_draws = length(Kall), E_K = mean(Kall), sd_K = sd(Kall),
      p_K1 = mean(Kall == 1), observed_largest_share_mean = mean(shares),
      population_largest_atom_weight_mean = mean(atommax),
      p_population_largest_atom_weight_gt_05 = mean(atommax > .5)
    ),
    score_summary = data.frame(
      r = as.integer(names(groups)), n_r = lengths(groups),
      EAP_r = colMeans(score_all)
    ),
    density = data.frame(
      x = densities[[1]]$grid, mean = mean_density,
      lower = apply(dens_all, 2, quantile, .025), upper = apply(dens_all, 2, quantile, .975)
    ),
    scope = 'Four same-fit same-attempt chains; score-group and model diagnostics; individual theta diagnostics omitted from this compact driver'
  )
}
