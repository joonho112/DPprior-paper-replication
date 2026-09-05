#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Run one declared Rasch chain
#
# Select one arm, prior role, attempt and chain from the recorded job table.
# Verify constrained-item identification and available mixture capacity before
# saving uncompiled draws for later combination.
#
# Run: Rscript refit/rasch/run_one_chain.R --input PATH --profile smoke
# PATH is the separately acquired IRW long-table RDS; add --execute to sample.
# Smoke schedule: 60 iterations, 30 warmup. Local latent draws are not release
# files.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', '..', 'common', 'R', 'bootstrap.R'))
source(rp_path('refit', 'common', 'driver.R', must_exist = TRUE))
args <- rf_args()
rf_check_job_inputs()
source(file.path(.script_dir, 'data_loader.R'))

# Select one declared arm, prior, attempt and chain
arm <- if (is.null(args$arm)) 'main' else args$arm
role <- if (is.null(args$role)) 'vague' else args$role
chain <- if (is.null(args$chain)) 1L else as.integer(args$chain)
jobs <- rp_read_csv(rp_path('data-frozen', 'rasch', 'chain_jobs.csv', must_exist = TRUE),
  key = 'job_id'
)
row <- jobs[jobs$arm == arm & jobs$role == role & jobs$attempt == args$attempt &
  jobs$chain == chain, ]
if (nrow(row) != 1L) stop('Select exactly one declared arm/role/attempt/chain.', call. = FALSE)
if (is.null(args$input)) stop('Supply --input with the separately acquired IRW long-table RDS.',
  call. = FALSE
)

# Reconstruct the fixed vocabulary analysis sample
data <- rasch_source_matrix(normalizePath(args$input, mustWork = TRUE))

# Use the declared schedule unless smoke was explicitly requested
niter <- row$niter
nburnin <- row$nburnin
thin <- row$thin
thin2 <- row$thin2
if (args$profile == 'smoke') {
  niter <- 60L
  nburnin <- 30L
  thin <- thin2 <- 1L
}

# Describe the chain, identification and random streams
job <- list(
  track = 'rasch',
  profile = args$profile,
  job_id = row$job_id,
  arm = arm,
  role = role,
  chain = chain,
  attempt = args$attempt,
  source_sha256 = data$source_sha256,
  analysis_object_sha256 = data$analysis_object_sha256,
  chain_seed = row$chain_seed, density_seed = row$density_seed, a = row$a, b = row$b, M = row$M,
  s2_mu = row$s2_mu, nu1 = row$nu1, nu2 = row$nu2, model = row$model, prior = row$prior,
  identification = row$identification,
  parameterization = row$parameterization,
  rescale = row$rescale,
  niter = niter, nburnin = nburnin, thin = thin, thin2 = thin2
)

# Fit one chain, then check identification and mixture capacity
rf_execute(job, args, function() {
  suppressPackageStartupMessages(library(DPMirt))
  nimble::nimbleOptions(MCMCprogressBar = FALSE)
  set.seed(row$chain_seed,
    kind = 'Mersenne-Twister',
    normal.kind = 'Inversion',
    sample.kind = 'Rejection'
  )
  capacity_warnings <- character()
  fit <- withCallingHandlers(DPMirt::dpmirt(data$Y,
    model = row$model, prior = row$prior,
    parameterization = row$parameterization, identification = row$identification,
    niter = niter, nburnin = nburnin, thin = thin, thin2 = thin2, nchains = 1L, seed = row$chain_seed,
    alpha_prior = c(a = row$a, b = row$b),
    base_measure = list(s2_mu = row$s2_mu, nu1 = row$nu1, nu2 = row$nu2),
    M = row$M, rescale = isTRUE(row$rescale), compute_waic = FALSE, compute_dp_density = FALSE,
    save_draws = TRUE, save_path = NULL, verbose = FALSE
  ), warning = function(w) {
    if (grepl('attempted to use more components', conditionMessage(w), fixed = TRUE))
      capacity_warnings <<- c(capacity_warnings, conditionMessage(w))
  })

  # Locate person, allocation and item columns by their model names. Check
  # dimensions and centering before accepting a saved chain.
  eta <- grep('^eta\\[', colnames(fit$samples2_raw))
  zi <- grep('^zi\\[', colnames(fit$samples_raw))
  K <- apply(fit$samples_raw[, zi, drop = FALSE], 1, function(z) length(unique(z)))
  beta <- grep('^beta\\[', colnames(fit$samples_raw))
  constants <- fit$compiled$spec$constants
  stopifnot(
    length(eta) == 500L, length(zi) == 500L, length(beta) == 13L,
    all(is.finite(fit$samples_raw)), all(is.finite(fit$samples2_raw)),
    identical(fit$config$identification, 'constrained_item'),
    all(fit$location_shift == 0),
    all(fit$scale_shift == 1),
    identical(fit$theta_samp, fit$samples2_raw[, eta, drop = FALSE]),
    max(abs(rowSums(fit$samples_raw[, beta, drop = FALSE]))) < 1e-7,
    max(K) < row$M, length(capacity_warnings) == 0L,
    identical(as.integer(K), as.integer(fit$cluster_info$n_clusters)),
    as.integer(fit$chain_info$seed) == row$chain_seed,
    nrow(fit$samples_raw) == (niter - nburnin) / thin,
    nrow(fit$samples2_raw) == (niter - nburnin) / thin2,
    as.numeric(constants$sigma2_beta) == 3,
    as.integer(constants$N) == 500L,
    as.integer(constants$I) == 13L
  )
  # Save only uncompiled draws and configuration, not the input matrix or C++ object.
  list(
    schema = 'public-rasch-chain/1', job = job, raw_scores = unname(data$raw_scores),
    samples_raw = fit$samples_raw, samples2_raw = fit$samples2_raw,
    config = fit$config,
    chain_info = fit$chain_info,
    K = as.integer(K),
    capacity_warnings = capacity_warnings,
    identification_checks_passed = TRUE, density_status = 'not-yet-computed'
  )
})
