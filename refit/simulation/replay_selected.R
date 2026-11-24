#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Replay one of the four synthetic fits underlying Figure F.5 and Table F.15.
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '',
  grep('^--file=', commandArgs(FALSE), value = TRUE)[1]), fixed = TRUE)))
source(file.path(.script_dir, '..', '..', 'common', 'R', 'bootstrap.R'))
source(rp_path('refit', 'common', 'driver.R'))
source(rp_path('simulation', 'inputs.R'))
args <- rf_args(c('fit-key', 'profile'))
rf_check_job_inputs()

audit <- rp_read_csv(rp_path('data-frozen', 'simulation104', 'selected-replay-audit.csv'))
key <- args[['fit-key']]
if (is.null(key)) key <- audit$fit_key[1L]
record <- audit[audit$fit_key == key, ]
if (nrow(record) != 1L) stop('Select a fit_key from selected-replay-audit.csv.')
manifest <- sim_read_inputs()$manifest
row <- manifest[manifest$fit_key == key, ]
stopifnot(nrow(row) == 1L, row$dgp_sha256 == record$dgp_sha256)
policy <- yaml::read_yaml(file.path(.script_dir, 'policy.yml'))
attempt <- record$attempt
schedule <- policy$mcmc$attempts[[attempt]]
if (args$profile == 'smoke') schedule <- list(n_iter = 200L, n_warmup = 100L)
seeds <- rp_read_csv(rp_path('data-frozen', 'simulation', 'chain_seeds.csv'), key = 'fit_key')
seeds <- as.integer(seeds[match(key, seeds$fit_key), paste0('attempt', attempt, '_chain', 1:4, '_seed')])
stopifnot(identical(seeds, as.integer(strsplit(record$chain_seeds, ',', fixed = TRUE)[[1]])))

job <- list(track = 'selected-simulation-replay', profile = args$profile, fit_key = key,
            dgp_sha256 = record$dgp_sha256, attempt = attempt, schedule = schedule, chain_seeds = seeds,
            distance_code_sha256 = rp_sha256(file.path(.script_dir, 'empirical-distance.cpp')))
rf_execute(job, args, function() {
  engine <- new.env(parent = globalenv())
  for (file in c('keys.R', 'dgp.R', 'prepare.R', 'model.R', 'diagnostics.R'))
    sys.source(file.path(.script_dir, file), envir = engine)
  Rcpp::sourceCpp(file.path(.script_dir, 'empirical-distance.cpp'), env = engine)
  data <- engine$sim_public_data(row, policy)
  # Collect latent effects at the usual end-of-chain reduction, after sampling.
  reduce <- engine$simv2_derived_chain_draws
  pool <- list()
  engine$simv2_derived_chain_draws <- function(samples) {
    cols <- engine$simv2_grep_columns(samples, 'tau_j')
    pool[[length(pool) + 1L]] <<- as.vector(samples[, cols, drop = FALSE])
    reduce(samples)
  }
  fit <- engine$simv2_fit_dp_chains(data$df, row$a, row$b, row$grand_mean_mean,
      row$grand_mean_variance, seeds, schedule$n_iter, schedule$n_warmup, policy$mcmc$thin)
  pass <- engine$simv2_modern_diagnostics(fit$chains)$pass
  draws <- sum(lengths(pool)) / row$J
  pooled <- sort(unlist(pool, use.names = FALSE), method = 'radix')
  truth <- sort(data$df$tau_true)
  gr <- as.numeric(quantile(pooled, (seq_len(row$J) - .5) / row$J, type = 1))
  direct <- engine$sorted_cdf_distance(pooled, truth)
  constrained_rank <- engine$sorted_cdf_distance(gr, truth)
  metrics <- c(posterior_CDF_ISEL = unname(direct[1]), posterior_CDF_KS = unname(direct[2]),
               GR_ISEL = unname(constrained_rank[1]), GR_KS = unname(constrained_rank[2]))
  old <- rp_read_csv(rp_path('data-frozen', 'simulation104', 'selected-truth-distances.csv'))
  old <- old[old$prior == record$prior & old$K_true == row$K_true & old$replicate == row$replicate, ]
  stopifnot(nrow(old) == 1L)
  difference <- metrics - as.numeric(old[1, names(metrics)])
  grid <- seq(-1.5, 1.5, by = .0025)
  curves <- data.frame(x = grid, posterior_finite_CDF = findInterval(grid, pooled)/length(pooled),
                       GR_CDF = findInterval(grid, gr)/row$J, true_CDF = findInterval(grid, truth)/row$J)
  if (args$profile == 'production' && draws != record$posterior_draws)
    stop('The replay did not retain the recorded number of draws.')
  list(metrics = metrics, difference_from_archived_replay = difference, curves = curves,
       draws = draws, diagnostics_pass = pass, max_absolute_error = max(abs(difference)))
})
