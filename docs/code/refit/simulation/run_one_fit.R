#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Run one synthetic four-chain fit
#
# Select one runnable frozen fit key, regenerate its data and use its recorded
# seeds. The smoke profile shortens each chain to 200 iterations with 100
# warmup iterations; those draws do not replace the paper's results.
#
# Run: Rscript refit/simulation/run_one_fit.R --profile smoke
# Add --execute to compile and sample. Use --fit-key to select a different
# job.
# Writes: a new job directory under outputs/refit/simulation/ only on
# execution.

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
if (args$attempt > 2L) stop('Simulation has only two declared schedules.', call. = FALSE)
source(rp_path('simulation', 'inputs.R', must_exist = TRUE))
for (file in c('keys.R', 'dgp.R', 'prepare.R', 'model.R', 'diagnostics.R')) source(file.path(.script_dir, file))

# Select one runnable fit from the frozen manifest
x <- sim_read_inputs()$manifest
if (is.null(args[['fit-key']])) {
  candidate <- which(x$J == 25L & x$prior == 'Vague' & x$runnable)[1]
  row <- x[candidate, ]
} else row <- x[x$fit_key == args[['fit-key']], ]
if (nrow(row) != 1L || !isTRUE(row$runnable)) stop('Select exactly one runnable frozen fit key.', call. = FALSE)

# Recreate the dataset and retrieve the recorded chain seeds
policy <- yaml::read_yaml(file.path(.script_dir, 'policy.yml'))
dataset <- sim_public_data(row, policy)
seed_table <- rp_read_csv(rp_path('data-frozen', 'simulation', 'chain_seeds.csv', must_exist = TRUE),
  key = 'fit_key'
)
seeds <- as.integer(seed_table[
  match(row$fit_key, seed_table$fit_key),
  paste0('attempt', args$attempt, '_chain', 1:4, '_seed')
])

# Apply the chosen attempt or the bounded smoke schedule
schedule <- policy$mcmc$attempts[[args$attempt]]
if (args$profile == 'smoke') {
  schedule$n_iter <- 200L
  schedule$n_warmup <- 100L
}
code_files <- c(
  'keys.R',
  'dgp.R',
  'prepare.R',
  'model.R',
  'diagnostics.R',
  'run_one_fit.R',
  'policy.yml'
)

# Record the complete model job before execution
job <- list(
  track = 'simulation',
  profile = args$profile,
  fit_key = row$fit_key,
  dgp_sha256 = row$dgp_sha256,
  dgp_seed = row$dgp_seed, J = row$J, K_true = row$K_true, a = row$a, b = row$b,
  grand_mean_mean = row$grand_mean_mean, grand_mean_variance = row$grand_mean_variance,
  attempt = args$attempt,
  chain_seeds = seeds,
  n_iter = schedule$n_iter,
  n_warmup = schedule$n_warmup,
  thin = 1L,
  code_sha256 = setNames(
    lapply(code_files, function(f) rp_sha256(file.path(.script_dir, f))),
    code_files
  )
)

# Fit the four chains and retain their diagnostic results
rf_execute(job, args, function() {
  fit <- simv2_fit_dp_chains(dataset$df,
    row$a,
    row$b,
    row$grand_mean_mean,
    row$grand_mean_variance,
    seeds, schedule$n_iter, schedule$n_warmup,
    thin = 1L
  )
  diagnostics <- simv2_modern_diagnostics(fit$chains)
  list(
    schema = 'public-simulation-fit/1',
    job = job,
    data = dataset,
    fit = fit,
    diagnostics = diagnostics,
    purpose = if (args$profile == 'smoke') 'compile/sampling/output path only' else 'new local fit; inspect diagnostic gates'
  )
})
