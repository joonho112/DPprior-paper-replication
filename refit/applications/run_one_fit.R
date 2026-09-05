#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Run one application fit
#
# Select a named prior for a prepared school/study input and verify that input
# against the original analysis rows. Use the recorded attempt schedule and
# seeds; a smoke run is only a short execution check.
#
# Example: Rscript refit/applications/run_one_fit.R --arm ocrs --role Vague
# --profile smoke
# Add --execute to compile and sample; the default prints the selected job.
# Prepare the corresponding data-derived/applications/ input first.

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
if (args$attempt > 2L) stop('Applications have two declared schedules.', call. = FALSE)
source(rp_path('applications', 'summary_helpers.R', must_exist = TRUE))
source(rp_path('applications', 'summary_inputs.R', must_exist = TRUE))
source(rp_path('applications', 'data_loaders.R', must_exist = TRUE))

# Select the application and prior
x <- app_read_summaries()
app <- if (is.null(args$arm)) 'ocrs' else args$arm
role <- if (is.null(args$role)) 'Vague' else args$role
if (is.null(args[['fit-key']])) {
  row <- x$fit[x$fit$app == app & x$fit$prior_label == role &
    x$fit$analysis_spec == 'primary_scale_aligned', ]
} else row <- x$fit[x$fit$execution_fit_key == args[['fit-key']], ]
if (nrow(row) != 1L) stop('Select one application execution key, or arm/role pair.',
  call. = FALSE
)

# Load the declared model settings and verify the prepared input
settings <- x$execution[match(row$execution_fit_key, x$execution$execution_fit_key), ]
input <- rp_path('data-derived',
  'applications',
  paste0(row$data_variant, '.csv'),
  must_exist = TRUE
)
d <- rp_read_csv(input, key = 'unit')
app_input_parity(d, row$data_variant)
for (file in c('keys.R', 'model.R', 'diagnostics.R')) source(rp_path('refit', 'simulation', file, must_exist = TRUE))
source(file.path(.script_dir, 'model.R'))

# Use the recorded attempt-specific streams and schedule
seeds <- as.integer(settings[paste0('attempt', args$attempt, '_chain', 1:4, '_seed')])
n_iter <- settings[[paste0('attempt', args$attempt, '_n_iter')]]
n_warmup <- settings[[paste0('attempt', args$attempt, '_n_warmup')]]
if (args$profile == 'smoke') {
  n_iter <- 200L
  n_warmup <- 100L
}

# Record the exact input, prior and code identity
job <- list(
  track = 'applications',
  profile = args$profile,
  execution_fit_key = row$execution_fit_key,
  app = row$app,
  role = row$prior_label,
  input_sha256 = rp_sha256(input),
  J = nrow(d),
  a = settings$a,
  b = settings$b,
  grand_mean_mean = settings$grand_mean_mean,
  grand_mean_variance = settings$grand_mean_variance,
  sigma2_tau_upper = settings$sigma2_tau_upper, attempt = args$attempt, chain_seeds = seeds,
  n_iter = n_iter,
  n_warmup = n_warmup,
  thin = settings$thin,
  population_seed = settings$population_seed,
  model_sha256 = rp_sha256(file.path(.script_dir, 'model.R')),
  driver_sha256 = rp_sha256(file.path(.script_dir, 'run_one_fit.R'))
)

# Run the selected fit through the shared execution helper
rf_execute(
  job,
  args,
  function() appv2_fit_chains(d,
    settings$a,
    settings$b,
    settings$grand_mean_mean,
    settings$grand_mean_variance, settings$sigma2_tau_upper, seeds, n_iter, n_warmup,
    thin = settings$thin,
    maximum_retained_per_chain = settings$maximum_retained_per_chain,
    population_seed = settings$population_seed
  )
)
