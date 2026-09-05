#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Combine four explicitly indexed Rasch results
#
# Read a user-supplied index rather than discovering result folders. Verify
# each saved result against its receipt and declared job before pooling;
# chains from different attempts or sampling profiles cannot be mixed.
#
# Run: Rscript refit/rasch/combine_and_check.R --arm main --role vague
# Execution also needs --index CSV --input RDS --execute.
# Index columns: chain, run_directory (relative to outputs/refit/rasch/).

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', '..', 'common', 'R', 'bootstrap.R'))
source(rp_path('refit', 'common', 'driver.R', must_exist = TRUE))
args <- rf_args(c('arm', 'role', 'attempt', 'profile', 'index', 'input'))
rf_check_job_inputs()

# Identify the expected four-chain set
arm <- if (is.null(args$arm)) 'main' else args$arm
role <- if (is.null(args$role)) 'vague' else args$role
jobs <- rp_read_csv(rp_path('data-frozen', 'rasch', 'chain_jobs.csv', must_exist = TRUE),
  key = 'job_id'
)
expected <- jobs[jobs$arm == arm & jobs$role == role & jobs$attempt == args$attempt, ]
if (nrow(expected) != 4L || !setequal(expected$chain, 1:4)) stop('No exact four-chain declared set.', call. = FALSE)

# Describe the combination and require explicit inputs for execution
job <- list(
  track = 'rasch-combined',
  profile = args$profile,
  arm = arm,
  role = role,
  attempt = args$attempt,
  expected_job_ids = expected$job_id,
  input_index_sha256 = if (!is.null(args$index)) rp_sha256(args$index) else NULL
)
if (args$execute && (is.null(args$index) || is.null(args$input)))
  stop('Execution requires --index and --input; no result-directory discovery is performed.',
    call. = FALSE
  )
if (!is.null(args$input)) job$source_sha256 <- rp_sha256(args$input)

# Verify the indexed chain results before pooling
rf_execute(job, args, function() {
  source(file.path(.script_dir, 'data_loader.R'))
  source(file.path(.script_dir, 'diagnostics.R'))
  source(file.path(.script_dir, 'combine_helpers.R'))
  data <- rasch_source_matrix(args$input)
  index <- rp_read_csv(args$index, required = c('chain', 'run_directory'), key = 'chain')
  stopifnot(nrow(index) == 4L, setequal(index$chain, 1:4))

  # Every indexed result must match its declared job and the reconstructed
  # response matrix. File presence alone is insufficient.
  chains <- lapply(seq_len(nrow(index)), function(i) {
    folder <- normalizePath(file.path(.replication_root, index$run_directory[i]),
      winslash = '/',
      mustWork = TRUE
    )
    if (!startsWith(folder, paste0(rp_path('outputs', 'refit', 'rasch'), '/'))) stop('Chain index must point inside outputs/refit/rasch.', call. = FALSE)
    receipt <- jsonlite::read_json(file.path(folder, 'receipt.json'), simplifyVector = TRUE)
    if (!identical(receipt$status, 'complete') || !identical(receipt$profile, args$profile) ||
      !identical(
        receipt$result_sha256,
        rp_sha256(file.path(folder, 'result.rds'))
      )) stop('Unverified chain receipt.', call. = FALSE)
    x <- readRDS(file.path(folder, 'result.rds'))
    e <- expected[expected$chain == index$chain[i], ]
    if (!identical(x$job$job_id, e$job_id) || x$job$chain != index$chain[i] ||
      x$job$chain_seed != e$chain_seed || x$job$density_seed != e$density_seed ||
      !identical(x$job$analysis_object_sha256, data$analysis_object_sha256)) stop('Indexed chain identity differs from the declared job.', call. = FALSE)
    fields <- c(
      'arm',
      'role',
      'attempt',
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
      'rescale'
    )
    for (field in fields) if (!isTRUE(all.equal(x$job[[field]], e[[field]], check.attributes = FALSE)))
      stop('Chain setting differs from declared job: ', field, call. = FALSE)
    schedule <- if (args$profile == 'smoke') list(niter = 60, nburnin = 30, thin = 1, thin2 = 1) else as.list(e[c('niter', 'nburnin', 'thin', 'thin2')])
    for (field in names(schedule)) if (!isTRUE(all.equal(x$job[[field]], schedule[[field]], check.attributes = FALSE)))
      stop('Chain schedule differs from selected profile.', call. = FALSE)
    x
  })
  rasch_combine(chains, data$Y)
})
