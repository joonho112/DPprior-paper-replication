# Author: JoonHo Lee (jlee296@ua.edu)
# Run one explicitly selected model job
#
# The default is a dry run. When execution is requested, bind the data,
# settings, seeds, source files and software pins to the result. A completed
# matching result may be reused with --resume; interrupted runs are not
# checkpoints.
#
# Sourced by the simulation, application and Rasch refit entrypoints.
# Writes new executed jobs only under outputs/refit/.
# Source comments and formatting affect byte hashes, so they also change run
# IDs.

# Read explicit execution options
#
# Production is the default schedule, but execution remains off unless
# --execute is supplied. --resume only permits verified completed-result
# reuse.
rf_args <- function(allowed = c('fit-key', 'attempt', 'profile', 'chain', 'input', 'arm', 'role')) {
  args <- commandArgs(trailingOnly = TRUE)
  out <- list(execute = FALSE, resume = FALSE, profile = 'production', attempt = 1L)
  i <- 1L
  while (i <= length(args)) {
    flag <- sub('^--', '', args[i])
    if (flag %in% c('execute', 'resume')) out[[flag]] <- TRUE
    else {
      if (!flag %in% allowed || i == length(args)) stop('Unknown or incomplete argument: ', args[i], call. = FALSE)
      i <- i + 1L
      out[[flag]] <- args[i]
    }
    i <- i + 1L
  }
  if (!out$profile %in% c('production', 'smoke')) stop('Profile must be production or smoke.', call. = FALSE)
  out$attempt <- as.integer(out$attempt)
  if (!is.finite(out$attempt) || out$attempt < 1L || out$attempt > 3L) stop('Invalid attempt.', call. = FALSE)
  out
}

# Verify the frozen job and seed tables
rf_check_job_inputs <- function() {
  reg <- jsonlite::read_json(rp_path('provenance', 'refit-job-inputs.json', must_exist = TRUE),
    simplifyVector = TRUE
  )$exports
  for (i in seq_len(nrow(reg))) if (!identical(
    rp_sha256(file.path(.replication_root, reg$path[i])),
    reg$sha256[i]
  ))
    stop('Refit job inputs changed.', call. = FALSE)
}

# Check package versions and the actual R function definitions
rf_check_software <- function() {
  lock <- rp_read_csv(rp_path('provenance', 'software-lock.csv', must_exist = TRUE))
  needed <- vapply(
    strsplit(lock$profiles, ';', fixed = TRUE),
    function(x) 'refit' %in% x,
    logical(1)
  )
  for (i in which(needed)) if (!requireNamespace(lock$package[i], quietly = TRUE) ||
    utils::packageVersion(lock$package[i]) != package_version(lock$version[i]))
    stop('Pinned refit dependency unavailable: ',
      lock$package[i],
      '; run 00_setup.R --profile refit.',
      call. = FALSE
    )
  definitions <- rp_read_csv(rp_path('provenance', 'package-function-lock.csv', must_exist = TRUE))
  for (i in seq_len(nrow(definitions))) {
    r <- definitions[i, ]
    ns <- asNamespace(r$package)
    if (!exists(r$object, envir = ns, inherits = FALSE)) stop('Pinned package definition missing.', call. = FALSE)
    f <- get(r$object, envir = ns, inherits = FALSE)

    # Hash the complete job description before creating any output. Byte
    # changes to these scripts intentionally produce a different run
    # directory.
    canonical <- list(
      formals = deparse(formals(f), width.cutoff = 500L),
      body = deparse(body(f), width.cutoff = 500L)
    )
    if (!identical(digest::digest(canonical, algo = 'sha256'), r$sha256)) stop('Pinned package function changed.', call. = FALSE)
  }
  invisible(TRUE)
}

# Describe, execute or reuse one exact job
rf_execute <- function(job, args, run) {
  # Job identity binds data, settings, seeds, code and software version pins.
  job$software_lock_sha256 <- rp_sha256(rp_path('provenance', 'software-lock.csv', must_exist = TRUE))
  code <- sort(c(
    list.files(rp_path('refit'),
      pattern = '\\.(R|yml)$',
      recursive = TRUE,
      full.names = TRUE
    ),
    list.files(rp_path('common', 'R'), pattern = '\\.R$', full.names = TRUE)
  ))
  names_code <- substring(code, nchar(.replication_root) + 2L)
  job$public_code_sha256 <- setNames(lapply(code, rp_sha256), names_code)
  canonical <- jsonlite::toJSON(job, auto_unbox = TRUE, digits = NA, null = 'null', na = 'null')
  id <- digest::digest(canonical, algo = 'sha256', serialize = FALSE)
  job$run_id <- id
  cat(jsonlite::toJSON(job, auto_unbox = TRUE, pretty = TRUE, digits = NA, na = 'null'), '\n')
  if (!isTRUE(args$execute)) {
    cat('DRY RUN: no model compiled, no sampling, no output directory created.\n')
    return(invisible(NULL))
  }

  # Everything above is available in dry-run mode. Compilation and sampling
  # can occur only after this point.
  rf_check_software()
  folder <- rp_path('outputs', 'refit', job$track, id)
  result_path <- file.path(folder, 'result.rds')
  receipt_path <- file.path(folder, 'receipt.json')

  # Do not treat the presence of a directory as a completed run. Verify both
  # the completion record and the saved result hash.
  if (dir.exists(folder)) {
    if (isTRUE(args$resume) && file.exists(receipt_path) && file.exists(result_path)) {
      old <- jsonlite::read_json(receipt_path, simplifyVector = TRUE)
      if (identical(old$run_id, id) && identical(old$status, 'complete') &&
        identical(rp_sha256(result_path), old$result_sha256)) {
        cat('Verified existing completed result; no fitting repeated.\n')
        return(invisible(readRDS(result_path)))
      }
    }
    stop('Existing output is not a verified completed result for --resume; refusing reuse or overwrite.',
      call. = FALSE
    )
  }
  dir.create(folder, recursive = TRUE)
  rp_write_json(job, file.path(folder, 'job.json'))
  # These process-local limits avoid nested math-library parallelism.
  Sys.setenv(
    OMP_NUM_THREADS = '1',
    OPENBLAS_NUM_THREADS = '1',
    MKL_NUM_THREADS = '1',
    VECLIB_MAXIMUM_THREADS = '1'
  )
  start <- proc.time()[['elapsed']]
  value <- tryCatch(run(), error = function(e) {
    rp_write_json(
      list(
        run_id = id,
        status = 'failed',
        message = conditionMessage(e),
        profile = job$profile
      ),
      receipt_path
    )
    stop(e)
  })

  # Write the result to a temporary file in the same directory, then rename it
  # before issuing the completion receipt.
  temp <- tempfile('result-', tmpdir = folder)
  on.exit(unlink(temp), add = TRUE)
  saveRDS(value, temp, version = 3)
  if (!file.rename(temp, result_path)) stop('Could not publish result file.', call. = FALSE)
  rp_write_json(list(
    run_id = id, status = 'complete', profile = job$profile,
    scientific_acceptance = if (job$profile == 'smoke') 'not assessed: smoke only' else 'inspect saved diagnostic gates',
    elapsed_seconds = proc.time()[['elapsed']] - start, result_sha256 = rp_sha256(result_path),
    manuscript_result_replaced = FALSE
  ), receipt_path)
  cat('Completed ',
    job$profile,
    ' job; output: outputs/refit/',
    job$track,
    '/',
    id,
    '\n',
    sep = ''
  )
  invisible(value)
}
