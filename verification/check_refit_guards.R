#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Check result reuse and job identity
#
# Use a small artificial result to test dry-run, completed-result reuse and
# refusal of corrupted or mismatched results. The test does not compile NIMBLE
# or run MCMC.
#
# Run: Rscript verification/check_refit_guards.R
# Creates and removes its own test output directories; writes a verification
# receipt.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(rp_path('refit', 'common', 'driver.R'))
source(rp_path('refit', 'rasch', 'combine_helpers.R'))
for (p in list.files(rp_path('refit'),
  pattern = '\\.R$',
  recursive = TRUE,
  full.names = TRUE
)) parse(p)

# Exercise result reuse with a disposable synthetic job
checks <- character()
expect_error <- function(expr, label) {
  result <- tryCatch(
    {
      force(expr)
      FALSE
    },
    error = function(e) TRUE
  )
  if (!result) stop('Negative control was not detected: ', label)
  checks <<- c(checks, label)
}
# This is a chain-identity fixture, not a set of fitted chains.
chains <- lapply(1:4, function(k) list(
  job = list(
    chain = k, chain_seed = k, density_seed = k + 4,
    arm = 'fixture',
    role = 'fixture',
    attempt = 1L,
    profile = 'smoke',
    analysis_object_sha256 = 'fixture',
    a = 1, b = 1, M = 100, s2_mu = 2, nu1 = 2.01, nu2 = 1.01, model = 'rasch', prior = 'dpm',
    identification = 'constrained_item',
    parameterization = 'irt',
    niter = 60,
    nburnin = 30,
    thin = 1,
    thin2 = 1
  ),
  samples_raw = matrix(1, 2, 1, dimnames = list(NULL, 'alpha')),
  samples2_raw = matrix(1, 2, 1, dimnames = list(NULL, 'eta[1]')),
  identification_checks_passed = TRUE
))
stopifnot(length(rasch_validate_chain_set(chains)) == 4)
expect_error(rasch_validate_chain_set(chains[1:3]), 'missing chain')
x <- chains
x[[4]]$job$chain <- 1L
expect_error(rasch_validate_chain_set(x), 'duplicate chain')
x <- chains
x[[4]]$job$attempt <- 2L
expect_error(rasch_validate_chain_set(x), 'mixed attempt')
x <- chains
x[[4]]$job$profile <- 'production'
expect_error(rasch_validate_chain_set(x), 'mixed profile')
x <- chains
x[[4]]$job$chain_seed <- 1L
expect_error(rasch_validate_chain_set(x), 'repeated seed')
x <- chains
x[[4]]$samples_raw[1, 1] <- NA
expect_error(rasch_validate_chain_set(x), 'nonfinite draws')
# Exercise the real receipt/hash/resume boundary with a trivial saved value.
job <- list(track = 'guard-fixture', profile = 'smoke', fixture = TRUE, seed = 123L)
args <- list(execute = TRUE, resume = FALSE)
calls <- 0L
invisible(capture.output(rf_execute(job, args, function() {
  calls <<- calls + 1L
  list(fixture = TRUE)
})))
folder <- rp_path('outputs', 'refit', 'guard-fixture')
args$resume <- TRUE
invisible(capture.output(rf_execute(
  job,
  args,
  function() {
    calls <<- calls + 1L
    stop('Should not execute')
  }
)))
stopifnot(calls == 1L)
checks <- c(checks, 'resume avoids repeated execution')
result <- list.files(folder, pattern = 'result.rds', recursive = TRUE, full.names = TRUE)
stopifnot(length(result) == 1L)
saveRDS(list(corrupted = TRUE), result)
invisible(capture.output(expect_error(
  rf_execute(job, args, function() NULL),
  'corrupt receipt rejected'
)))
unlink(folder, recursive = TRUE)
rp_write_csv(
  data.frame(check = checks, passed = TRUE),
  rp_output_path('outputs', 'verification', 'refit_guard_checks.csv')
)
cat(length(checks), 'refit boundary checks passed; synthetic fixtures only.\n')
