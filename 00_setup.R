#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Check the replication environment
#
# Check the packages needed for one task before starting work. Versions and
# the recorded DPprior/DPMirt function definitions are checked separately;
# passing the version check alone does not establish that the fitted model
# code matches.
#
# Run: Rscript 00_setup.R --profile rebuild
# Reads: provenance/software-lock.csv and package-function-lock.csv.
# Writes: console diagnostics only; installation is a separate command.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
if (!length(.file_arg)) stop('Run with Rscript 00_setup.R.', call. = FALSE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1L]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, 'common', 'R', 'bootstrap.R'))
args <- commandArgs(trailingOnly = TRUE)
profile <- 'core'
if (length(args)) {
  if (length(args) != 2L || args[1L] != '--profile') stop('Usage: Rscript 00_setup.R [--profile core|rebuild|calibration|public-data|refit|book]',
    call. = FALSE
  )
  profile <- args[2L]
}
if (!profile %in% c('core', 'rebuild', 'calibration', 'public-data', 'refit', 'book')) stop('Unknown profile.', call. = FALSE)
cat(
  'Replication environment check\nProfile:',
  profile,
  '\nR:',
  as.character(getRversion()),
  '\n'
)
ok <- getRversion() >= '4.5.0'
if (!ok) cat('FAIL: R >= 4.5.0 required; the production reference used R 4.6.0.\n')
lock <- rp_read_csv(rp_path('provenance', 'software-lock.csv', must_exist = TRUE),
  required = c('package', 'version', 'profiles')
)
needed <- vapply(
  strsplit(lock$profiles, ';', fixed = TRUE),
  function(x) profile %in% x,
  logical(1)
)
for (i in which(needed)) {
  p <- lock$package[i]
  installed <- requireNamespace(p, quietly = TRUE)
  version <- if (installed) as.character(utils::packageVersion(p)) else 'missing'
  pass <- installed && utils::packageVersion(p) == package_version(lock$version[i])
  cat(
    if (pass) 'PASS' else 'FAIL',
    p,
    ':',
    version,
    '(expected',
    lock$version[i],
    ')\n'
  )
  ok <- ok && pass
}
if (profile %in% c('calibration', 'refit') &&
  requireNamespace('digest', quietly = TRUE)) {
  functions <- rp_read_csv(rp_path('provenance', 'package-function-lock.csv', must_exist = TRUE))
  for (p in intersect(c('DPprior', 'DPMirt'), lock$package[needed])) {
    if (!requireNamespace(p, quietly = TRUE)) next
    ns <- asNamespace(p)
    expected <- functions[functions$package == p, ]
    valid <- vapply(seq_len(nrow(expected)), function(i) {
      name <- expected$object[i]
      if (!exists(name, envir = ns, inherits = FALSE)) return(FALSE)
      x <- get(name, envir = ns, inherits = FALSE)
      if (!is.function(x)) return(FALSE)
      canonical <- list(
        formals = deparse(formals(x), width.cutoff = 500L),
        body = deparse(body(x), width.cutoff = 500L)
      )
      identical(digest::digest(canonical, algo = 'sha256'), expected$sha256[i])
    }, logical(1))
    cat(
      if (all(valid)) 'PASS' else 'FAIL',
      p,
      'pinned R function definitions:',
      sum(valid),
      '/',
      length(valid),
      '\n'
    )
    ok <- ok && all(valid)
  }
}
if (profile == 'book') {
  quarto <- Sys.which('quarto')
  if (!nzchar(quarto)) {
    cat('FAIL: Quarto is not installed.\n')
    ok <- FALSE
  } else cat(
    'Quarto:',
    system2(quarto, '--version', stdout = TRUE),
    '(production guide baseline 1.9.37)\n'
  )
}
cat('No data downloaded; no installation or model fitting performed.\n')
if (!ok) {
  cat(
    'See Rscript common/install_dependencies.R --profile',
    profile,
    '--execute for explicit installation.\n'
  )
  quit(status = 1L)
}
cat('Environment checks passed for this profile. This does not certify an analysis or another operating system.\n')
