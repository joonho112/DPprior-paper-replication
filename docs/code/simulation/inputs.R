# Author: JoonHo Lee (jlee296@ua.edu)
# Load and compare the frozen simulation records
#
# Verify the exported fit metrics, planned jobs and calibration records before
# joining them. The comparison helper aligns rows by scientific keys so
# changes in row order cannot disguise a missing or duplicated result.
#
# Sourced by simulation runners and verification scripts.
# Reads: data-frozen/simulation/ and provenance/simulation-export.json.

# Check source fingerprints and reconcile fit-level identities with the
# planned design and terminal-status denominators before expanding shared
# metadata.
sim_read_inputs <- function() {
  receipt <- jsonlite::read_json(rp_path('provenance', 'simulation-export.json', must_exist = TRUE),
    simplifyVector = TRUE
  )
  for (i in seq_len(nrow(receipt$exports))) {
    r <- receipt$exports[i, ]
    p <- file.path(.replication_root, r$path)
    if (!identical(rp_sha256(p), r$sha256)) stop('Frozen simulation input changed: ', r$path, call. = FALSE)
  }
  read <- function(
    name,
    key
  ) rp_read_csv(rp_path('data-frozen', 'simulation', name, must_exist = TRUE),
    key = key
  )
  design <- read('design.csv', 'design_id')
  status <- read('fit_status.csv', 'fit_key')
  fits <- read('fit_metrics.csv', 'fit_key')
  denoms <- read('cell_denominators.csv', c('suite', 'conceptual_cell_key'))
  calibration <- read('calibration.csv', 'calibration_id')
  if (!setequal(fits$fit_key, status$fit_key[status$scientific_eligible])) stop('Retained fit identities disagree.', call. = FALSE)
  match_status <- status[match(fits$fit_key, status$fit_key), ]
  for (k in c('design_id', 'replicate', 'dgp_sha256', 'retry'))
    if (!identical(fits[[k]], match_status[[k]])) stop('Fit/status identity mismatch: ', k, call. = FALSE)
  if (anyNA(match(status$design_id, design$design_id))) stop('Unknown design.', call. = FALSE)
  if (anyDuplicated(paste(status$design_id, status$replicate, sep = ':'))) stop('Duplicate design/replicate.', call. = FALSE)
  # Reconcile collector denominators independently against every terminal status.
  for (i in seq_len(nrow(denoms))) {
    id <- paste(denoms$suite[i], denoms$conceptual_cell_key[i], sep = '::')
    z <- status[status$design_id == id, ]
    observed <- c(
      n_manifest = nrow(z), n_runnable = sum(z$runnable),
      n_scientific_eligible = sum(z$scientific_eligible),
      n_dependency_excluded = sum(z$status_category == 'dependency_rejection'),
      n_diagnostic_excluded = sum(z$status_category == 'final_diagnostic_failure'),
      n_other_excluded = sum(!z$status_category %in% c('success', 'dependency_rejection', 'final_diagnostic_failure'))
    )
    if (!identical(as.numeric(observed), as.numeric(denoms[i, names(observed)])))
      stop('Terminal status denominators disagree: ', id, call. = FALSE)
  }
  # Preserve original row order while restoring repeated metadata for the
  # manuscript's aggregation functions. No numeric result comes from expected.
  expand <- function(x) cbind(
    x,
    design[match(x$design_id, design$design_id),
      setdiff(names(design), 'design_id'),
      drop = FALSE
    ]
  )
  list(
    fits = expand(fits),
    manifest = expand(status),
    denominators = denoms,
    calibration = calibration
  )
}

# Align actual and expected rows by key, then compare missingness, numeric
# values and nonnumeric fields separately. Return one check row per compared
# column.
sim_compare <- function(actual, expected, key, tolerance = 1e-10) {
  keys <- function(x) do.call(paste, c(x[key], sep = '\r'))
  ak <- keys(actual)
  ek <- keys(expected)
  if (anyDuplicated(ak) || anyDuplicated(ek) || !setequal(ak, ek) ||
    !setequal(names(actual), names(expected)))
    stop('Comparison schema or keys differ.', call. = FALSE)
  actual <- actual[match(ek, ak), names(expected), drop = FALSE]
  rows <- lapply(names(expected), function(n) {
    a <- actual[[n]]
    e <- expected[[n]]
    missing_equal <- identical(is.na(a), is.na(e))
    use <- !is.na(a) & !is.na(e)
    numeric <- is.numeric(a) && is.numeric(e)
    delta <- if (numeric && any(use)) max(abs(a[use] - e[use])) else NA_real_
    ok <- missing_equal &&
      if (numeric) all(abs(a[use] - e[use]) <= tolerance) else identical(as.character(a[use]), as.character(e[use]))
    data.frame(
      column = n,
      n_compared = sum(use),
      max_absolute_difference = delta,
      tolerance = if (numeric) tolerance else NA_real_,
      passed = ok
    )
  })
  do.call(rbind, rows)
}
