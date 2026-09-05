# Author: JoonHo Lee (jlee296@ua.edu)
# Load public study data and check prepared inputs
#
# Obtain STAR and writing-to-learn data from the recorded package versions.
# Compare prepared school/study rows with the frozen analysis input before
# fitting; the shared normal benchmark uses REML.
#
# Sourced by applications/01_prepare_public_data.R, 02_first_stage.R and
# 05_prepare_ocrs.R.
# Participant-level STAR records remain in memory; only reviewed summaries are
# saved.

# Load the pinned STAR table and verify the kindergarten complete-case sample
# counts. Return the source table for the first-stage routine.
app_load_star <- function() {
  if (as.character(utils::packageVersion('mlmRev')) != '1.0.9') stop('Use pinned mlmRev 1.0-9.', call. = FALSE)
  e <- new.env(parent = emptyenv())
  utils::data('star', package = 'mlmRev', envir = e)
  x <- e$star
  k <- x[x$gr == 'K', ]
  stopifnot(
    nrow(k) == 6325L,
    sum(complete.cases(k[c('cltype', 'sch', 'read', 'math', 'tch')])) == 5786L
  )
  x
}

# Use the package-provided effect sizes and sampling variances. Construct one
# row per study in the order used by the original analysis.
app_load_bd <- function() {
  if (as.character(utils::packageVersion('metadat')) != '1.6.0') stop('Use pinned metadat 1.6-0.', call. = FALSE)
  e <- new.env(parent = emptyenv())
  utils::data('dat.bangertdrowns2004', package = 'metadat', envir = e)
  d <- e$dat.bangertdrowns2004
  d <- d[!is.na(d$yi) & !is.na(d$vi), ]
  stopifnot(nrow(d) == 48L, all(d$vi > 0))
  data.frame(
    unit = seq_len(nrow(d)), study = paste(d$author, d$year), tau_hat = d$yi,
    se2 = d$vi, se = sqrt(d$vi), n_j = d$ni, grade = d$grade, length = d$length,
    data_variant = 'bangert_drowns_primary'
  )
}

# Match the prepared input to its frozen reference by unit, compare every
# reference field and save discrepancies before stopping on a mismatch.
app_input_parity <- function(actual, name, key = 'unit', tolerance = 1e-8) {
  expected <- rp_read_csv(rp_path('data-frozen', 'applications', paste0(name, '.csv'), must_exist = TRUE),
    key = key
  )
  if (anyDuplicated(actual[[key]]) || !setequal(actual[[key]], expected[[key]])) stop('Input keys changed: ', name, call. = FALSE)
  actual <- actual[match(expected[[key]], actual[[key]]), ]
  checks <- lapply(names(expected), function(n) {
    a <- actual[[n]]
    e <- expected[[n]]
    if (is.null(a)) stop('Missing input field: ', n, call. = FALSE)
    use <- !is.na(a) & !is.na(e)
    numeric <- is.numeric(a) && is.numeric(e)
    delta <- if (numeric && any(use)) max(abs(a[use] - e[use])) else NA_real_
    ok <- identical(is.na(a), is.na(e)) &&
      if (numeric) all(abs(a[use] - e[use]) <= tolerance) else identical(as.character(a[use]), as.character(e[use]))
    data.frame(dataset = name, column = n, max_absolute_difference = delta, passed = ok)
  })
  result <- do.call(rbind, checks)
  rp_write_csv(
    result,
    rp_output_path('outputs', 'verification', paste0(name, '_input_checks.csv'))
  )
  if (!all(result$passed)) stop('Input drift: ', name, '; inspect parity receipt.', call. = FALSE)
  invisible(result)
}

# Fit the normal random-effects comparison by REML. Return the pooled and
# predictive summaries together with unit-level BLUPs.
app_normal_benchmark <- function(d, application) {
  yi_values <- d$tau_hat
  vi_values <- d$se2
  fit <- metafor::rma.uni(yi = yi_values, vi = vi_values, method = 'REML')
  pr <- predict(fit, level = 95)
  bl <- metafor::blup(fit, level = 95)
  list(
    summary = data.frame(
      application = application,
      data_variant = unique(d$data_variant),
      J = nrow(d),
      pooled_effect = unname(fit$b[[1]]),
      pooled_se = unname(fit$se),
      pooled_ci_lower = unname(fit$ci.lb),
      pooled_ci_upper = unname(fit$ci.ub),
      tau2_REML = unname(fit$tau2),
      I2_percent = unname(fit$I2),
      prediction_lower = unname(pr$pi.lb), prediction_upper = unname(pr$pi.ub)
    ),
    units = data.frame(
      application = application,
      data_variant = unique(d$data_variant),
      unit = d$unit,
      blup = as.numeric(bl$pred), q025 = as.numeric(bl$pi.lb), q975 = as.numeric(bl$pi.ub)
    )
  )
}
