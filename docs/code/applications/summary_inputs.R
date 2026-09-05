# Author: JoonHo Lee (jlee296@ua.edu)
# Read the application summary exports
#
# Load fit, unit, diagnostic and profile records only after their source
# hashes pass. These summaries come from the retained fits; reading them does
# not run a posterior model.
#
# Sourced by application summary runners and refit selection code.
# Reads: data-frozen/applications/ and its provenance record.

app_read_summaries <- function() {
  receipt <- jsonlite::read_json(rp_path('provenance', 'application-posterior-export.json', must_exist = TRUE),
    simplifyVector = TRUE
  )
  for (i in seq_len(nrow(receipt$exports))) {
    r <- receipt$exports[i, ]
    if (!identical(rp_sha256(file.path(.replication_root, r$export)), r$export_sha256))
      stop('Frozen application summary changed: ', r$export, call. = FALSE)
  }
  read <- function(
    name,
    key = character()
  ) rp_read_csv(rp_path('data-frozen', 'applications', paste0(name, '.csv'), must_exist = TRUE),
    key = key
  )
  x <- list(
    fit = read('fit_summary', 'execution_fit_key'),
    unit = read('unit_summary', c('execution_fit_key', 'unit')),
    base = read('base_manifest', 'fit_key'),
    execution = read('execution_manifest', 'execution_fit_key'),
    diagnostic = read('diagnostic', c('execution_fit_key', 'diagnostic_attempt', 'variable')),
    attempt = read('attempts', c('execution_fit_key', 'attempt')),
    normal = read('normal_benchmark', 'application'),
    star_location = read('star_profiles', c('execution_fit_key', 'moderator', 'level')),
    bd_moderator = read('bd_profiles', c('execution_fit_key', 'moderator'))
  )
  x$normal$app <- ifelse(x$normal$application == 'bangert_drowns', 'bd', x$normal$application)
  stopifnot(
    nrow(x$fit) == 35L,
    all(x$fit$final_diagnostic_pass),
    all(x$fit$retained_partition_draws == 16000L),
    sum(x$fit$final_attempt == 1L) == 25L, sum(x$fit$final_attempt == 2L) == 10L,
    sum(x$fit$analysis_spec == 'primary_scale_aligned') == 15L,
    sum(x$fit$analysis_spec == 'wide_scale_sensitivity') == 15L,
    sum(x$fit$analysis_spec == 'star_hc3_uncertainty') == 5L
  )
  if (!setequal(x$unit$execution_fit_key, x$fit$execution_fit_key)) stop('Unit/fit identities differ.', call. = FALSE)
  for (i in seq_len(nrow(x$fit))) {
    f <- x$fit[i, ]
    u <- x$unit[x$unit$execution_fit_key == f$execution_fit_key, ]
    stopifnot(nrow(u) == f$J, setequal(u$unit, seq_len(f$J)), all(u$final_diagnostic_pass))
  }
  x
}
