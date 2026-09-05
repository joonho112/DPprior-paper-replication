#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Check exhibit frames and printed numerical claims
#
# Check every declared exhibit and the selected key-number ledger. Numeric
# comparisons use the recorded precision rules; the OSM F.7 row set is also
# compared with a separate export of the original TeX table.
#
# Run after exhibits/00_build_all.R: Rscript verification/check_exhibits.R
# Reads output receipts, numerical frames and independent expected values.
# Writes exhibit and key-number verification results.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))

# Check every declared artifact and its numerical frame
contract <- rp_read_csv(rp_path('provenance', 'artifact-contract.csv'), key = 'artifact_id')
records <- lapply(contract$artifact_id, function(id) {
  x <- jsonlite::read_json(rp_path('outputs', 'exhibit-receipts', paste0(id, '.json'), must_exist = TRUE),
    simplifyVector = TRUE
  )
  stopifnot(x$artifact_id == id, x$rows > 0)
  for (group in c('inputs', 'outputs')) for (i in seq_len(nrow(x[[group]]))) {
    z <- x[[group]][i, ]
    stopifnot(rp_sha256(file.path(.replication_root, z$path)) == z$sha256)
  }
  area <- strsplit(id, '-', fixed = TRUE)[[1]][1]
  frame <- rp_read_csv(rp_path('outputs', area, paste0(id, '.csv'), must_exist = TRUE))
  stopifnot(nrow(frame) == x$rows, identical(names(frame), x$columns))
  data.frame(artifact_id = id, rows = x$rows, tier = x$tier, passed = TRUE)
})
rp_write_csv(
  do.call(rbind, records),
  rp_output_path('outputs', 'verification', 'exhibit_checks.csv')
)

# Compare the key-number ledger with the declared printed values
actual <- rp_read_csv(rp_path('outputs', 'key_numbers.csv'), key = 'claim_id')
expected <- rp_read_csv(rp_path('provenance', 'claim-contract.csv'), key = 'claim_id')
stopifnot(setequal(actual$claim_id, expected$claim_id))
actual <- actual[match(expected$claim_id, actual$claim_id), ]
# Preserve the displayed precision contract even where CSV numeric conversion
# drops trailing zeros. Count means use two decimals; policy probabilities use two.
raw <- read.csv(rp_path('provenance', 'claim-contract.csv'),
  colClasses = 'character',
  check.names = FALSE
)
decimal <- vapply(
  raw$expected_display,
  function(s) if (grepl('.', s, fixed = TRUE)) nchar(sub('.*\\.', '', s)) else 0L,
  integer(1)
)
decimal[grepl('worked-', raw$claim_id)] <- 4L
decimal[grepl('lowI_.*_mean|dual_K|grandmean_K', raw$claim_id)] <- 2L
exact <- grepl('exact integer', expected$comparison_rule) |
  (expected$track == 'rasch' &
    expected$metric %in% c(
      'N',
      'items',
      'perfect_scores',
      'distinct_priors',
      'labeled_fits',
      'retained_draws'
    )) |
  expected$claim_id == 'policy-grid_points'
tolerance <- ifelse(exact, 0, .5 * 10^-decimal) + 1e-12
checks <- data.frame(
  claim_id = expected$claim_id,
  actual = actual$value,
  expected = as.numeric(expected$expected_display),
  tolerance = tolerance
)
checks$difference <- checks$actual - checks$expected
checks$passed <- abs(checks$difference) <= tolerance
rp_write_csv(checks, rp_output_path('outputs', 'verification', 'key_number_checks.csv'))
if (!all(checks$passed)) {
  print(checks[!checks$passed, ])
  stop('A predeclared key number failed.', call. = FALSE)
}
cat(
  length(records),
  'exhibit contracts and',
  nrow(checks),
  'key-number comparisons passed.\n'
)
cat('Separate open manuscript issue: main Table 2 VIF=1.25 prints .15, while the exact tail rounds to .14. This check does not mark that discrepancy as resolved.\n')
# The current OSM F.7 is a selected 36-row display, not the 60-cell medium grid.
# Its independent oracle is parsed from the locked TeX rows, not this builder.

# Check the original OSM F.7 row identities and displayed precision
f7 <- rp_read_csv(rp_path('outputs', 'osm', 'osm-table-F.7.csv'))
e7 <- rp_read_csv(rp_path('verification', 'expected', 'osm_F7_printed.csv'))
keys <- c('prior', 'K_true', 'I_target', 'VIF', 'r')
key <- function(x) do.call(paste, c(x[keys], sep = '|'))
stopifnot(nrow(f7) == 36L, setequal(key(f7), key(e7)))
f7 <- f7[match(key(e7), key(f7)), ]
for (field in c(
  'n_scientific_eligible',
  'mean_E_K',
  'mean_p_K1',
  'mean_tau_rmse',
  'coverage95_K'
)) {
  tol <- switch(field,
    n_scientific_eligible = 0,
    mean_tau_rmse = .000005,
    .0005
  )
  stopifnot(all(abs(f7[[field]] - e7[[field]]) <= tol + 1e-12))
}
cat('Current OSM F.7: exact 36-row identity and printed numerical cells verified against locked TeX.\n')
nonmissing <- e7$effective_lambda != '---'
stopifnot(all(abs(f7$effective_lambda[nonmissing] - as.numeric(e7$effective_lambda[nonmissing])) <= .005 + 1e-12))
