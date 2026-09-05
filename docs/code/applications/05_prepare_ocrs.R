#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Prepare the public OCRS school contrasts
#
# Read the official AIR/ICPSR V1 impact-analysis CSV supplied by the reader.
# Standardize scores across the complete-case sample before estimating a
# treatment contrast within each school. These are descriptive contrasts, not
# the original blocked ITT estimator.
#
# Run: Rscript applications/05_prepare_ocrs.R --input PATH
# PATH is the separately acquired OCRS_publicuse_impactanalysisdata.csv.
# Writes: 24 school rows, normal-model summaries and a source receipt.
# No student records are exported and no posterior model is fitted here.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(file.path(.script_dir, 'data_loaders.R'))

# Require an explicit public-data path
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L || args[1] != '--input') stop(
  'Supply --input followed by the public-use impact-analysis CSV path; see DATA_ACCESS.md.',
  call. = FALSE
)
input <- normalizePath(args[2], winslash = '/', mustWork = TRUE)

# Verify the exact official V1 source before analysis
contract <- jsonlite::read_json(rp_path('provenance', 'ocrs-public-source.json', must_exist = TRUE),
  simplifyVector = TRUE
)
expected_hash <- contract$files$sha256[contract$files$required_for_paper]
actual_hash <- rp_sha256(input)
if (length(expected_hash) != 1L || !identical(actual_hash, expected_hash))
  stop('OCRS source differs from the documented V1 CSV. Inspect the release before proceeding.',
    call. = FALSE
  )
raw <- rp_read_csv(input, required = c('sts_irtscore', 'ragrp', 'studyschids'))
# Standardize across all retained subject observations, before school regressions.
# These are observation rows: some source students occur in both subject samples.
oc <- raw[!is.na(raw$sts_irtscore) & !is.na(raw$ragrp), ]
stopifnot(nrow(raw) == 1737L, nrow(oc) == 1063L, !anyNA(oc$studyschids))
oc$y <- as.numeric(scale(oc$sts_irtscore))
oc$trt <- as.integer(oc$ragrp == 1)

# Estimate one descriptive contrast per school
schools <- sort(unique(oc$studyschids))
rows <- lapply(schools, function(school) {
  d <- oc[oc$studyschids == school, ]
  if (length(unique(d$trt)) < 2L || nrow(d) < 10L) return(NULL)
  fit <- lm(y ~ trt, data = d)
  data.frame(
    studyschids = school, n_j = nrow(d), n_trt = sum(d$trt), n_ctl = sum(1 - d$trt),
    tau_hat = unname(coef(fit)[2]), se = unname(sqrt(diag(vcov(fit)))[2])
  )
})

# Construct and check the 24-school input
result <- do.call(rbind, rows)
result$unit <- seq_len(nrow(result))
result$se2 <- result$se^2
result$data_variant <- 'ocrs_descriptive_primary'
stopifnot(nrow(result) == 24L, sum(result$n_j) == 1063L)
app_input_parity(result, 'ocrs_descriptive_primary')
rp_write_csv(
  result,
  rp_output_path('data-derived', 'applications', 'ocrs_descriptive_primary.csv')
)

# Recalculate the normal-model comparison
benchmark <- app_normal_benchmark(result, 'ocrs')
for (n in c('summary', 'units')) rp_write_csv(
  benchmark[[n]],
  rp_output_path('data-derived', 'applications', paste0('ocrs_normal_re_', n, '.csv'))
)

# Record source identity, sample counts and analysis scope
rp_write_json(
  list(
    source_classification = 'public-use', deposit = contract$deposit,
    source_sha256 = actual_hash,
    source_observations = nrow(raw),
    complete_case_observations = nrow(oc),
    schools = nrow(result),
    interpretation = 'Descriptive complete-case school OLS contrasts; not the published blocked/covariate-adjusted ITT estimator',
    participant_rows_exported = FALSE, posterior_refit_performed = FALSE
  ),
  rp_output_path('outputs', 'applications', 'ocrs_public_input_receipt.json')
)
cat('OCRS public-use: 1737 source rows -> 1063 complete-case rows -> 24 schools; input parity passed.\n')
