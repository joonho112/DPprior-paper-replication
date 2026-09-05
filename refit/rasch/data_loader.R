# Author: JoonHo Lee (jlee296@ua.edu)
# Recover the vocabulary analysis sample
#
# Read the separately acquired IRW long table, preserve character-ID ordering
# and draw the fixed 500-person sample before removing the three nonwords.
# Sorting IDs as integers or removing items before sampling would change the
# analysis input.
#
# Called by the Rasch refit and combination entrypoints.
# Returns the response matrix and source/content fingerprints; never exports
# raw responses.

rasch_source_matrix <- function(path) {
  # Validate the long source table before reshaping it.
  raw <- readRDS(path)
  if (!is.data.frame(raw) || !setequal(names(raw), c('id', 'item', 'resp')) ||
    nrow(raw) != 39920L ||
    anyNA(raw))
    stop('Expected complete IRW long data with id, item, resp and 39920 rows.',
      call. = FALSE
    )
  raw$id <- as.character(raw$id)
  raw$item <- as.character(raw$item)
  if (anyDuplicated(raw[c('id', 'item')]) || !all(raw$resp %in% c(0L, 1L)) ||
    !setequal(unique(raw$item), paste0('VCL', 1:16)) || any(table(raw$id) != 16L))
    stop('IRW item/response identity differs.', call. = FALSE)

  # Sample in the historical character-ID ordering. Convert selected IDs to
  # integers only after the sample positions have been chosen.
  ids <- sort(unique(raw$id))
  stopifnot(length(ids) == 2495L, setequal(as.integer(ids), 1:2495))
  set.seed(81770971L,
    kind = 'Mersenne-Twister',
    normal.kind = 'Inversion',
    sample.kind = 'Rejection'
  )
  pos <- sort(sample.int(2495L, 500L))
  keep <- sort(as.integer(ids[pos]))
  x <- raw[as.integer(raw$id) %in% keep, ]

  # Construct all 16 response columns first; then remove the three nonwords.
  Y16 <- matrix(NA_integer_, 500L, 16L, dimnames = list(as.character(keep), paste0('VCL', 1:16)))
  Y16[cbind(match(as.integer(x$id), keep), match(x$item, colnames(Y16)))] <- as.integer(x$resp)
  Y <- Y16[, setdiff(colnames(Y16), c('VCL6', 'VCL9', 'VCL12')), drop = FALSE]
  stopifnot(!anyNA(Y), identical(dim(Y), c(500L, 13L)), all(Y %in% 0:1))

  # Check canonical matrix content as well as the raw-score frequency table.
  jobs <- rp_read_csv(rp_path('data-frozen', 'rasch', 'chain_jobs.csv', must_exist = TRUE))
  expected <- unique(jobs$data_object_sha256)
  actual <- digest::digest(Y, algo = 'sha256')
  if (length(expected) != 1L || !identical(actual, expected))
    stop('Canonical prepared response-matrix fingerprint differs from the paper.',
      call. = FALSE
    )
  counts <- rp_read_csv(rp_path('data-frozen', 'rasch', 'score_counts.csv', must_exist = TRUE))
  stopifnot(identical(tabulate(rowSums(Y) + 1L, nbins = 14L), counts$n))
  # Byte identity can differ after harmless RDS serialization. Canonical matrix
  # identity, item ordering and sample frequencies must still match exactly.
  list(
    Y = Y, source_sha256 = rp_sha256(path), analysis_object_sha256 = actual,
    raw_scores = rowSums(Y), source_N = 2495L, analysis_N = 500L
  )
}
