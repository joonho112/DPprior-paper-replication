# Author: JoonHo Lee (jlee296@ua.edu)
# Read the frozen Rasch aggregates
#
# Check export fingerprints before loading score, item or model summaries. The
# public files do not contain response rows or individual posterior draws.
#
# Sourced by the three Rasch reconstruction scripts.
# Reads: data-frozen/rasch/ and its provenance record.

rasch_check_exports <- function() {
  registry <- jsonlite::read_json(rp_path('provenance', 'rasch-export.json', must_exist = TRUE),
    simplifyVector = TRUE
  )$exports
  for (i in seq_len(nrow(registry))) {
    x <- registry[i, ]
    if (!identical(rp_sha256(file.path(.replication_root, x$export)), x$export_sha256))
      stop('Rasch frozen input changed: ', x$export, call. = FALSE)
  }
  invisible(TRUE)
}
rasch_read <- function(name, key = character()) rp_read_csv(
  rp_path('data-frozen', 'rasch', paste0(name, '.csv'), must_exist = TRUE),
  key = key
)
