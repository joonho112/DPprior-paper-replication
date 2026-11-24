#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Verify current exhibit IDs, complete output sets, numerical frames and source receipts.
arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', arg[1]), fixed = TRUE)))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))

contract <- rp_read_csv(rp_path('provenance', 'artifact-contract.csv', must_exist = TRUE),
                        key = 'artifact_id')
if (nrow(contract) != 62L || sum(contract$kind == 'table') != 43L ||
    sum(contract$kind == 'figure') != 19L ||
    any(contract$artifact_id != paste(contract$document, contract$kind,
                                     contract$printed_number, sep = '-')))
  stop('Expected 43 tables and 19 figures with canonical manuscript exhibit IDs.', call. = FALSE)

same_names <- function(found, required, context) {
  if (anyNA(found) || anyNA(required) || anyDuplicated(found) || anyDuplicated(required) ||
      !setequal(found, required))
    stop(context, ': missing [', paste(setdiff(required, found), collapse = ', '),
         '], extra [', paste(setdiff(found, required), collapse = ', '),
         '] or duplicate paths.', call. = FALSE)
}
output_files <- function(id, kind) {
  ext <- if (kind == 'table') c('html', 'tex') else c('pdf', 'png')
  paste(id, ext, sep = '.')
}
artifact_files <- unlist(Map(output_files, contract$artifact_id, contract$kind), use.names = FALSE)
frame_files <- paste0(contract$artifact_id, '.csv')
out <- rp_path('outputs', 'v104', must_exist = TRUE)
receipts <- rp_path('outputs', 'exhibit-receipts', must_exist = TRUE)
files <- function(path) list.files(path, all.files = TRUE, no.. = TRUE)
same_names(files(out), c(artifact_files, frame_files, 'G2-example.json'), '104 output inventory')
same_names(files(receipts), paste0(contract$artifact_id, '.json'), '104 receipt inventory')

check_hash <- function(relative, sha, id) {
  parts <- strsplit(relative, '/', fixed = TRUE)[[1]]
  path <- do.call(rp_path, c(as.list(parts), list(must_exist = TRUE)))
  if (length(sha) != 1L || !grepl('^[a-f0-9]{64}$', sha) || !identical(rp_sha256(path), sha))
    stop(id, ': SHA-256 mismatch for ', relative, call. = FALSE)
}
records <- lapply(seq_len(nrow(contract)), function(i) {
  row <- contract[i, ]
  id <- row$artifact_id
  receipt <- jsonlite::read_json(file.path(receipts, paste0(id, '.json')), simplifyVector = TRUE)
  if (!identical(receipt$artifact_id, id) || !identical(receipt$tier, row$reproduction_tier))
    stop(id, ': receipt ID or reproduction tier differs from the exhibit contract.', call. = FALSE)
  sources <- strsplit(row$direct_inputs, ';', fixed = TRUE)[[1]]
  same_names(receipt$inputs$path, sources, paste(id, 'direct inputs'))
  same_names(receipt$outputs$path, file.path('outputs', 'v104', output_files(id, row$kind)),
             paste(id, 'rendered outputs'))
  for (group in list(receipt$inputs, receipt$outputs))
    for (j in seq_len(nrow(group))) check_hash(group$path[j], group$sha256[j], id)
  numeric_path <- file.path('outputs', 'v104', paste0(id, '.csv'))
  check_hash(numeric_path, receipt$numeric_sha256, id)
  frame <- rp_read_csv(file.path(out, paste0(id, '.csv')))
  if (!length(receipt$rows) || nrow(frame) == 0L || nrow(frame) != receipt$rows ||
      !identical(names(frame), as.character(receipt$columns)))
    stop(id, ': numeric frame row or column identity mismatch.', call. = FALSE)
  data.frame(artifact_id = id, rows = nrow(frame), tier = receipt$tier, input_files = length(sources),
             numeric_sha256 = receipt$numeric_sha256, passed = TRUE)
})
rp_write_csv(do.call(rbind, records), rp_output_path('outputs', 'verification', 'exhibit_checks.csv'))
cat('Verified all 62 exhibit receipts and 186 exhibit files, including numeric frame bytes.\n')
