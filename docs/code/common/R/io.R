# Author: JoonHo Lee (jlee296@ua.edu)
# Read checked inputs and protect the frozen results
#
# Keep file validation in one place. CSV readers check required fields and
# keys; writers accept only generated-output locations. This prevents an
# ordinary rebuild from replacing the original reference values.
#
# Sourced by common/R/bootstrap.R. Outputs belong in data-derived/ or
# outputs/.

# Read a CSV without renaming its columns, then check the declared fields and
# uniqueness of any supplied key.
rp_read_csv <- function(path, required = character(), key = character()) {
  if (!file.exists(path)) stop("Input file does not exist: ", basename(path), call. = FALSE)
  x <- utils::read.csv(path,
    stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("NA", "")
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) stop("Missing required columns: ",
    paste(missing, collapse = ", "),
    call. = FALSE
  )
  if (anyDuplicated(names(x))) stop("Duplicate column names are not permitted.", call. = FALSE)
  if (length(key)) {
    if (!all(key %in% names(x))) stop("Key columns are missing.", call. = FALSE)
    if (anyNA(x[key]) || anyDuplicated(x[key])) stop("Input key is missing or duplicated.", call. = FALSE)
  }
  x
}

# Reject frozen, outside-project and otherwise unsupported destinations before
# a writer opens the file.
rp_check_output <- function(path) {
  roots <- vapply(c("outputs", "data-derived"), rp_path, character(1))
  # Revalidate the path against the root, including any pre-existing symlinks.
  rel <- substring(path, nchar(.replication_root) + 2L)
  parts <- strsplit(rel, "/", fixed = TRUE)[[1L]]
  checked <- do.call(rp_path, as.list(parts))
  if (!identical(path, checked) ||
    !any(vapply(roots, function(x) startsWith(path, paste0(x, "/")), logical(1))))
    stop("Refusing to overwrite a frozen input or a file outside output areas.",
      call. = FALSE
    )
  invisible(path)
}

# Create the generated-output directory if needed and save a CSV with a
# consistent missing-value convention.
rp_write_csv <- function(x, path) {
  rp_check_output(path)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = ".replication-",
    tmpdir = dirname(path),
    fileext = ".csv"
  )
  on.exit(unlink(temporary), add = TRUE)
  utils::write.csv(x, temporary, row.names = FALSE, na = "NA")
  if (!file.rename(temporary, path)) stop("Could not install completed output: ",
    basename(path),
    call. = FALSE
  )
  invisible(path)
}

# Write a readable machine record with full numerical precision and an
# explicit representation for missing values.
rp_write_json <- function(x, path) {
  rp_check_output(path)
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("JSON output requires jsonlite; run setup for the requested profile.",
    call. = FALSE
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(
    pattern = ".replication-",
    tmpdir = dirname(path),
    fileext = ".json"
  )
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(x,
    temporary,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null",
    digits = NA
  )
  if (!file.rename(temporary, path)) stop("Could not install completed JSON output.", call. = FALSE)
  invisible(path)
}

# Fingerprint the file bytes. This identifies a particular file, not the
# equivalence of two serialized numerical objects.
rp_sha256 <- function(path) {
  if (!requireNamespace("digest", quietly = TRUE)) stop("SHA-256 verification requires digest.", call. = FALSE)
  digest::digest(file = path, algo = "sha256")
}
