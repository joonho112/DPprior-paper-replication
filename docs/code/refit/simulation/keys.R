# Author: JoonHo Lee (jlee296@ua.edu)
# Define simulation identities and random streams
#
# Use the full SHA-256 digest as the scientific identifier. Integer seeds are
# deterministic projections into R's seed range. This makes generated data and
# streams depend on the declared design rather than row order or execution
# order.
#
# Sourced by the synthetic generator and model adapters.
# Scientific identity rules are retained from the manuscript computation.

simv2_require_namespace <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required", package), call. = FALSE)
  }
}

# Represent one identity field without locale-dependent numeric formatting;
# reject unsupported or nonscalar values.
simv2_scalar_token <- function(x) {
  if (length(x) != 1L) stop("Semantic-key fields must be scalar", call. = FALSE)
  if (is.factor(x)) x <- as.character(x)
  if (is.null(x) || is.na(x)) return("n:")
  if (inherits(x, "Date")) return(paste0("D:", format(x, "%Y-%m-%d")))
  if (inherits(x, "POSIXt")) {
    return(paste0("T:", format(x, "%Y-%m-%dT%H:%M:%OS6Z", tz = "UTC")))
  }
  if (is.logical(x)) return(paste0("l:", if (x) "true" else "false"))
  if (is.integer(x)) return(paste0("i:", format(x, scientific = FALSE)))
  if (is.numeric(x)) {
    if (!is.finite(x)) stop("Non-finite numeric semantic-key field", call. = FALSE)
    return(paste0("d:", sprintf("%.17g", x)))
  }
  if (is.character(x)) {
    # encode reserved separators and non-ASCII bytes without locale dependence
    return(paste0("c:", utils::URLencode(enc2utf8(x), reserved = TRUE)))
  }
  stop(sprintf("Unsupported semantic-key type: %s", paste(class(x), collapse = "/")),
    call. = FALSE
  )
}

# Sort and encode named identity fields after checking required and forbidden
# entries.
simv2_canonical_record <- function(x, required = NULL, forbidden = NULL) {
  if (is.data.frame(x)) {
    if (nrow(x) != 1L) stop("A semantic record must contain exactly one row", call. = FALSE)
    x <- as.list(x[1L, , drop = FALSE])
  }
  if (!is.list(x) || is.null(names(x)) || any(!nzchar(names(x)))) {
    stop("A semantic record must be a fully named list", call. = FALSE)
  }
  if (anyDuplicated(names(x))) stop("Semantic record has duplicate field names", call. = FALSE)
  missing <- setdiff(required %||% character(), names(x))
  if (length(missing)) stop(sprintf("Missing semantic fields: %s", paste(missing, collapse = ", ")),
    call. = FALSE
  )
  present_forbidden <- intersect(forbidden %||% character(), names(x))
  if (length(present_forbidden)) {
    stop(sprintf(
      "Forbidden semantic fields present: %s",
      paste(present_forbidden, collapse = ", ")
    ), call. = FALSE)
  }
  nms <- sort(names(x), method = "radix")
  paste(vapply(nms, function(nm) {
    paste0(
      utils::URLencode(enc2utf8(nm), reserved = TRUE),
      "=",
      simv2_scalar_token(x[[nm]])
    )
  }, character(1)), collapse = "|")
}
`%||%` <- function(a, b) if (is.null(a)) b else a
simv2_sha256 <- function(text) {
  simv2_require_namespace("digest")
  digest::digest(enc2utf8(text), algo = "sha256", serialize = FALSE)
}

# Hash a canonical record within its own namespace so different kinds of
# scientific objects cannot share an identity accidentally.
simv2_semantic_id <- function(namespace, record) {
  canonical <- simv2_canonical_record(record)
  digest <- simv2_sha256(paste(namespace, canonical, sep = "\n"))
  list(
    namespace = namespace, canonical = canonical, sha256 = digest,
    id = paste0(namespace, "_", digest)
  )
}
simv2_dgp_fields <- c(
  "schema_version", "study_id", "J", "K_true",
  "weight_scheme", "dominant_share", "dominant_location_mode", "I_target",
  "n_mean", "replicate", "sigma_tau", "within_sd_raw", "cluster_span_d",
  "n_cv", "n_min", "kappa", "dgp_version"
)
simv2_fit_fields <- c(
  "schema_version", "dgp_sha256", "conceptual_cell_sha256", "prior_label",
  "prior_calibration_id", "a", "b", "grand_mean_label", "grand_mean_mean",
  "grand_mean_variance", "model_version", "n_chains", "n_iter", "n_warmup",
  "thin", "diagnostic_policy_id", "attempt"
)

# Construct the generating-study identity from design fields only,
# independently of the prior used to fit it.
simv2_dgp_identity <- function(record) {
  forbidden <- c(
    "prior", "prior_label", "prior_calibration_id", "a", "b",
    "grand_mean", "grand_mean_label", "grand_mean_mean",
    "grand_mean_variance", "chain", "chain_id", "attempt"
  )
  unknown <- setdiff(names(record), simv2_dgp_fields)
  if (length(unknown)) {
    stop(sprintf("Unexpected DGP semantic fields: %s", paste(unknown, collapse = ", ")),
      call. = FALSE
    )
  }
  canonical <- simv2_canonical_record(record,
    required = simv2_dgp_fields,
    forbidden = forbidden
  )
  digest <- simv2_sha256(paste("dgp", canonical, sep = "\n"))
  list(
    canonical = canonical, sha256 = digest, key = paste0("dgp_", digest),
    seed = simv2_seed_from_digest(digest, domain = "dgp")
  )
}

# Bind one generated study to the declared model and prior choices.
simv2_fit_identity <- function(record) {
  unknown <- setdiff(names(record), simv2_fit_fields)
  if (length(unknown)) {
    stop(sprintf("Unexpected fit semantic fields: %s", paste(unknown, collapse = ", ")),
      call. = FALSE
    )
  }
  canonical <- simv2_canonical_record(record, required = simv2_fit_fields)
  digest <- simv2_sha256(paste("fit", canonical, sep = "\n"))
  list(canonical = canonical, sha256 = digest, key = paste0("fit_", digest))
}

# Distinguish chain and attempt identities within the same fit.
simv2_chain_identity <- function(fit_sha256, chain_id, attempt) {
  record <- list(
    schema_version = "simulation-chain-key/1",
    fit_sha256 = as.character(fit_sha256),
    chain_id = as.integer(chain_id), attempt = as.integer(attempt)
  )
  canonical <- simv2_canonical_record(record)
  digest <- simv2_sha256(paste("chain", canonical, sep = "\n"))
  list(
    canonical = canonical, sha256 = digest, key = paste0("chain_", digest),
    seed = simv2_seed_from_digest(digest, domain = "chain")
  )
}

# Project a domain-specific digest into a supported positive R seed, keeping
# the full digest as the authoritative identity.
simv2_seed_from_digest <- function(sha256, domain) {
  if (!grepl("^[0-9a-f]{64}$", sha256)) stop("Expected a lowercase SHA-256 digest", call. = FALSE)
  hex <- strsplit(
    substr(simv2_sha256(paste(domain, sha256, sep = "\n")), 1L, 16L),
    ""
  )[[1L]]
  values <- match(hex, c(as.character(0:9), letters[1:6])) - 1L
  modulus <- 2147483646
  accumulator <- 0
  for (value in values) accumulator <- (accumulator * 16 + value) %% modulus
  as.integer(accumulator + 1)
}

# Derive a named secondary stream, for example for population-weight sampling,
# from its parent identity.
simv2_substream_seed <- function(parent_sha256, label) {
  digest <- simv2_sha256(paste("substream", parent_sha256, label, sep = "\n"))
  simv2_seed_from_digest(digest, domain = paste0("substream:", label))
}

# Assign deterministic seeds and check collisions instead of depending on
# execution order.
simv2_assign_unique_seeds <- function(sha256, domain) {
  # The finite 31-bit seed space is smaller than SHA-256, so a large chain
  # manifest can have birthday collisions.  Resolve them deterministically in
  # digest order with domain-separated rehashing.  The mapping is immutable once
  # written in the production manifest.
  sha256 <- as.character(sha256)
  if (any(!grepl("^[0-9a-f]{64}$", sha256)) || anyDuplicated(sha256)) {
    stop("Unique lowercase SHA-256 digests are required for seed assignment",
      call. = FALSE
    )
  }
  order_index <- order(sha256, method = "radix")
  assigned <- integer(length(sha256))
  probes <- integer(length(sha256))
  occupied <- new.env(hash = TRUE, parent = emptyenv())
  for (index in order_index) {
    probe <- 0L
    repeat {
      candidate_digest <- simv2_sha256(paste(domain, sha256[[index]], probe, sep = "\n"))
      candidate <- simv2_seed_from_digest(candidate_digest,
        domain = paste0(domain, ":seed")
      )
      key <- as.character(candidate)
      if (!exists(key, envir = occupied, inherits = FALSE)) {
        assign(key, TRUE, envir = occupied)
        assigned[[index]] <- candidate
        probes[[index]] <- probe
        break
      }
      probe <- probe + 1L
      if (probe > 100000L) stop("Seed collision resolution exhausted", call. = FALSE)
    }
  }
  data.frame(
    sha256 = sha256, seed = assigned, seed_collision_probe = probes,
    stringsAsFactors = FALSE
  )
}

# Check manifest identity and stream relationships before jobs are scheduled.
simv2_audit_identities <- function(manifest) {
  required <- c(
    "dgp_key", "dgp_sha256", "dgp_seed", "fit_key", "fit_sha256",
    "chain_key", "chain_sha256", "chain_seed"
  )
  missing <- setdiff(required, names(manifest))
  if (length(missing)) stop(
    sprintf("Identity audit missing columns: %s", paste(missing, collapse = ", ")),
    call. = FALSE
  )
  dgp <- unique(manifest[c("dgp_key", "dgp_sha256", "dgp_seed")])
  fits <- unique(manifest[c("fit_key", "fit_sha256")])
  chains <- unique(manifest[c("chain_key", "chain_sha256", "chain_seed")])
  checks <- data.frame(
    check = c(
      "dgp_key_collision", "fit_key_collision", "chain_key_collision",
      "dgp_seed_collision", "chain_seed_collision",
      "chain_key_duplicate_rows"
    ),
    failures = c(
      sum(duplicated(dgp$dgp_key)) + sum(duplicated(dgp$dgp_sha256)),
      sum(duplicated(fits$fit_key)) + sum(duplicated(fits$fit_sha256)),
      sum(duplicated(chains$chain_key)) + sum(duplicated(chains$chain_sha256)),
      sum(duplicated(dgp$dgp_seed)), sum(duplicated(chains$chain_seed)),
      sum(duplicated(manifest$chain_key))
    ),
    stringsAsFactors = FALSE
  )
  list(
    ok = all(checks$failures == 0L), checks = checks,
    counts = c(dgp = nrow(dgp), fit = nrow(fits), chain = nrow(chains))
  )
}
