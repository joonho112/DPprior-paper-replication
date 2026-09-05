# Author: JoonHo Lee (jlee296@ua.edu)
# Generate one synthetic study
#
# Generate latent effects and noisy unit estimates from a declared design and
# seed. Exact cluster counts preserve the intended allocation shares; balanced
# dominant locations distribute that role across the replicate sequence.
#
# Sourced by refit/simulation/prepare.R and verification/check_dgp.R.
# Returns synthetic data and generating quantities; no posterior fitting.

# Evaluate a calculation with the specified RNG state, then restore the
# caller's previous random state.
simv2_with_seed <- function(seed, code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_kind <- RNGkind()
  on.exit(
    {
      do.call(RNGkind, as.list(old_kind))
      if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
      else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        rm(".Random.seed", envir = .GlobalEnv)
      }
    },
    add = TRUE
  )
  set.seed(as.integer(seed),
    kind = "Mersenne-Twister", normal.kind = "Inversion",
    sample.kind = "Rejection"
  )
  force(code)
}

# Choose the dominant cluster position using the fixed balance scheme across
# replicates.
simv2_balanced_dominant_location <- function(K_true, replicate, dgp_sha256) {
  K_true <- as.integer(K_true)
  replicate <- as.integer(replicate)
  if (K_true < 2L || replicate < 1L) stop("Invalid K_true/replicate", call. = FALSE)
  block <- (replicate - 1L) %/% K_true
  within <- (replicate - 1L) %% K_true + 1L
  # Replicate itself must not perturb the within-block permutation: each block
  # of K replicates uses every ordered component exactly once.  The argument is
  # retained in the API to make the DGP provenance dependency explicit.
  balance_digest <- simv2_sha256(paste("dominant-location-balance-v1", K_true, block,
    sep = "\n"
  ))
  scores <- vapply(seq_len(K_true), function(component) {
    simv2_sha256(paste(balance_digest, component, sep = "\n"))
  }, character(1))
  permutation <- order(scores, method = "radix")
  as.integer(permutation[[within]])
}

# Convert the declared balanced or dominant allocation into integer cluster
# counts summing to J.
simv2_exact_cluster_counts <- function(J, K_true, weight_scheme,
                                       dominant_share = NA_real_,
                                       dominant_location_mode = "none",
                                       replicate = 1L, dgp_sha256) {
  J <- as.integer(J)
  K_true <- as.integer(K_true)
  if (J < K_true || K_true < 1L) stop("Require J >= K_true >= 1", call. = FALSE)
  if (identical(weight_scheme, "equal_exact")) {
    base <- J %/% K_true
    remainder <- J %% K_true
    counts <- rep.int(base, K_true)
    if (remainder > 0L) {
      scores <- vapply(seq_len(K_true), function(component) {
        simv2_sha256(paste(dgp_sha256, "equal-count-remainder", component, sep = "\n"))
      }, character(1))
      recipients <- order(scores, method = "radix")[seq_len(remainder)]
      counts[recipients] <- counts[recipients] + 1L
    }
    return(list(
      counts = counts, dominant_component = NA_integer_,
      realized_dominant_share = max(counts) / J
    ))
  }
  if (!identical(weight_scheme, "dominant_exact")) {
    stop("weight_scheme must be equal_exact or dominant_exact", call. = FALSE)
  }
  if (K_true < 2L || !is.finite(dominant_share) || dominant_share <= 0 ||
    dominant_share >= 1) {
    stop("dominant_exact requires K_true >= 2 and 0 < dominant_share < 1",
      call. = FALSE
    )
  }
  desired <- J * dominant_share
  if (abs(desired - round(desired)) > 1e-10) {
    stop("J * dominant_share must be an integer for exact-count generation",
      call. = FALSE
    )
  }
  dominant_count <- as.integer(round(desired))
  if (dominant_count < 1L || J - dominant_count < K_true - 1L) {
    stop("Requested exact dominant count cannot keep every component occupied",
      call. = FALSE
    )
  }
  dominant_component <- switch(dominant_location_mode,
    balanced_random = simv2_balanced_dominant_location(K_true, replicate, dgp_sha256),
    central = as.integer(which.min(abs(seq(-1, 1, length.out = K_true)))),
    stop("dominant_exact requires balanced_random or central location mode",
      call. = FALSE
    )
  )
  other <- setdiff(seq_len(K_true), dominant_component)
  remaining <- J - dominant_count
  counts <- integer(K_true)
  counts[dominant_component] <- dominant_count
  counts[other] <- remaining %/% length(other)
  remainder <- remaining %% length(other)
  if (remainder > 0L) {
    scores <- vapply(other, function(component) {
      simv2_sha256(paste(dgp_sha256, "dominant-other-remainder", component,
        sep = "\n"
      ))
    }, character(1))
    recipients <- other[order(scores, method = "radix")[seq_len(remainder)]]
    counts[recipients] <- counts[recipients] + 1L
  }
  stopifnot(
    sum(counts) == J, all(counts > 0L),
    counts[dominant_component] == dominant_count
  )
  list(
    counts = counts, dominant_component = dominant_component,
    realized_dominant_share = counts[dominant_component] / J
  )
}

# Generate one study with the declared latent-effect scale and measurement
# uncertainty, returning both data and generating metadata.
simv2_generate_dataset <- function(dgp_record, dgp_seed = NULL) {
  identity <- simv2_dgp_identity(dgp_record)
  if (is.null(dgp_seed)) dgp_seed <- identity$seed
  dgp_seed <- as.integer(dgp_seed)
  if (!is.finite(dgp_seed) || dgp_seed < 1L) stop("Invalid manifest DGP seed", call. = FALSE)
  x <- dgp_record
  counts_info <- simv2_exact_cluster_counts(
    J = x$J, K_true = x$K_true, weight_scheme = x$weight_scheme,
    dominant_share = x$dominant_share,
    dominant_location_mode = x$dominant_location_mode,
    replicate = x$replicate, dgp_sha256 = identity$sha256
  )
  means <- seq(-as.numeric(x$cluster_span_d), as.numeric(x$cluster_span_d),
    length.out = as.integer(x$K_true)
  )
  generated <- simv2_with_seed(dgp_seed, {
    z_true <- sample(rep.int(seq_len(as.integer(x$K_true)), counts_info$counts),
      size = as.integer(x$J), replace = FALSE
    )
    tau_raw <- means[z_true] + stats::rnorm(
      as.integer(x$J),
      sd = as.numeric(x$within_sd_raw)
    )
    tau_centered <- tau_raw - mean(tau_raw)
    tau_scale <- stats::sd(tau_centered)
    if (!is.finite(tau_scale) || tau_scale <= 0) {
      stop("Degenerate generated site effects", call. = FALSE)
    }
    tau_true <- as.numeric(x$sigma_tau) * tau_centered / tau_scale
    shape <- 1 / as.numeric(x$n_cv)^2
    n_raw <- stats::rgamma(as.integer(x$J),
      shape = shape,
      rate = shape / as.numeric(x$n_mean)
    )
    n_j <- as.integer(pmax(
      round(pmax(n_raw, as.numeric(x$n_min))),
      as.integer(x$n_min)
    ))
    se2 <- as.numeric(x$kappa) / n_j
    tau_hat <- stats::rnorm(as.integer(x$J), mean = tau_true, sd = sqrt(se2))
    list(
      z_true = z_true, tau_true = tau_true, n_j = n_j, se2 = se2,
      tau_hat = tau_hat
    )
  })
  z_true <- generated$z_true
  tau_true <- generated$tau_true
  n_j <- generated$n_j
  se2 <- generated$se2
  tau_hat <- generated$tau_hat
  I_realized <- as.numeric(x$sigma_tau)^2 /
    (as.numeric(x$sigma_tau)^2 + exp(mean(log(se2))))
  df <- data.frame(
    site = seq_len(as.integer(x$J)), tau_hat = tau_hat, se2 = se2,
    se = sqrt(se2), n_j = n_j, tau_true = tau_true, z_true = z_true,
    stringsAsFactors = FALSE
  )
  meta <- list(
    dgp_key = identity$key, dgp_sha256 = identity$sha256,
    dgp_seed = dgp_seed, J = as.integer(x$J), K_true = as.integer(x$K_true),
    cluster_counts = counts_info$counts,
    dominant_component = counts_info$dominant_component,
    realized_dominant_share = counts_info$realized_dominant_share,
    I_target = as.numeric(x$I_target), I_realized = I_realized
  )
  list(df = df, meta = meta)
}

# Summarize dominant-location frequencies across the declared records to check
# the intended balance.
simv2_audit_dominant_balance <- function(dgp_records) {
  if (!is.data.frame(dgp_records)) stop("dgp_records must be a data frame", call. = FALSE)
  dominant <- dgp_records[
    dgp_records$weight_scheme == "dominant_exact" &
      dgp_records$dominant_location_mode == "balanced_random", ,
    drop = FALSE
  ]
  if (!nrow(dominant)) return(data.frame())
  dominant$location <- vapply(seq_len(nrow(dominant)), function(i) {
    id <- simv2_dgp_identity(as.list(dominant[i, simv2_dgp_fields, drop = FALSE]))
    simv2_balanced_dominant_location(
      dominant$K_true[i],
      dominant$replicate[i],
      id$sha256
    )
  }, integer(1))
  aggregate(replicate ~ suite + design + J + K_true + I_target + location,
    data = dominant, FUN = length
  )
}
