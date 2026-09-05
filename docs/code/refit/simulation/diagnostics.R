# Author: JoonHo Lee (jlee296@ua.edu)
# Check the simulation chains
#
# Compute modern R-hat and effective sample sizes on aligned chain arrays.
# Keep convergence decisions separate from model execution: a completed
# sampler may still fail the scientific acceptance criteria.
#
# Used by synthetic and application refit drivers.
# Returns diagnostic rows and a disposition under the recorded thresholds.

simv2_default_monitors <- function() c(
  "alpha", "K", "grand_mean", "base_variance", "base_shape", "base_rate",
  "active_atom_mean", "active_atom_sd", "active_atom_variance_mean",
  "outcome_mean", "outcome_sd"
)

# Check compatible chain dimensions and arrange monitored draws as iteration
# by chain by variable.
simv2_draws_array <- function(chains, variables = simv2_default_monitors()) {
  simv2_require_namespace("posterior")
  if (!is.list(chains) || length(chains) != 4L) {
    stop("Exactly four chains are required", call. = FALSE)
  }
  chains <- lapply(chains, as.data.frame)
  iterations <- vapply(chains, nrow, integer(1))
  if (length(unique(iterations)) != 1L || iterations[[1L]] < 2L) {
    stop("Four chains must have the same number of at least two post-warmup draws",
      call. = FALSE
    )
  }
  missing <- lapply(chains, function(x) setdiff(variables, names(x)))
  if (any(lengths(missing))) {
    stop(sprintf(
      "Missing monitored variables: %s",
      paste(unique(unlist(missing)), collapse = ", ")
    ), call. = FALSE)
  }
  a <- array(NA_real_,
    dim = c(iterations[[1L]], 4L, length(variables)),
    dimnames = list(
      iteration = NULL, chain = paste0("chain", 1:4),
      variable = variables
    )
  )
  for (chain in seq_len(4L)) {
    a[, chain, ] <- as.matrix(chains[[chain]][, variables, drop = FALSE])
  }
  if (any(!is.finite(a))) stop("Diagnostics received non-finite draws", call. = FALSE)
  posterior::as_draws_array(a)
}

# Calculate R-hat and bulk/tail ESS using posterior, then apply the recorded
# diagnostic thresholds.
simv2_modern_diagnostics <- function(chains,
                                     variables = simv2_default_monitors(),
                                     rhat_max = 1.01,
                                     ess_bulk_min = 400,
                                     ess_tail_min = 400,
                                     constant_tolerance = 0) {
  draws <- simv2_draws_array(chains, variables)
  summaries <- lapply(variables, function(variable) {
    values <- as.numeric(draws[, , variable])
    constant <- diff(range(values)) <= constant_tolerance
    if (constant) {
      data.frame(
        variable = variable, mean = values[[1L]], sd = 0,
        rhat = 1, ess_bulk = length(values), ess_tail = length(values),
        mcse_mean = 0, mcse_sd = 0, constant = TRUE,
        stringsAsFactors = FALSE
      )
    } else {
      one <- as.array(draws)[, , variable, drop = TRUE]
      tail_ess <- posterior::ess_tail(one)
      if (!is.finite(tail_ess)) {
        # posterior::ess_tail can be undefined for discrete draws with tied
        # empirical tail quantiles (notably K).  Its defining lower/upper-tail
        # indicator ESS remains estimable, so use the smaller of those two.
        limits <- stats::quantile(as.numeric(one), c(.05, .95),
          names = FALSE,
          type = 1
        )
        lower <- matrix(as.numeric(one <= limits[[1L]]), nrow = nrow(one))
        upper <- matrix(as.numeric(one >= limits[[2L]]), nrow = nrow(one))
        tail_ess <- min(posterior::ess_bulk(lower), posterior::ess_bulk(upper))
      }
      data.frame(
        variable = variable, mean = mean(one), sd = stats::sd(as.numeric(one)),
        rhat = posterior::rhat(one), ess_bulk = posterior::ess_bulk(one),
        ess_tail = tail_ess,
        mcse_mean = posterior::mcse_mean(one), mcse_sd = posterior::mcse_sd(one),
        constant = FALSE, stringsAsFactors = FALSE
      )
    }
  })
  summary <- do.call(rbind, summaries)
  summary$rhat_pass <- is.finite(summary$rhat) & summary$rhat <= rhat_max
  summary$ess_bulk_pass <- is.finite(summary$ess_bulk) & summary$ess_bulk >= ess_bulk_min
  summary$ess_tail_pass <- is.finite(summary$ess_tail) & summary$ess_tail >= ess_tail_min
  summary$gate_pass <- summary$rhat_pass & summary$ess_bulk_pass & summary$ess_tail_pass
  k_mixing <- do.call(rbind, lapply(seq_along(chains), function(chain) {
    K <- as.integer(chains[[chain]]$K)
    data.frame(
      chain = chain, draws = length(K), transitions = sum(diff(K) != 0L),
      transition_rate = if (length(K) > 1L) mean(diff(K) != 0L) else NA_real_,
      unique_states = length(unique(K)), min_K = min(K), max_K = max(K),
      stringsAsFactors = FALSE
    )
  }))
  failed <- summary$variable[!summary$gate_pass]
  list(
    schema_version = "simulation-diagnostics/1",
    pass = !length(failed), summary = summary, K_mixing = k_mixing,
    failed_variables = failed,
    thresholds = list(
      rhat_max = rhat_max, ess_bulk_min = ess_bulk_min,
      ess_tail_min = ess_tail_min
    ),
    constants_policy = paste0(
      "Exactly constant finite draws are recorded explicitly and assigned Rhat=1, ",
      "ESS=total draws, MCSE=0; no near-constant override is used."
    )
  )
}

# Translate diagnostics and attempt number into acceptance, another scheduled
# attempt or final failure.
simv2_diagnostic_disposition <- function(diagnostics, attempt, max_attempts) {
  attempt <- as.integer(attempt)
  max_attempts <- as.integer(max_attempts)
  if (attempt < 1L || max_attempts < 1L || attempt > max_attempts) {
    stop("Invalid diagnostic attempt counters", call. = FALSE)
  }
  if (isTRUE(diagnostics$pass)) {
    return(list(
      code = "DIAGNOSTIC_PASS", final = TRUE, retry = FALSE,
      message = "All predeclared R-hat and ESS gates passed"
    ))
  }
  details <- paste(diagnostics$failed_variables, collapse = ",")
  if (attempt < max_attempts) {
    return(list(
      code = "RETRY_REQUIRED", final = FALSE, retry = TRUE,
      message = sprintf(
        "Modern diagnostic gate failed for %s; run attempt %d",
        details, attempt + 1L
      )
    ))
  }
  list(
    code = "FINAL_DIAGNOSTIC_FAILURE", final = TRUE, retry = FALSE,
    message = sprintf(
      "Modern diagnostic gate failed after %d attempts for %s",
      max_attempts, details
    )
  )
}

# Attach fit and attempt identifiers to the per-variable diagnostic rows.
simv2_diagnostics_flatten <- function(diagnostics, fit_key, attempt) {
  x <- diagnostics$summary
  x$fit_key <- fit_key
  x$attempt <- as.integer(attempt)
  x[, c("fit_key", "attempt", setdiff(names(x), c("fit_key", "attempt")))]
}
