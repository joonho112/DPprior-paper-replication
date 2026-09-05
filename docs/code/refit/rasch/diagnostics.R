# Author: JoonHo Lee (jlee296@ua.edu)
# Summarize Rasch convergence diagnostics
#
# Treat columns as chains when checking one variable, or use an iteration-by-
# chain-by-variable array. The gated flag distinguishes quantities used for
# acceptance from descriptive diagnostics.
#
# Called by refit/rasch/combine_helpers.R.
# Returns R-hat, bulk/tail ESS and threshold decisions; no fitting.

RHAT_MAX <- 1.01
ESS_BULK_MIN <- 400
ESS_TAIL_MIN <- 400

# Check one variable across chains; handle a constant draw matrix explicitly
# before calling the standard diagnostics.
diag_matrix <- function(m, variable, group, gated) {
  m <- as.matrix(m)
  values <- as.numeric(m)
  constant <- diff(range(values)) == 0
  if (constant) {
    out <- data.frame(
      group = group,
      variable = variable,
      mean = values[[1L]],
      sd = 0,
      rhat = 1,
      ess_bulk = length(values),
      ess_tail = length(values),
      mcse_mean = 0,
      mcse_sd = 0,
      constant = TRUE, stringsAsFactors = FALSE
    )
  } else {
    tail_ess <- posterior::ess_tail(m)
    if (!is.finite(tail_ess)) {
      limits <- stats::quantile(values, c(.05, .95), names = FALSE, type = 1)
      lower <- matrix(as.numeric(m <= limits[[1L]]), nrow = nrow(m))
      upper <- matrix(as.numeric(m >= limits[[2L]]), nrow = nrow(m))
      tail_ess <- min(posterior::ess_bulk(lower), posterior::ess_bulk(upper))
    }
    out <- data.frame(
      group = group,
      variable = variable,
      mean = mean(values),
      sd = stats::sd(values),
      rhat = posterior::rhat(m),
      ess_bulk = posterior::ess_bulk(m),
      ess_tail = tail_ess,
      mcse_mean = posterior::mcse_mean(m),
      mcse_sd = posterior::mcse_sd(m),
      constant = FALSE, stringsAsFactors = FALSE
    )
  }
  out$gated <- gated
  out$rhat_pass <- is.finite(out$rhat) & out$rhat <= RHAT_MAX
  out$ess_bulk_pass <- is.finite(out$ess_bulk) & out$ess_bulk >= ESS_BULK_MIN
  out$ess_tail_pass <- is.finite(out$ess_tail) & out$ess_tail >= ESS_TAIL_MIN
  out$gate_pass <- out$rhat_pass & out$ess_bulk_pass & out$ess_tail_pass
  out
}
# arr: iteration x chain x variable array (a posterior draws_array is accepted)

# Apply the single-variable checker along the third dimension of an
# iteration-by-chain-by-variable array.
diag_array <- function(arr, group, gated, variables = dimnames(arr)[[3]]) {
  arr <- unclass(arr)
  rows <- lapply(
    seq_along(variables),
    function(v) diag_matrix(arr[, , v, drop = TRUE], variables[[v]], group, gated)
  )
  do.call(rbind, rows)
}
