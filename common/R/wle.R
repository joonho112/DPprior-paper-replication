# Author: JoonHo Lee (jlee296@ua.edu)
# Conditional weighted-likelihood scoring
#
# Calculate Warm scores from fixed Rasch item difficulties. All respondents
# with the same raw score share a score estimate and standard error, so the
# reliability calculation needs score frequencies rather than individual
# response rows.
#
# Used by rasch/01_wle.R. Inputs are item difficulties, scores and
# frequencies.
# Returns scores, standard errors and frequency-weighted reliability
# summaries.
# The item estimates are held fixed throughout; their uncertainty is not
# propagated.

# Score one raw total
#
# For a given beta vector, solve the ordinary score equation plus Warm's
# information correction. The correction also gives finite estimates for the
# all-wrong and all-correct patterns.
wle_score <- function(beta, raw_score) {
  if (!is.numeric(beta) || any(!is.finite(beta)) || !length(beta) ||
    length(raw_score) != 1L || !is.finite(raw_score) || raw_score < 0 ||
    raw_score > length(beta) || raw_score != as.integer(raw_score))
    stop('Invalid fixed item difficulties or raw score.', call. = FALSE)

  # p contains item success probabilities at theta. info is test information;
  # third is its derivative, giving the adjustment third / (2 * info).
  equation <- function(theta) {
    p <- plogis(theta - beta)
    info <- sum(p * (1 - p))
    third <- sum(p * (1 - p) * (1 - 2 * p))
    raw_score - sum(p) + third / (2 * info)
  }

  # The bracket and tolerance match the independent scoring implementation
  # used for the paper. Return the equation residual as an accuracy check.
  theta <- uniroot(equation, interval = c(-30, 30), tol = 1e-13)$root
  p <- plogis(theta - beta)
  c(wle = theta, standard_error = 1 / sqrt(sum(p * (1 - p))), residual = equation(theta))
}

# Combine score groups
#
# Use frequency weights to recover the sample moments. Score variance uses
# N-1; mean squared standard error uses N. Reliability is one minus their
# ratio.
wle_reliability <- function(theta, se, n) {
  stopifnot(
    length(theta) == length(se),
    length(se) == length(n),
    all(is.finite(c(theta, se, n))),
    all(se > 0), all(n >= 0), all(n == as.integer(n)), sum(n) > 1
  )
  N <- sum(n)
  average <- sum(n * theta) / N
  variance <- sum(n * (theta - average)^2) / (N - 1) # unbiased sample variance
  error <- sum(n * se^2) / N
  c(
    wle_reliability = 1 - error / variance,
    average_error_variance = error,
    wle_mean = average,
    wle_variance = variance
  )
}
