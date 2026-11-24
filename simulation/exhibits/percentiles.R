# Author: JoonHo Lee (jlee296@ua.edu)
# Aggregate empirical finite-site quantiles on matched, jointly converged replicates.
paired_percentiles <- function(fits, vectors) {
  design <- c("K_true", "I_target")
  keys <- c(design, "replicate", "dgp_sha256")
  design_rep <- c(design, "replicate")
  gamma <- fits[fits$prior == "Vague", c(keys, "fit_key")]
  dual <- fits[fits$prior == "Dual-Anchor", c(keys, "fit_key")]
  pairs <- merge(gamma, dual, by = keys, suffixes = c("_gamma", "_dual"))
  stopifnot(!anyDuplicated(pairs[design_rep]),
            all(pairs$fit_key_gamma != pairs$fit_key_dual))
  design_counts <- function(x, count_name) {
    z <- aggregate(rep(1L, nrow(x)), x[design], sum)
    names(z)[3] <- count_name
    z
  }
  support <- Reduce(function(a, b) merge(a, b, by = design),
                    list(design_counts(gamma, "n_gamma"), design_counts(dual, "n_dual"),
                         design_counts(pairs, "n_paired")))
  support$n_attempted <- 200L
  support <- support[order(support$K_true, support$I_target), ]
  # A finite CDF with J=100 equally weighted sites jumps at each order statistic.
  # For these seven integer percentages, Q(p/100) is exactly the p-th sorted value.
  percentiles <- c(1L, 5L, 25L, 50L, 75L, 95L, 99L)
  site_quantiles <- function(x) t(apply(x, 1L, function(v) sort(v)[percentiles]))
  truth <- site_quantiles(vectors$truth[pairs$dgp_sha256, , drop = FALSE])
  q_gamma <- site_quantiles(vectors$CB[pairs$fit_key_gamma, , drop = FALSE])
  q_dual <- site_quantiles(vectors$CB[pairs$fit_key_dual, , drop = FALSE])
  replicates <- pairs[rep(seq_len(nrow(pairs)), each = length(percentiles)), keys]
  replicates$percentile <- rep(percentiles, times = nrow(pairs))
  replicates$truth <- as.vector(t(truth))
  replicates$gamma <- as.vector(t(q_gamma))
  replicates$dual <- as.vector(t(q_dual))
  replicates$error_gamma <- replicates$gamma - replicates$truth
  replicates$error_dual <- replicates$dual - replicates$truth
  replicates$shift <- replicates$dual - replicates$gamma
  replicates$abs_shift <- abs(replicates$shift)
  stopifnot(!anyDuplicated(replicates[c(keys, "percentile")]),
            max(abs(replicates$error_dual - replicates$error_gamma - replicates$shift)) < 1e-14)

  mcse <- function(x) sd(x) / sqrt(length(x))
  groups <- c(design, "percentile")
  summarize_cells <- function(data, summarize) {
    cells <- split(data, data[groups], drop = TRUE)
    rows <- lapply(cells, function(x) cbind(x[1L, groups], n = nrow(x), summarize(x)))
    z <- do.call(rbind, unname(rows))
    z[do.call(order, z[groups]), , drop = FALSE]
  }
  results <- summarize_cells(replicates, function(x) {
    data.frame(mean_truth = mean(x$truth), mean_gamma = mean(x$gamma), mean_dual = mean(x$dual),
               bias_gamma = mean(x$error_gamma), bias_dual = mean(x$error_dual),
               mae_gamma = mean(abs(x$error_gamma)), mae_dual = mean(abs(x$error_dual)),
               rmse_gamma = sqrt(mean(x$error_gamma^2)), rmse_dual = sqrt(mean(x$error_dual^2)),
               mean_shift = mean(x$shift), mcse_shift = mcse(x$shift),
               mean_abs_shift = mean(x$abs_shift), mcse_abs_shift = mcse(x$abs_shift),
               rms_shift = sqrt(mean(x$shift^2)),
               mcse_bias_gamma = mcse(x$error_gamma), mcse_bias_dual = mcse(x$error_dual),
               mcse_mae_gamma = mcse(abs(x$error_gamma)), mcse_mae_dual = mcse(abs(x$error_dual)))
  })

  # Upper-minus-median differences are formed within a replicate, before averaging.
  median_rows <- replicates[replicates$percentile == 50L, c(keys, "abs_shift")]
  upper <- merge(replicates[replicates$percentile > 50L, ], median_rows,
                 by = keys, suffixes = c("", "_median"))
  upper_vs_median <- summarize_cells(upper, function(x) {
    diff <- x$abs_shift - x$abs_shift_median
    avg <- mean(diff)
    se <- mcse(diff)
    margin <- qt(0.975, nrow(x) - 1L) * se
    data.frame(mean_abs_shift = mean(x$abs_shift), median_mean_abs_shift = mean(x$abs_shift_median),
               mean_abs_difference = avg, mcse_abs_difference = se,
               ci_lower = avg - margin, ci_upper = avg + margin,
               fraction_greater = mean(diff > 0), fraction_equal = mean(diff == 0),
               fraction_less = mean(diff < 0))
  })
  stopifnot(nrow(results) == 63L, nrow(upper_vs_median) == 27L,
            nrow(upper) == nrow(pairs) * 3L,
            all(results$rmse_gamma >= results$mae_gamma),
            all(results$rmse_dual >= results$mae_dual),
            all(results$mean_abs_shift + 1e-15 >= abs(results$mean_shift)))

  list(support = support, percentiles = results, replicates = replicates, upper_vs_median = upper_vs_median)
}
