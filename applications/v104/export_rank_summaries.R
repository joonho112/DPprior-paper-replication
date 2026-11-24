# Author: JoonHo Lee (jlee296@ua.edu)
# Optional extraction from locally authorized, retained joint draws.
# Usage: Rscript applications/v104/export_rank_summaries.R NORMAL.csv RASCH.csv OUTDIR
# See README.md for input schemas. No posterior draws or individual Rasch rows are written.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
normal <- read.csv(args[1]); rasch <- read.csv(args[2])
out <- args[3]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
apps <- c(star = 79L, bd = 48L, rasch = 500L)
normal_ids <- c(gamma = "Vague", tsmm = "TSMM", dual = "Dual-Soft-Selected")
rasch_ids <- c(gamma = "vague", tsmm = "tsmm", dual = "dasoft")
halves <- list(chains_12 = 1:8000, chains_34 = 8001:16000)
rank_parts <- unit_parts <- curve_parts <- list()
for (app in names(apps)) {
  n <- unname(apps[app])
  signatures <- list()
  ordered_ids <- NULL
  for (prior in names(normal_ids)) {
    if (app == "rasch") {
      r <- rasch[rasch$arm == "main" & rasch$prior_role == rasch_ids[prior] & rasch$v5_reportable, ]
      stopifnot(nrow(r) == 1L, r$gate_pass, unname(tools::sha256sum(r$combined_path)) == r$combined_sha256_actual)
      f <- readRDS(r$combined_path)
      stopifnot(f$fit_key == r$fit_key, identical(dim(f$draws$theta), c(4000L, 4L, n)),
                identical(f$chains$chain, 1:4), all(f$chains$n_draws == 4000L))
      if (is.null(ordered_ids)) ordered_ids <- f$persons$person_id
      stopifnot(identical(ordered_ids, f$persons$person_id), !anyDuplicated(ordered_ids))
      groups <- f$persons$raw_score
      stopifnot(identical(sort(unique(as.integer(groups))), 0:13))
      theta <- matrix(f$draws$theta, nrow = 16000L, ncol = n)
    } else {
      r <- normal[normal$app == app & normal$prior_label == normal_ids[prior], ]
      stopifnot(nrow(r) == 1L, r$analysis_spec == "primary_scale_aligned", r$input_unit_order_match,
                r$chain_row_blocks == "1:4000;4001:8000;8001:12000;12001:16000",
                unname(tools::sha256sum(r$artifact_path)) == r$artifact_sha256_actual)
      f <- readRDS(r$artifact_path)
      stopifnot(f$execution_fit_key == r$execution_fit_key, f$fit$diagnostic_pass)
      theta <- as.matrix(f$fit$retained$tau_j)
      stopifnot(identical(colnames(theta), sprintf("tau_j[%d]", seq_len(n))))
      groups <- seq_len(n)
    }
    stopifnot(identical(dim(theta), c(16000L, n)), all(is.finite(theta)))
    rm(f)
    for (block in names(halves)) {
      d <- theta[halves[[block]], , drop = FALSE]
      average <- tapply(colMeans(d), groups, mean)
      unit_parts[[length(unit_parts)+1L]] <- data.frame(app, prior, block,
        unit_or_score = as.integer(names(average)), frequency = as.integer(table(groups)), mean = as.numeric(average))
      signatures[[paste(prior, block)]] <- sort(as.numeric(d), method = "radix")
    }
    signatures[[paste(prior, "full")]] <- sort(as.numeric(theta), method = "radix")
    for (block in c("full", names(halves))) {
      if (prior == "tsmm") next
      ordered <- signatures[[paste(prior, block)]]
      p <- seq_len(199L)/200
      curve_parts[[length(curve_parts)+1L]] <- data.frame(app, prior, block, p, quantile = ordered[round(p*length(ordered))])
    }
    rm(theta, d); invisible(gc())
  }
  accumulate <- function(first, block_a, second, block_b) {
    a <- signatures[[paste(first, block_a)]]; b <- signatures[[paste(second, block_b)]]
    stopifnot(length(a) == length(b), length(a) %% 100L == 0L)
    # 100 contiguous equally populated percentile intervals partition every joint marginal rank.
    size <- length(a) %/% 100L
    squared <- (b - a)^2
    data.frame(app, prior_a = first, block_a, prior_b = second, block_b, bin = 1:100,
               n_rank = size, sum_sq = colSums(matrix(squared, nrow = size)))
  }
  for (pair in combn(names(normal_ids), 2L, simplify = FALSE)) {
    for (block in c("full", names(halves))) {
      rank_parts[[length(rank_parts)+1L]] <- accumulate(pair[1], block, pair[2], block)
    }
  }
  for (prior in names(normal_ids))
    rank_parts[[length(rank_parts)+1L]] <- accumulate(prior, "chains_12", prior, "chains_34")
  rm(signatures); invisible(gc())
}
write.csv(do.call(rbind, rank_parts), file.path(out, "rank-bin-sums.csv"), row.names=FALSE)
write.csv(do.call(rbind, curve_parts), file.path(out, "percentile-values.csv"), row.names=FALSE)
write.csv(do.call(rbind, unit_parts), file.path(out, "chain-mean-by-unit-or-score.csv"), row.names=FALSE)
