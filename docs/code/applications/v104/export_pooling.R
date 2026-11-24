# Author: JoonHo Lee (jlee296@ua.edu)
# Optional school/study summaries from the authorized retained fit archives.
# Usage: Rscript applications/v104/export_pooling.R NORMAL.csv OUTDIR
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
manifest <- read.csv(args[1]); out <- args[2]
dir.create(out, recursive = TRUE, showWarnings = FALSE)
priors <- c(gamma = "Vague", dual = "Dual-Soft-Selected")
summaries <- errors <- by_count <- halves <- cutoffs <- list()
mcse <- function(x) posterior::mcse_mean(matrix(x, ncol = 4L))
for (app in c("star", "bd")) {
  manifest_rows <- manifest[manifest$app == app & manifest$prior_label %in% priors, ]
  stopifnot(nrow(manifest_rows) == 2L, length(unique(manifest_rows$input_sha256)) == 1L,
    all(manifest_rows$input_unit_order_match), all(manifest_rows$analysis_spec == "primary_scale_aligned"),
    all(manifest_rows$input_sha256 == unname(tools::sha256sum(manifest_rows$input_file[1]))))
  observed <- read.csv(manifest_rows$input_file[1]); y <- observed$tau_hat
  J <- length(y); stopifnot(identical(observed$unit, seq_len(J)), J == if (app == "star") 79L else 48L)
  q <- quantile(y, c(.25, .75, .90), type = 7, names = FALSE)
  middle <- which(y >= q[1] & y <= q[2]); upper <- which(y >= q[3])
  for (prior in names(priors)) {
    r <- manifest_rows[manifest_rows$prior_label == priors[prior], ]
    stopifnot(nrow(r) == 1L, unname(tools::sha256sum(r$artifact_path)) == r$artifact_sha256_actual,
              r$chain_row_blocks == "1:4000;4001:8000;8001:12000;12001:16000")
    f <- readRDS(r$artifact_path)
    z <- as.matrix(f$fit$retained$z); th <- as.matrix(f$fit$retained$tau_j)
    stopifnot(f$execution_fit_key == r$execution_fit_key, f$input_sha256 == r$input_sha256,
              f$fit$diagnostic_pass, identical(dim(z), c(16000L,J)), identical(dim(z),dim(th)),
              identical(colnames(z), sprintf("z[%d]",seq_len(J))), identical(colnames(th),sprintf("tau_j[%d]", seq_len(J))))
    sharing <- vapply(seq_len(J), function(j) {
      peers <- setdiff(middle, j)
      rowMeans(z[,peers,drop=FALSE] == z[,j])
    }, numeric(nrow(z)))
    # Only the estimated chance of sharing a cluster with a distinct middle-group unit leaves the archive.
    mean_effect <- colMeans(th)
    summaries[[length(summaries)+1L]] <- data.frame(app, prior, unit = seq_len(J),
        sharing = colMeans(sharing), mcse_mean = apply(th,2,mcse))
    weight <- function(idx) as.numeric(seq_len(J) %in% idx)/length(idx)
    contrast <- weight(upper) - weight(middle)
    errors[[length(errors)+1L]] <- data.frame(app, prior, coassign = mcse(drop(sharing %*% contrast)),
        distance = mcse(drop(th %*% (contrast * sign(mean_effect-y)))))
    for (h in 1:2) {
      rows <- if (h == 1L) 1:8000 else 8001:16000
      for (group in c("upper","central")) {
        ids <- if (group == "upper") upper else middle
        halves[[length(halves)+1L]] <- data.frame(app,prior,group,half=h,coassign=mean(sharing[rows,ids]),
            displacement=mean(abs(colMeans(th[rows,ids,drop=FALSE])-y[ids])))
      }
    }
    K <- apply(z, 1, function(x) length(unique(x)))
    for (count in 1:4) {
      rows <- K == count
      chain_n <- colSums(matrix(rows,ncol=4L))
      by_count[[length(by_count)+1L]] <- data.frame(app,prior,K=count,retained_draws=sum(rows),
        min_draws_per_chain=min(chain_n),upper_to_central=mean(sharing[rows,upper]),central_to_other_central=mean(sharing[rows,middle]))
    }
    for (fraction in c(.05,.10,.15,.20)) {
      threshold <- unname(quantile(y, 1-fraction, type=7))
      ids <- which(y >= threshold)
      cutoffs[[length(cutoffs)+1L]] <- data.frame(app,prior,upper_fraction=fraction,n_upper=length(ids),
        n_central=length(middle),upper_coassignment=mean(sharing[,ids]),central_coassignment=mean(sharing[,middle]),
        upper_gap=mean(abs(mean_effect[ids]-y[ids])),central_gap=mean(abs(mean_effect[middle]-y[middle])))
    }
  }
}
write_out <- function(rows, filename) write.csv(do.call(rbind,rows),file.path(out,paste0(filename,".csv")),row.names=FALSE)
write_out(summaries,"pooling-unit")
write_out(errors,"pooling-contrast-mcse")
write_out(by_count,"pooling-fixed-count")
write_out(halves,"pooling-chain-halves")
write_out(cutoffs,"pooling-cutoffs")
