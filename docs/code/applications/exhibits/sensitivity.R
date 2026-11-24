# Author: JoonHo Lee (jlee296@ua.edu)
# Integrate squared shifts on all ranks or the central 90%, then match unit or score-group means.
rank_sums <- frozen("rank-bin-sums")
units <- frozen("chain-mean-by-unit-or-score")
frequencies <- ex_read("rasch","score_counts.csv","data-frozen")
stopifnot(identical(frequencies$raw_score,0:13),sum(frequencies$n)==500L)
for (app in names(apps)) {
  x <- units[units$app==app,]
  partitions <- split(x,list(x$prior,x$block),drop=TRUE)
  for (z in partitions) {
    if (app=="rasch") {
      stopifnot(identical(z$unit_or_score,frequencies$raw_score),identical(z$frequency,frequencies$n))
    } else stopifnot(identical(z$unit_or_score,seq_len(apps[app])),all(z$frequency==1L))
  }
}
unit_keys <- c("app","prior","unit_or_score","frequency")
full <- aggregate(units["mean"],units[unit_keys],mean)
full$block <- "full"
units <- rbind(units,full[names(units)])
distance <- function(z) {
  a <- z[1,]
  idx <- order(z$bin); z <- z[idx,]; stopifnot(identical(z$bin,1:100))
  u <- units[units$app==a$app & units$prior==a$prior_a & units$block==a$block_a,]
  v <- units[units$app==a$app & units$prior==a$prior_b & units$block==a$block_b,]
  v <- v[match(u$unit_or_score,v$unit_or_score),]
  n <- unname(apps[a$app]); draws <- if (a$block_a=="full") 16000L else 8000L
  stopifnot(identical(v$unit_or_score,u$unit_or_score),identical(v$frequency,u$frequency),
            sum(u$frequency)==n,all(z$n_rank==draws*n/100))
  central <- z$bin>5 & z$bin<=95
  data.frame(app=a$app,prior_a=a$prior_a,block_a=a$block_a,prior_b=a$prior_b,block_b=a$block_b,
    n=n,n_ranks=sum(z$n_rank),w2=sqrt(sum(z$sum_sq)/sum(z$n_rank)),
    w2_central_90=sqrt(sum(z$sum_sq[central])/sum(z$n_rank[central])),
    unit_rms=sqrt(weighted.mean((v$mean-u$mean)^2,u$frequency)))
}
pair_keys <- c("app","prior_a","block_a","prior_b","block_b")
metrics <- do.call(rbind,lapply(split(rank_sums,rank_sums[pair_keys],drop=TRUE),distance))
rownames(metrics) <- NULL
lookup <- function(app,a,block_a,b,block_b=block_a) {
  row <- metrics[metrics$app==app & metrics$prior_a==a & metrics$block_a==block_a &
                   metrics$prior_b==b & metrics$block_b==block_b,]
  stopifnot(nrow(row)==1L);row
}
save_frame(metrics,"pair-and-split-metrics")
pairs <- list(); split_rows <- list()
for (app in names(apps)) {
  for (comb in combn(names(priors),2,simplify=FALSE)) {
    z <- lookup(app,comb[1],"full",comb[2])
    z$prior_a <- priors[comb[1]];z$prior_b <- priors[comb[2]]
    pairs[[length(pairs)+1L]] <- z[c("app","prior_a","prior_b","n","n_ranks","w2","w2_central_90","unit_rms")]
  }
  for (metric in c("w2","w2_central_90","unit_rms")) {
    cross <- function(block) lookup(app,"gamma",block,"dual")[[metric]]
    within <- function(prior) lookup(app,prior,"chains_12",prior,"chains_34")[[metric]]
    split_rows[[length(split_rows)+1L]] <- data.frame(app,metric,full=cross("full"),
       chains_12=cross("chains_12"),chains_34=cross("chains_34"),within_gamma=within("gamma"),within_dual=within("dual"))
  }
}
h17 <- do.call(rbind,pairs); h18 <- do.call(rbind,split_rows)
h17_view <- h17[c("app","prior_a","prior_b","w2","w2_central_90","unit_rms")]
h17_view[4:6] <- lapply(h17_view[4:6],sprintf,fmt="%.5f")
h17_view$app <- app_names[h17$app]
ex_table("osm-table-H.17",h17,h17_view,sources=frame_path("pair-and-split-metrics"),
 note="Root mean square changes in the original effect units. The central column includes matching ranks between the 5th and 95th percentiles.")
view18 <- h18; view18$app <- app_names[view18$app]
view18$metric <- c(w2="All quantiles (W2)",w2_central_90="5th–95th percentiles",unit_rms="Individual estimates")[h18$metric]
view18[3:7] <- lapply(view18[3:7],sprintf,fmt="%.5f")
ex_table("osm-table-H.18",h18,view18,sources=frame_path("pair-and-split-metrics"),
 note="Across-prior estimates use the indicated chains. Within-prior distances compare chains 1–2 with 3–4. Each half contributes 8,000 draws for each observed unit.")
panel_a <- h17[h17$prior_a==priors["gamma"] & h17$prior_b==priors["dual"],c("app","unit_rms","w2")]
names(panel_a) <- c("application","rms_change_same_unit","rms_change_same_percentile")
save_frame(panel_a,"figure-5-panel-a")
quantiles <- frozen("percentile-values")
panel_b <- merge(quantiles[quantiles$prior=="gamma",c("app","block","p","quantile")],
                 quantiles[quantiles$prior=="dual",c("app","block","p","quantile")],
                 by=c("app","block","p"),suffixes=c("_gamma","_dual"))
names(panel_b)[4:5] <- c("gamma_quantile","dual_quantile")
panel_b$shift <- panel_b$dual_quantile-panel_b$gamma_quantile
stopifnot(nrow(panel_b)==1791L,all(table(panel_b$app,panel_b$block)==199L))
save_frame(panel_b,"figure-5-panel-b")
