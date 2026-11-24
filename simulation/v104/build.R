#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Rebuild the revised simulation exhibits, using the synthetic terminal summaries.
file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub("~+~", " ", sub("^--file=", "", file_arg[1]), fixed = TRUE)))
source(file.path(.script_dir, "..", "..", "common", "R", "bootstrap.R"))
source(rp_path("exhibits", "helpers.R"))
for (f in c("percentiles.R", "figure4.R", "percentile-figures.R", "replay-figure.R"))
  source(file.path(.script_dir, f))
if (system2("python3", shQuote(file.path(.script_dir, "check_inputs.py"))) != 0)
  stop("Synthetic simulation input integrity check failed.")
directory <- rp_path("data-frozen", "simulation104")
derived <- rp_path("data-derived", "v104")
out <- rp_path("outputs", "v104")
dir.create(derived, recursive = TRUE, showWarnings = FALSE)
dir.create(out, recursive = TRUE, showWarnings = FALSE)
read_frozen <- function(name) read.csv(file.path(directory, paste0(name, ".csv")))
save_frame <- function(x, name) rp_write_csv(x, file.path(derived, paste0(name, ".csv")))
source_files <- function(...) file.path("data-frozen", "simulation104", paste0(c(...), ".csv"))
record_figure <- function(id, frame, sources, note) {
  ex_meta(id, file.path(out, paste0(id, c(".pdf", ".png"))), frame, sources,
          "rebuilt-from-synthetic-replicates", note)
}
priors <- c("Vague", "DORO-Unif", "TSMM", "Dual-Anchor", "SSI-Weight")
design <- c("K_true", "I_target")
cell <- c(design, "prior")
study <- c(design, "replicate", "dgp_sha256")
order_cells <- function(x) x[order(x$K_true, x$I_target, match(x$prior, priors)), , drop = FALSE]

# Join fitted outcomes to their generating dataset. Different priors have distinct fits.
history <- ex_read("simulation", "headline_fits.csv")
primary <- ex_read("simulation", "headline_primary.csv")
spreads <- read_frozen("posterior-spread")
stopifnot(nrow(history) == 8607L, !anyDuplicated(history$fit_key),
          setequal(history$fit_key, spreads$fit_key), length(unique(history$dgp_sha256)) == 1786,
          all(history$J == 100), all(history$grand_mean_mean == 0),
          all(history$grand_mean_variance == 1), all(history$n_manifest == 200),
          !anyDuplicated(history[c(cell, "replicate")]),
          !anyDuplicated(unique(history[study])[c(design, "replicate")]),
          !anyDuplicated(unique(history[study])$dgp_sha256), setequal(history$prior, priors))
fits <- merge(history[c(study, "prior", "fit_key", "E_K", "p_K1", "tau_rmse")], spreads,
              by = "fit_key", sort = FALSE)
fits <- fits[order(fits$K_true, fits$I_target, match(fits$prior, priors), fits$replicate), ]
read_sites <- function(name, keys) {
  x <- read_frozen(name)
  stopifnot(identical(names(x)[-1], sprintf("site_%03d", 1:100)),
            !anyDuplicated(x[[1]]), setequal(keys, x[[1]]))
  m <- as.matrix(x[-1])
  rownames(m) <- x[[1]]
  stopifnot(all(is.finite(m)))
  m
}
vectors <- list(PM = read_sites("sites-pm", fits$fit_key),
                CB = read_sites("sites-cb", fits$fit_key),
                truth = read_sites("sites-truth", fits$dgp_sha256))
# Integrate each squared CDF gap exactly between consecutive pooled jump points.
loss <- function(a, b) {
  a <- sort(a)
  b <- sort(b)
  z <- sort(unique(c(a, b)))
  gap <- findInterval(z, a) / length(a) - findInterval(z, b) / length(b)
  c(ISEL = sum(diff(z) * head(gap^2, -1)), KS = max(abs(gap)))
}
pm <- vectors$PM[fits$fit_key, , drop = FALSE]
cb <- vectors$CB[fits$fit_key, , drop = FALSE]
truth <- vectors$truth[fits$dgp_sha256, , drop = FALSE]
fits$true_sd <- sqrt(rowMeans((truth - rowMeans(truth))^2))
fits$site_PM_RMSE <- sqrt(rowMeans((pm - truth)^2))
fits$sd_bias <- fits$posterior_mean_sd - fits$true_sd
fits$collapse_prob <- fits$p_K1
losses <- t(vapply(seq_len(nrow(fits)), function(i) {
  c(loss(pm[i, ], truth[i, ]), loss(cb[i, ], truth[i, ]))
}, numeric(4)))
colnames(losses) <- c("PM_ISEL","PM_KS","CB_ISEL","CB_KS")
fits <- cbind(fits, losses)
stopifnot(max(abs(fits$site_PM_RMSE - fits$tau_rmse)) < 1e-12,
          max(abs(fits$true_sd - .25 * sqrt(99 / 100))) < 1e-12)
fits$tau_rmse <- NULL
fits$p_K1 <- NULL
save_frame(fits, "simulation-fit-metrics")
metrics <- c("E_K","collapse_prob","posterior_mean_sd","true_sd","sd_bias","site_PM_RMSE",
             "PM_ISEL","PM_KS","CB_ISEL","CB_KS")
summaries <- function(x, count_field) {
  counts <- aggregate(list(n = x$replicate), x[cell], length)
  names(counts)[names(counts) == "n"] <- count_field
  avg <- aggregate(x[metrics], x[cell], mean)
  se <- aggregate(x[metrics], x[cell], function(v) sd(v) / sqrt(length(v)))
  names(se)[match(metrics, names(se))] <- paste0(metrics, "_mcse")
  order_cells(Reduce(function(a,b) merge(a,b,by=cell), list(counts,avg,se)))
}
per_prior <- summaries(fits, "n")
per_prior$attempted <- 200L
checked <- merge(per_prior, primary[c(cell, "n_scientific_eligible")], by=cell)
stopifnot(nrow(checked) == 45, all(checked$n == checked$n_scientific_eligible))
save_frame(per_prior, "simulation-cell-means")
joint <- aggregate(list(n_priors = fits$prior), fits[study], length)
all_five <- merge(fits, joint[joint$n_priors == 5L, study], by = study)
five_means <- summaries(all_five, "n_all_five")
five_means <- order_cells(merge(five_means, per_prior[c(cell, "n", "attempted")], by = cell))
names(five_means)[match(c("n", "attempted"), names(five_means))] <- c("n_converged", "n_attempted")
names(five_means) <- sub("_mcse$", "_MCSE", names(five_means))
ranges <- unique(five_means[c(design, "n_all_five")])
for (metric in c("CB_ISEL", "site_PM_RMSE")) {
  range_pct <- aggregate(five_means[metric], five_means[design],
                         function(x) 100 * (max(x) - min(x)) / min(x))
  names(range_pct)[3] <- paste0(metric, "_range_pct")
  ranges <- merge(ranges, range_pct, by = design)
}
stopifnot(nrow(ranges) == 9, identical(range(ranges$n_all_five), c(124L,200L)))
save_frame(five_means, "simulation-all-five-means")
save_frame(ranges, "simulation-all-five-ranges")

# Low-information count accuracy, including explicit attempted and retained denominators.
counts_columns <- c(cell, "n_scientific_eligible", "n_manifest", "mean_E_K", "sd_between_E_K",
                    "mean_p_K1", "coverage95_K", "mean_interval_width_K")
counts <- order_cells(primary[primary$I_target == .2, c(counts_columns, "mean_K_bias", "rmse_E_K")])
show_counts <- data.frame(
  "Generating count"=counts$K_true, Prior=counts$prior,
  "Mean count (SD)"=sprintf("%.2f (%.2f)", counts$mean_E_K, counts$sd_between_E_K),
  "Single-cluster probability"=sprintf("%.3f", counts$mean_p_K1),
  "Coverage"=sprintf("%.3f", counts$coverage95_K),
  "Mean width"=sprintf("%.2f", counts$mean_interval_width_K),
  "Converged / attempted"=paste0(counts$n_scientific_eligible,"/",counts$n_manifest), check.names=FALSE)
count_note <- "J=100, grand-mean prior N(0,1), informativeness 0.2. The SD is across replications. Vague denotes Gamma(1,1). DORO-Unif is an external reference."
count_source <- "data-derived/simulation/headline_primary.csv"
ex_table("main-table-3", counts[counts_columns], show_counts, sources=count_source, note=count_note)
show_counts$Bias <- sprintf("%.2f", counts$mean_K_bias)
show_counts$RMSE <- sprintf("%.2f", counts$rmse_E_K)
ex_table("osm-table-F.6", counts, show_counts, sources=count_source, note=count_note)

# Distribution errors on each prior's eligible replications. Use the exported mean
# and round before formatting, following the manuscript tables at decimal ties.
exported_means <- read.csv(file.path(derived, "simulation-cell-means.csv"))
dist_cols <- c(cell,"n","E_K","posterior_mean_sd","sd_bias","site_PM_RMSE","PM_ISEL","CB_ISEL","PM_KS","CB_KS")
info <- c(.2,.5,.8)
for (i in seq_along(info)) {
  values <- per_prior[per_prior$I_target == info[i], dist_cols]
  display <- exported_means[exported_means$I_target == info[i], setdiff(dist_cols, "I_target")]
  display$prior[display$prior=="Vague"] <- "Gamma(1,1)"
  digits <- c(E_K=2, posterior_mean_sd=4, sd_bias=4, site_PM_RMSE=4,
              PM_ISEL=5, CB_ISEL=5, PM_KS=4, CB_KS=4)
  for (m in names(digits)) display[[m]] <- sprintf(paste0("%.", digits[m], "f"), round(display[[m]], digits[m]))
  names(display) <- c("Generating count","Prior","Converged","Mean count","Finite SD","SD bias",
                      "Site RMSE","PM ISEL","CB ISEL","PM KS","CB KS")
  ex_table(paste0("osm-table-F.", 11L+i), values, display,
           sources="data-derived/v104/simulation-cell-means.csv",
           note=paste("Informativeness", info[i], "with 200 attempts per prior and design."))
}

# Inverse empirical CDFs are order statistics of the 100 reconstructed site effects.
paired <- paired_percentiles(fits, vectors)
stopifnot(sum(paired$support$n_paired)==1681,
          identical(range(paired$support$n_paired), c(165L,200L)))
save_frame(paired$support, "simulation-pair-convergence")
save_frame(paired$replicates, "simulation-replicate-percentiles")
save_frame(paired$percentiles, "simulation-percentiles")
save_frame(paired$upper_vs_median, "simulation-upper-vs-median")
percentile_source <- "data-derived/v104/simulation-percentiles.csv"
for (i in seq_along(info)) {
  values <- paired$percentiles[paired$percentiles$I_target == info[i], ]
  display <- values[c("K_true","n","percentile","mean_truth","bias_gamma","mae_gamma","rmse_gamma",
                      "bias_dual","mae_dual","rmse_dual","mean_shift","mean_abs_shift")]
  for (m in names(display)[4:12]) {
    digits <- if (m %in% c("mean_shift", "mean_abs_shift")) 5 else 4
    rounded <- round(display[[m]], digits)
    rounded[rounded == 0] <- 0
    display[[m]] <- sprintf(paste0("%.", digits, "f"), rounded)
  }
  names(display) <- c("Generating count","Paired n","Percentile","Mean truth","Gamma bias",
                      "Gamma MAE","Gamma RMSE","Dual bias","Dual MAE","Dual RMSE","Signed change","Absolute change")
  ex_table(paste0("osm-table-F.", 15L+i), values, display, sources=percentile_source,
           note=paste("Informativeness", info[i], "on matched data. Changes are Dual-Anchor minus Gamma(1,1)."))
}

# Reconstruct selected GR errors and check the plotting grid against all discrete supports.
curves <- read_frozen("selected-curves")
points <- read_frozen("selected-finite-site-supports")
audit <- read_frozen("selected-replay-audit")
replay <- read_frozen("selected-truth-distances")
case_key <- c("K_true", "I", "replicate", "prior")
replay <- merge(replay, audit, by=case_key, suffixes=c("", "_audit"))
stopifnot(nrow(replay) == 4L, !anyDuplicated(replay$fit_key),
          all(replay$converged & replay$converged_audit & replay$seed_manifest_match &
              replay$posterior_within_archive_tolerance), all(replay$source_SHA_verified),
          all(replay$posterior_draws %in% c(32000,240000)),
          all(replay$replicate == 1L), all(replay$I == .2))
for (i in seq_len(nrow(replay))) {
  row <- replay[i, ]
  src <- fits[fits$fit_key == row$fit_key, ]
  stopifnot(nrow(src)==1, src$K_true==row$K_true, src$I_target==row$I, src$prior==row$prior,
            src$replicate==row$replicate, src$dgp_sha256==row$dgp_sha256,
            max(abs(src$E_K-row$archived_E_K), abs(src$site_PM_RMSE-row$archived_RMSE))<1e-10)
  p <- points[points$K_true==row$K_true & points$prior==row$prior, ]
  z <- curves[curves$K_true==row$K_true & curves$prior==row$prior, ]
  stopifnot(nrow(p)==100L, identical(p$index, 1:100), nrow(z)==1201L)
  for (type in c("truth","GR","CB","PM")) {
    support <- p[[paste0(type,"_order_stat")]]
    stopifnot(!is.unsorted(support), max(abs(findInterval(z$x,support)/100 - z[[paste0(type,"_CDF")]])) < 1e-12)
  }
  gr <- loss(p$GR_order_stat, p$truth_order_stat)
  stopifnot(max(abs(gr - c(row$GR_ISEL,row$GR_KS)))<1e-12)
  replay$GR_ISEL[i] <- gr["ISEL"]
  replay$GR_KS[i] <- gr["KS"]
}
replay <- replay[order(replay$K_true, match(replay$prior, priors)), ]
display <- replay[c("K_true","prior","replicate","posterior_draws","posterior_CDF_ISEL","posterior_CDF_KS","GR_ISEL","GR_KS")]
display$prior[display$prior=="Vague"] <- "Gamma(1,1)"
for (m in c("posterior_CDF_ISEL","GR_ISEL")) display[[m]] <- sprintf("%.6f",display[[m]])
for (m in c("posterior_CDF_KS","GR_KS")) display[[m]] <- sprintf("%.4f",display[[m]])
names(display) <- c("Generating count","Prior","Replicate","Joint draws","Posterior CDF ISEL","Posterior CDF KS","GR ISEL","GR KS")
ex_table("osm-table-F.15", replay, display,
         sources=source_files("selected-replay-audit","selected-truth-distances","selected-finite-site-supports"),
         note="Full-line posterior CDF errors use archived pooled-draw loss summaries. GR errors are recomputed from the 100 ordered supports of each fit.")

draw_replay(curves, out)
record_figure("osm-figure-F.5", curves, source_files("selected-curves"),
              "Four replayed fits over two low-information data sets. The plotting grid has 1,201 points per fit.")
draw_percentile_figures(paired$percentiles, out)
for (id in c("osm-figure-F.6","osm-figure-F.7"))
  record_figure(id, paired$percentiles, percentile_source,
                "Finite-site inverse empirical CDFs on 1,681 matched Gamma(1,1) and Dual-Anchor datasets.")

# Long numeric figure frame identifies the denominator and effect units of each mark.
panel_frame <- function(panel, d, metric, value, denominator, prior=NA_character_, percentile=NA_integer_) {
  data.frame(panel=panel, K_true=d$K_true, I_target=d$I_target, n=denominator,
             prior=prior, percentile=percentile, metric=metric, value=value)
}
selected <- paired$percentiles[paired$percentiles$percentile %in% c(50,95,99), ]
figure <- rbind(panel_frame("A",five_means,"posterior_mean_sd",five_means$posterior_mean_sd,
                            five_means$n_all_five, prior=five_means$prior),
                panel_frame("B",ranges,"CB_ISEL_range_pct",ranges$CB_ISEL_range_pct,ranges$n_all_five),
                panel_frame("B",ranges,"site_PM_RMSE_range_pct",ranges$site_PM_RMSE_range_pct,ranges$n_all_five),
                panel_frame("C",selected,"mean_abs_shift",selected$mean_abs_shift,
                            selected$n, percentile=selected$percentile))
draw_main_figure(five_means, ranges, paired$percentiles, out)
record_figure("main-figure-4", figure,
              c("data-derived/v104/simulation-all-five-means.csv","data-derived/v104/simulation-all-five-ranges.csv",percentile_source),
              "Panels A and B require all five priors to converge. Panel C uses paired Gamma(1,1) and Dual-Anchor fits.")
cat("Built 13 revised simulation exhibits from", nrow(fits),"converged fits.\n")
