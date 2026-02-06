#!/usr/bin/env Rscript
# ==============================================================================
# step3_smoketest.R — Minimal end-to-end smoke test (tiny MCMC)
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Pipeline: Step 3a of 7 — Smoke test (recommended before pilot/full runs)
# Depends:  Steps 1-2 (calibration + priors)
# Produces: results/sim/raw_results_smoketest.csv (1 row)
# Runtime:  ~1-3 minutes (single scenario, tiny MCMC: 200 iter, 1 chain)
#
# Description:
#   Runs a single scenario (K*=5, I=0.5, vague prior) with a very short
#   MCMC chain to verify that the entire pipeline compiles and executes.
#   Use this to confirm NIMBLE installation works before longer runs.
#
# Usage:
#   Rscript scripts/step3_smoketest.R
#   Rscript scripts/step3_smoketest.R --config config/fig3_design.yaml
#
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
config_path <- "config/fig3_design.yaml"
if (length(args) >= 2 && args[1] == "--config") config_path <- args[2]

source("R/utils_config.R", local = TRUE)
source("R/utils_io.R", local = TRUE)
source("R/dgp_generate.R", local = TRUE)
source("R/fit_dpmixture_nimble.R", local = TRUE)

cfg <- read_config(config_path)
ensure_project_dirs(cfg)

calib_path <- file.path(cfg$paths$sim_dir, "informativeness_calibration.csv")
if (!file.exists(calib_path)) stop("Missing calibration file. Run Step 1 first.")
calib_df <- read_csv_auto(calib_path)

J <- as.integer(cfg$design$J)
K_true <- 5
I_target <- 0.5

idx <- which(abs(calib_df$I_target - I_target) < 1e-9)
if (length(idx) != 1) stop("Cannot find n_mean for I_target=0.5")
n_mean <- calib_df$n_mean[idx]

seed_dgp <- as.integer(cfg$seeds$dgp_base + 999)
ds <- generate_dataset(
  J = J, K_true = K_true, I_target = I_target, n_mean = n_mean,
  sigma_tau = cfg$dgp$sigma_tau,
  within_sd_raw = cfg$dgp$within_sd_raw,
  cluster_span_d = cfg$dgp$cluster_span_d,
  n_cv = cfg$dgp$n_cv, n_min = cfg$dgp$n_min, kappa = cfg$dgp$kappa,
  weights = cfg$dgp$weights,
  seed = seed_dgp
)

df_sim <- ds$df
a <- 1.0; b <- 1.0

mcmc_cfg <- cfg$mcmc
mcmc_cfg$nchains <- 1
mcmc_cfg$niter <- 200
mcmc_cfg$nburnin <- 100
mcmc_cfg$thin <- 1
mcmc_cfg$monitors <- c("alpha", "z")
mcmc_cfg$summary <- FALSE
mcmc_cfg$setSeed <- TRUE

cat("\n[Step 3 Smoke Test] Running tiny NIMBLE MCMC...\n")
fit_sum <- fit_dp_mixture(df_sim, a = a, b = b, mcmc_cfg = mcmc_cfg,
                          seed = cfg$seeds$mcmc_base + 999, verbose = TRUE)

row <- data.frame(
  scenario_J = J,
  K_true = K_true,
  I_target = I_target,
  I_realized = ds$meta$I_realized,
  replicate = 1,
  method = "vague",
  confidence = "none",
  a = a,
  b = b,
  status = "ok",
  runtime_sec = fit_sum$runtime_sec,
  post_EK = fit_sum$post_EK,
  post_modeK = fit_sum$post_modeK,
  post_probK1 = fit_sum$post_probK1,
  stringsAsFactors = FALSE
)

out_csv <- file.path(cfg$paths$sim_dir, "raw_results_smoketest.csv")
write_csv_safe(row, out_csv, append = FALSE)

cat("Saved:", out_csv, "\n")
print(row)
cat("Done.\n")
