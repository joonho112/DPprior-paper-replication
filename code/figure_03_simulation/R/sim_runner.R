# ==============================================================================
# sim_runner.R — Sequential simulation loop (single-threaded)
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Paper:    "Design-Conditional Prior Elicitation for DP Mixtures"
# Author:   JoonHo Lee
#
# Description:
#   Runs the full simulation loop sequentially: for each (K_true, I_target,
#   replicate, method, confidence) combination, generates data, fits a DP
#   mixture model, and writes results incrementally to CSV.
#
#   For large-scale runs, prefer sim_runner_parallel.R instead.
#
# Functions:
#   raw_sum()            — Deterministic integer hash for seed generation
#   lookup_n_mean()      — Look up calibrated n_mean for given I_target
#   lookup_prior_ab()    — Look up (a, b) hyperprior for given method/mu_K
#   run_simulation()     — Main sequential simulation loop
#
# Dependencies: R/utils_config.R, R/utils_io.R, R/dgp_generate.R,
#               R/fit_dpmixture_nimble.R
# Called by:    scripts/step3_run_simulation.R (sequential mode)
# ==============================================================================

source("R/utils_config.R", local = TRUE)
source("R/utils_io.R", local = TRUE)
source("R/dgp_generate.R", local = TRUE)
source("R/fit_dpmixture_nimble.R", local = TRUE)

raw_sum <- function(x) {
  x <- as.character(x)
  sum(as.integer(charToRaw(x)))
}

lookup_n_mean <- function(calib_df, I_target) {
  idx <- which(abs(calib_df$I_target - I_target) < 1e-9)
  if (length(idx) != 1) stop(sprintf("n_mean not found for I_target=%.3f", I_target))
  calib_df$n_mean[idx]
}

lookup_prior_ab <- function(prior_df, method, mu_K, confidence) {
  if (method == "vague") return(list(a = 1.0, b = 1.0))

  if (method %in% c("tsmm", "dual")) {
    idx <- which(prior_df$method == method & prior_df$mu_K == mu_K & prior_df$confidence == confidence)
  } else {
    idx <- which(prior_df$method == method & prior_df$mu_K == mu_K & prior_df$confidence == "none")
  }

  if (length(idx) != 1) {
    stop(sprintf("Prior not found: method=%s mu_K=%s confidence=%s", method, mu_K, confidence))
  }
  list(a = prior_df$a[idx], b = prior_df$b[idx])
}

run_simulation <- function(cfg, n_reps, out_csv,
                           rep_start = 1L, rep_end = NULL,
                           verbose = TRUE) {

  ensure_project_dirs(cfg)

  calib_path <- file.path(cfg$paths$sim_dir, "informativeness_calibration.csv")
  prior_path <- file.path(cfg$paths$priors_dir, "prior_table.csv")
  if (!file.exists(calib_path)) stop("Missing calibration file. Run Step 1 first.")
  if (!file.exists(prior_path)) stop("Missing prior table. Run Step 2 first.")

  calib_df <- read_csv_auto(calib_path)
  prior_df <- read_csv_auto(prior_path)

  J              <- as.integer(cfg$design$J)
  K_grid         <- as.numeric(cfg$design$K_true_grid)
  I_grid         <- as.numeric(cfg$design$I_grid)
  conf_grid      <- as.character(cfg$design$confidence_grid)
  methods        <- as.character(cfg$methods$list)
  sigma_tau      <- as.numeric(cfg$dgp$sigma_tau)
  within_sd_raw  <- as.numeric(cfg$dgp$within_sd_raw)
  cluster_span_d <- as.numeric(cfg$dgp$cluster_span_d)
  weights        <- as.character(cfg$dgp$weights)
  n_cv           <- as.numeric(cfg$dgp$n_cv)
  n_min          <- as.integer(cfg$dgp$n_min)
  kappa          <- as.numeric(cfg$dgp$kappa)
  mcmc_cfg       <- cfg$mcmc

  if (is.null(rep_end)) rep_end <- rep_start + n_reps - 1L
  rep_start <- as.integer(rep_start)
  rep_end   <- as.integer(rep_end)

  if (file.exists(out_csv)) {
    message(sprintf("[run_simulation] Appending to existing file: %s", out_csv))
  } else {
    message(sprintf("[run_simulation] Creating new file: %s", out_csv))
  }

  for (r in rep_start:rep_end) {
    for (K_true in K_grid) {
      for (I_target in I_grid) {

        seed_dgp <- as.integer(cfg$seeds$dgp_base + 100000 * r + 1000 * K_true + round(100 * I_target))
        n_mean   <- lookup_n_mean(calib_df, I_target)

        ds <- generate_dataset(
          J = J, K_true = K_true, I_target = I_target, n_mean = n_mean,
          sigma_tau = sigma_tau, within_sd_raw = within_sd_raw,
          cluster_span_d = cluster_span_d,
          n_cv = n_cv, n_min = n_min, kappa = kappa,
          weights = weights, seed = seed_dgp
        )
        df_sim     <- ds$df
        I_realized <- ds$meta$I_realized

        for (method in methods) {
          conf_list <- if (method %in% c("tsmm", "dual")) conf_grid else "none"

          for (conf in conf_list) {
            ab <- lookup_prior_ab(prior_df, method, mu_K = K_true, confidence = conf)
            a <- ab$a; b <- ab$b

            seed_mcmc <- as.integer(
              cfg$seeds$mcmc_base +
                100000 * r + 1000 * K_true + round(100 * I_target) +
                raw_sum(method) + raw_sum(conf)
            )

            status  <- "ok"
            err_msg <- ""
            fit_sum <- tryCatch({
              fit_dp_mixture(df_sim, a = a, b = b, mcmc_cfg = mcmc_cfg,
                             seed = seed_mcmc, verbose = FALSE, K_true = K_true)
            }, error = function(e) {
              status  <<- "fail"
              err_msg <<- conditionMessage(e)
              NULL
            })

            row <- data.frame(
              scenario_J          = J,
              K_true              = K_true,
              I_target            = I_target,
              I_realized          = I_realized,
              replicate           = r,
              method              = method,
              confidence          = conf,
              a = a, b = b,
              status              = status,
              error               = err_msg,
              runtime_sec         = if (!is.null(fit_sum)) fit_sum$runtime_sec else NA_real_,
              post_EK             = if (!is.null(fit_sum)) fit_sum$post_EK else NA_real_,
              post_modeK          = if (!is.null(fit_sum)) fit_sum$post_modeK else NA_real_,
              post_probK1         = if (!is.null(fit_sum)) fit_sum$post_probK1 else NA_real_,
              post_medianK        = if (!is.null(fit_sum)) fit_sum$post_medianK else NA_real_,
              post_sdK            = if (!is.null(fit_sum)) fit_sum$post_sdK else NA_real_,
              post_q025           = if (!is.null(fit_sum)) fit_sum$post_q025 else NA_real_,
              post_q975           = if (!is.null(fit_sum)) fit_sum$post_q975 else NA_real_,
              post_probK_eq_true  = if (!is.null(fit_sum)) fit_sum$post_probK_eq_true else NA_real_,
              post_probK_within1  = if (!is.null(fit_sum)) fit_sum$post_probK_within1 else NA_real_,
              n_draws             = if (!is.null(fit_sum)) fit_sum$n_draws else NA_integer_,
              alpha_mean          = if (!is.null(fit_sum)) fit_sum$alpha_mean else NA_real_,
              alpha_sd            = if (!is.null(fit_sum)) fit_sum$alpha_sd else NA_real_,
              alpha_rhat          = if (!is.null(fit_sum)) fit_sum$alpha_rhat else NA_real_,
              stringsAsFactors    = FALSE
            )

            write_csv_safe(row, out_csv, append = TRUE)

            if (verbose) {
              ek_str <- if (!is.null(fit_sum)) sprintf("%.2f", fit_sum$post_EK) else "NA"
              msg <- sprintf("[rep=%d K=%d I=%.1f method=%s conf=%s] status=%s EK=%s",
                             r, K_true, I_target, method, conf, status, ek_str)
              message(msg)
              if (status != "ok") message("  Error: ", err_msg)
            }
          }
        }
      }
    }
  }

  invisible(out_csv)
}
