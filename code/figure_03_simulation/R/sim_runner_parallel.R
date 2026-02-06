# ==============================================================================
# sim_runner_parallel.R — Parallel simulation runner with checkpointing
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Paper:    "Design-Conditional Prior Elicitation for DP Mixtures"
# Author:   JoonHo Lee
#
# Description:
#   Parallel simulation runner using {future} and {future.apply}. Key features:
#     - OS-aware plan selection: multicore (fork) on macOS/Linux for memory
#       efficiency; multisession on Windows.
#     - Replication-level checkpointing: each replication writes a separate
#       CSV part file, enabling pause/resume workflows.
#     - Compiled NIMBLE objects are cached per worker process (see
#       fit_dpmixture_nimble.R) to avoid repeated compilation overhead.
#
# Functions:
#   get_default_workers()            — OS-aware worker count heuristic
#   select_future_plan()             — Choose multicore vs. multisession
#   run_one_dataset_replicate()      — Run all methods for one (r, K, I) combo
#   run_one_replication()            — Run one full replication (all K × I)
#   run_simulation_parallel()        — Public API: parallel simulation with merge
#
# Dependencies: future, future.apply, parallel,
#               R/utils_config.R, R/utils_io.R, R/dgp_generate.R,
#               R/fit_dpmixture_nimble.R
# Called by:    scripts/step3_run_simulation.R (parallel mode)
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

# ---- OS-aware defaults ----
get_default_workers <- function() {
  cores_logical  <- parallel::detectCores(logical = TRUE)
  cores_physical <- parallel::detectCores(logical = FALSE)
  if (is.na(cores_physical) || cores_physical < 1L) {
    cores_physical <- max(1L, as.integer(cores_logical %/% 2L))
  }

  sysname <- Sys.info()[["sysname"]]
  if (.Platform$OS.type == "windows") return(max(1L, min(16L, cores_physical)))
  if (identical(sysname, "Darwin"))   return(max(1L, min(12L, cores_physical)))
  max(1L, cores_physical - 1L)   # Linux: leave one core free
}

select_future_plan <- function(plan_name = "auto") {
  plan_name <- tolower(as.character(plan_name))
  if (plan_name %in% c("auto", "")) {
    if (.Platform$OS.type == "windows") return("multisession")
    return("multicore")
  }
  if (plan_name %in% c("multicore", "multisession")) return(plan_name)
  warning("Unknown parallel plan='", plan_name, "'. Falling back to 'auto'.")
  if (.Platform$OS.type == "windows") return("multisession")
  "multicore"
}

# ---- Core worker logic ----
run_one_dataset_replicate <- function(cfg, calib_df, prior_df,
                                      r, K_true, I_target,
                                      verbose = FALSE) {

  J              <- as.integer(cfg$design$J)
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

  rows <- list()
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
        alpha_mean          = if (!is.null(fit_sum)) fit_sum$alpha_mean else NA_real_,
        alpha_sd            = if (!is.null(fit_sum)) fit_sum$alpha_sd else NA_real_,
        alpha_rhat          = if (!is.null(fit_sum)) fit_sum$alpha_rhat else NA_real_,
        n_draws             = if (!is.null(fit_sum)) fit_sum$n_draws else NA_integer_,
        stringsAsFactors    = FALSE
      )

      rows[[length(rows) + 1L]] <- row
      rm(fit_sum); invisible(gc())
    }
  }

  do.call(rbind, rows)
}

run_one_replication <- function(cfg, calib_df, prior_df, r,
                                K_grid, I_grid, verbose = FALSE) {
  out <- list()
  for (K_true in K_grid) {
    for (I_target in I_grid) {
      out[[length(out) + 1L]] <- run_one_dataset_replicate(
        cfg = cfg, calib_df = calib_df, prior_df = prior_df,
        r = as.integer(r), K_true = as.numeric(K_true),
        I_target = as.numeric(I_target), verbose = verbose
      )
    }
  }
  do.call(rbind, out)
}

# ---- Public parallel API ----
run_simulation_parallel <- function(cfg, rep_start, rep_end, out_csv,
                                    workers = NULL, chunk_size = 1L,
                                    plan = NULL, skip_completed = NULL,
                                    cleanup_parts = NULL, verbose = TRUE) {

  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)) {
    stop("Packages 'future' and 'future.apply' are required.")
  }

  ensure_project_dirs(cfg)

  calib_path <- file.path(cfg$paths$sim_dir, "informativeness_calibration.csv")
  prior_path <- file.path(cfg$paths$priors_dir, "prior_table.csv")
  if (!file.exists(calib_path)) stop("Missing calibration file. Run Step 1 first.")
  if (!file.exists(prior_path)) stop("Missing prior table. Run Step 2 first.")

  calib_df <- read_csv_auto(calib_path)
  prior_df <- read_csv_auto(prior_path)
  K_grid   <- as.numeric(cfg$design$K_true_grid)
  I_grid   <- as.numeric(cfg$design$I_grid)

  rep_start <- as.integer(rep_start)
  rep_end   <- as.integer(rep_end)
  if (rep_end < rep_start) stop("rep_end must be >= rep_start")
  reps <- rep_start:rep_end

  if (is.null(plan)) {
    plan <- if (!is.null(cfg$parallel) && !is.null(cfg$parallel$plan)) cfg$parallel$plan else "auto"
  }
  plan <- select_future_plan(plan)

  if (is.null(workers) || is.na(workers)) {
    if (!is.null(cfg$parallel) && !is.null(cfg$parallel$workers)) {
      workers <- as.integer(cfg$parallel$workers)
    } else {
      workers <- get_default_workers()
    }
  }
  workers <- as.integer(max(1L, workers))

  chunk_size <- as.integer(chunk_size)
  if (is.na(chunk_size) || chunk_size < 1L) chunk_size <- 1L

  if (is.null(skip_completed)) skip_completed <- TRUE
  if (is.null(cleanup_parts)) cleanup_parts <- FALSE

  parts_dir <- file.path(cfg$paths$sim_dir, sprintf("parts_full_rep_%d_%d", rep_start, rep_end))
  dir.create(parts_dir, recursive = TRUE, showWarnings = FALSE)

  if (verbose) {
    message(sprintf("[parallel] reps=%d (%d..%d), plan=%s, workers=%d",
                    length(reps), rep_start, rep_end, plan, workers))
  }

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)

  if (identical(plan, "multicore") && .Platform$OS.type != "windows") {
    future::plan(future::multicore, workers = workers)
  } else {
    future::plan(future::multisession, workers = workers)
  }

  idx    <- seq_along(reps)
  chunks <- split(reps, ceiling(idx / chunk_size))

  part_files_list <- future.apply::future_lapply(
    chunks,
    function(r_chunk) {
      out_paths <- character(0)
      for (r in r_chunk) {
        part_path <- file.path(parts_dir, sprintf("part_rep_%06d.csv", as.integer(r)))
        if (isTRUE(skip_completed) && file.exists(part_path)) {
          out_paths <- c(out_paths, part_path)
          next
        }

        df_r <- tryCatch({
          run_one_replication(
            cfg = cfg, calib_df = calib_df, prior_df = prior_df,
            r = as.integer(r), K_grid = K_grid, I_grid = I_grid, verbose = FALSE
          )
        }, error = function(e) {
          data.frame(
            scenario_J = as.integer(cfg$design$J),
            K_true = NA_real_, I_target = NA_real_, I_realized = NA_real_,
            replicate = as.integer(r), method = NA_character_,
            confidence = NA_character_, a = NA_real_, b = NA_real_,
            status = "fail", error = conditionMessage(e),
            runtime_sec = NA_real_, post_EK = NA_real_, post_modeK = NA_real_,
            post_probK1 = NA_real_, post_medianK = NA_real_, post_sdK = NA_real_,
            post_q025 = NA_real_, post_q975 = NA_real_,
            post_probK_eq_true = NA_real_, post_probK_within1 = NA_real_,
            alpha_mean = NA_real_, alpha_sd = NA_real_, alpha_rhat = NA_real_,
            n_draws = NA_integer_, stringsAsFactors = FALSE
          )
        })

        write_csv_safe(df_r, part_path, append = FALSE)
        rm(df_r); invisible(gc())
        out_paths <- c(out_paths, part_path)
      }
      out_paths
    },
    future.seed = TRUE
  )

  part_files <- sort(unique(unlist(part_files_list, use.names = FALSE)))
  part_files <- part_files[file.exists(part_files)]
  if (length(part_files) == 0) stop("No part files produced. Check worker errors.")

  df_all <- read_many_csv(
    files = part_files,
    key_cols = c("replicate", "K_true", "I_target", "method", "confidence")
  )
  if ("source_file" %in% names(df_all)) df_all$source_file <- NULL

  dir.create(dirname(out_csv), recursive = TRUE, showWarnings = FALSE)
  write_csv_safe(df_all, out_csv, append = FALSE)
  if (verbose) message("[parallel] Saved: ", out_csv)

  if (isTRUE(cleanup_parts)) {
    unlink(parts_dir, recursive = TRUE, force = TRUE)
  }

  rm(df_all); invisible(gc())
  invisible(out_csv)
}
