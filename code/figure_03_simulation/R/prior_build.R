# ==============================================================================
# prior_build.R — Build Gamma hyperprior table for all methods
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Paper:    "Design-Conditional Prior Elicitation for DP Mixtures"
# Author:   JoonHo Lee
#
# Description:
#   Constructs a table of calibrated Gamma(a, b) hyperprior parameters for
#   every combination of method and design point used in the simulation:
#     - vague:        Gamma(1, 1) baseline
#     - tsmm:         Two-Stage Moment Matching via DPprior_fit()
#     - dual:         Dual-Anchor refinement via DPprior_dual() (conditional)
#     - doro_uniform: KL fit to uniform-encoded PMF (Dorazio-style)
#     - chi2_doro:    KL fit to chi-square-encoded moments
#
# Functions:
#   assert_dpprior_available() — Check DPprior package is installed
#   build_prior_table()        — Main function: build full prior table from config
#
# Dependencies: DPprior (GitHub: joonho112/DPprior)
# Called by:    scripts/step2_build_priors.R
# ==============================================================================

assert_dpprior_available <- function() {
  if (!requireNamespace("DPprior", quietly = TRUE)) {
    stop("Package 'DPprior' is required. Install via remotes::install_github('joonho112/DPprior').")
  }
}

build_prior_table <- function(cfg) {
  assert_dpprior_available()

  J         <- as.integer(cfg$design$J)
  mu_grid   <- as.numeric(cfg$design$K_true_grid)
  conf_grid <- as.character(cfg$design$confidence_grid)

  tsmm_method      <- as.character(cfg$priors$tsmm_method)
  dual_trigger     <- as.numeric(cfg$priors$dual$trigger_prob_w1_gt_0_5)
  dual_target_prob <- as.numeric(cfg$priors$dual$w1_target_prob)
  dual_threshold   <- as.numeric(cfg$priors$dual$w1_threshold)
  dual_lambda      <- as.numeric(cfg$priors$dual$lambda)
  dual_loss        <- as.character(cfg$priors$dual$loss_type)

  rows <- list()

  # --- Vague baseline: Gamma(1, 1) ---
  moms_vague <- DPprior::exact_K_moments(J, 1.0, 1.0)
  rows[[length(rows) + 1]] <- data.frame(
    method = "vague", confidence = "none", J = J, mu_K = NA_real_,
    a = 1.0, b = 1.0, var_K_target = NA_real_,
    induced_EK = moms_vague$mean, induced_VarK = moms_vague$var,
    prob_w1_gt_0_5 = DPprior::prob_w1_exceeds(0.5, 1.0, 1.0),
    dual_applied = NA, note = "Gamma(1,1)",
    stringsAsFactors = FALSE
  )

  # --- TSMM and Dual-Anchor for each (mu_K, confidence) ---
  for (mu_K in mu_grid) {
    for (conf in conf_grid) {
      fit_tsmm  <- DPprior::DPprior_fit(J = J, mu_K = mu_K, confidence = conf, method = tsmm_method)
      diag_tsmm <- DPprior::DPprior_diagnostics(fit_tsmm)
      moms_tsmm <- DPprior::exact_K_moments(J, fit_tsmm$a, fit_tsmm$b)

      rows[[length(rows) + 1]] <- data.frame(
        method = "tsmm", confidence = conf, J = J, mu_K = mu_K,
        a = fit_tsmm$a, b = fit_tsmm$b,
        var_K_target = fit_tsmm$target$var_K,
        induced_EK = moms_tsmm$mean, induced_VarK = moms_tsmm$var,
        prob_w1_gt_0_5 = diag_tsmm$weights$prob_exceeds["prob_gt_0.5"],
        dual_applied = FALSE,
        note = sprintf("DPprior_fit(method=%s)", tsmm_method),
        stringsAsFactors = FALSE
      )

      # Dual-Anchor: apply only if dominance risk exceeds trigger threshold
      p_dom <- as.numeric(diag_tsmm$weights$prob_exceeds["prob_gt_0.5"])
      if (is.na(p_dom)) p_dom <- DPprior::prob_w1_exceeds(0.5, fit_tsmm$a, fit_tsmm$b)

      if (p_dom >= dual_trigger) {
        w1_target <- list(prob = list(threshold = dual_threshold, value = dual_target_prob))
        fit_dual  <- DPprior::DPprior_dual(
          fit_tsmm, w1_target = w1_target,
          lambda = dual_lambda, loss_type = dual_loss, verbose = FALSE
        )
        diag_dual <- DPprior::DPprior_diagnostics(fit_dual)
        moms_dual <- DPprior::exact_K_moments(J, fit_dual$a, fit_dual$b)

        rows[[length(rows) + 1]] <- data.frame(
          method = "dual", confidence = conf, J = J, mu_K = mu_K,
          a = fit_dual$a, b = fit_dual$b,
          var_K_target = fit_tsmm$target$var_K,
          induced_EK = moms_dual$mean, induced_VarK = moms_dual$var,
          prob_w1_gt_0_5 = diag_dual$weights$prob_exceeds["prob_gt_0.5"],
          dual_applied = TRUE,
          note = sprintf("DPprior_dual(lambda=%.2f, loss=%s)", dual_lambda, dual_loss),
          stringsAsFactors = FALSE
        )
      } else {
        # Below trigger: Dual-Anchor returns the TSMM parameters unchanged
        rows[[length(rows) + 1]] <- data.frame(
          method = "dual", confidence = conf, J = J, mu_K = mu_K,
          a = fit_tsmm$a, b = fit_tsmm$b,
          var_K_target = fit_tsmm$target$var_K,
          induced_EK = moms_tsmm$mean, induced_VarK = moms_tsmm$var,
          prob_w1_gt_0_5 = p_dom, dual_applied = FALSE,
          note = "Dominance below trigger; returned TSMM parameters",
          stringsAsFactors = FALSE
        )
      }
    }
  }

  # --- DORO-Uniform baseline ---
  make_doro_uniform_pmf <- function(mu_K) {
    m <- as.integer(2 * mu_K - 1)
    m <- min(m, J)
    p <- rep(0, J)
    p[1:m] <- 1 / m
    p
  }

  for (mu_K in mu_grid) {
    p_star   <- make_doro_uniform_pmf(mu_K)
    fit_doro <- DPprior::DPprior_a2_kl(J = J, target = p_star, method = "pmf", verbose = FALSE)
    moms     <- DPprior::exact_K_moments(J, fit_doro$a, fit_doro$b)

    rows[[length(rows) + 1]] <- data.frame(
      method = "doro_uniform", confidence = "none", J = J, mu_K = mu_K,
      a = fit_doro$a, b = fit_doro$b, var_K_target = NA_real_,
      induced_EK = moms$mean, induced_VarK = moms$var,
      prob_w1_gt_0_5 = DPprior::prob_w1_exceeds(0.5, fit_doro$a, fit_doro$b),
      dual_applied = NA,
      note = "DPprior_a2_kl(method=pmf, Dorazio-uniform)",
      stringsAsFactors = FALSE
    )
  }

  # --- Chi-square DORO baseline ---
  for (mu_K in mu_grid) {
    target   <- list(mu_K = mu_K, var_K = 2 * mu_K)   # chi-square-like: Var = 2 * mean
    fit_chi2 <- DPprior::DPprior_a2_kl(J = J, target = target, method = "chisq", verbose = FALSE)
    moms     <- DPprior::exact_K_moments(J, fit_chi2$a, fit_chi2$b)

    rows[[length(rows) + 1]] <- data.frame(
      method = "chi2_doro", confidence = "none", J = J, mu_K = mu_K,
      a = fit_chi2$a, b = fit_chi2$b, var_K_target = 2 * mu_K,
      induced_EK = moms$mean, induced_VarK = moms$var,
      prob_w1_gt_0_5 = DPprior::prob_w1_exceeds(0.5, fit_chi2$a, fit_chi2$b),
      dual_applied = NA,
      note = "DPprior_a2_kl(method=chisq, target Var=2*mu)",
      stringsAsFactors = FALSE
    )
  }

  prior_table <- do.call(rbind, rows)
  prior_table <- prior_table[, c(
    "method", "confidence", "J", "mu_K", "a", "b",
    "var_K_target", "induced_EK", "induced_VarK", "prob_w1_gt_0_5",
    "dual_applied", "note"
  )]
  prior_table
}
