# ==============================================================================
# dgp_generate.R — Data Generating Process for the simulation study
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Paper:    "Design-Conditional Prior Elicitation for DP Mixtures"
# Author:   JoonHo Lee
#
# Description:
#   Generates simulated multisite datasets with known cluster structure.
#   Each dataset consists of J sites, K_true clusters, and site-level
#   effect estimates tau_hat_j with known standard errors.
#
# Functions:
#   sample_clusters_exact()      — Assign J sites to K_true clusters
#   generate_tau_true()          — Generate true site effects from cluster means
#   geometric_mean()             — Geometric mean (shared with calibrate_*)
#   compute_informativeness()    — Compute I from tau variance and se^2 vector
#   generate_dataset()           — Main entry: generate one complete dataset
#
# Dependencies: base R
# Called by:    sim_runner.R, sim_runner_parallel.R
# ==============================================================================

sample_clusters_exact <- function(J, K_true, weights = "equal", seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  stopifnot(J >= K_true, K_true >= 1)

  z <- integer(J)
  z[1:K_true] <- seq_len(K_true)  # Guarantee all K_true clusters occupied

  if (J > K_true) {
    if (weights == "equal") {
      z[(K_true + 1):J] <- sample(seq_len(K_true), size = J - K_true, replace = TRUE)
    } else if (weights == "dirichlet") {
      w <- rgamma(K_true, shape = 1, rate = 1)
      w <- w / sum(w)
      z[(K_true + 1):J] <- sample(seq_len(K_true), size = J - K_true, replace = TRUE, prob = w)
    } else {
      stop("weights must be 'equal' or 'dirichlet'")
    }
  }

  sample(z, size = J, replace = FALSE)  # Shuffle to remove ordering artifact
}

generate_tau_true <- function(z, K_true, sigma_tau = 0.25,
                              within_sd_raw = 0.15, cluster_span_d = 2.5,
                              seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Evenly spaced cluster means across [-cluster_span_d, +cluster_span_d]
  mu_k <- seq(-cluster_span_d, cluster_span_d, length.out = K_true)
  tau_raw <- mu_k[z] + rnorm(length(z), mean = 0, sd = within_sd_raw)

  # Rescale to exact sigma_tau
  tau_centered <- tau_raw - mean(tau_raw)
  tau_sd <- sd(tau_centered)
  if (tau_sd <= 0) stop("Degenerate tau_sd in generate_tau_true()")

  sigma_tau * tau_centered / tau_sd
}

geometric_mean <- function(x) {
  stopifnot(all(x > 0))
  exp(mean(log(x)))
}

compute_informativeness <- function(sigma_tau2, se2_vec) {
  gm <- geometric_mean(se2_vec)
  sigma_tau2 / (sigma_tau2 + gm)
}

generate_dataset <- function(J, K_true, I_target, n_mean,
                             sigma_tau = 0.25, within_sd_raw = 0.15,
                             cluster_span_d = 2.5,
                             n_cv = 0.5, n_min = 5, kappa = 4,
                             weights = "equal",
                             seed = 1L) {
  set.seed(seed)

  # (1) Cluster assignments
  z_true <- sample_clusters_exact(J, K_true, weights = weights, seed = seed + 11)

  # (2) True site effects
  tau_true <- generate_tau_true(
    z_true, K_true,
    sigma_tau = sigma_tau,
    within_sd_raw = within_sd_raw,
    cluster_span_d = cluster_span_d,
    seed = seed + 23
  )

  # (3) Site-level sample sizes from Gamma(shape, rate) truncated at n_min
  shape <- 1 / (n_cv^2)
  rate  <- shape / n_mean
  n_raw   <- rgamma(J, shape = shape, rate = rate)
  n_trunc <- pmax(n_raw, n_min)
  n_j     <- as.integer(round(n_trunc))
  n_j[n_j < n_min] <- n_min

  # (4) Standard errors and observed effects
  se2     <- kappa / n_j          # se^2 = kappa / n_j
  se      <- sqrt(se2)
  tau_hat <- rnorm(J, mean = tau_true, sd = se)

  I_realized <- compute_informativeness(sigma_tau^2, se2)

  df <- data.frame(
    site       = seq_len(J),
    tau_hat    = tau_hat,
    se2        = se2,
    se         = se,
    n_j        = n_j,
    tau_true   = tau_true,
    z_true     = z_true,
    K_true     = K_true,
    I_target   = I_target,
    I_realized = I_realized,
    stringsAsFactors = FALSE
  )

  meta <- list(
    J = J, K_true = K_true, I_target = I_target,
    I_realized = I_realized, n_mean = n_mean, seed = seed
  )

  list(df = df, meta = meta)
}
