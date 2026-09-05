# Author: JoonHo Lee (jlee296@ua.edu)
# Translate a frozen design row into synthetic data
#
# Extract only the fields that define the data-generating process, then verify
# its identity before generation. Prior choices for fitting must not change
# which synthetic dataset is used.
#
# Called by refit/simulation/run_one_fit.R.
# Inputs: one manifest row and the simulation policy; returns a generated
# study.

# Translate a frozen row into the exact fields required by the
# generating-study identity.
sim_public_record <- function(row, policy) {
  d <- policy$dgp
  list(
    schema_version = 'simulation-dgp-key/1', study_id = policy$study_id,
    J = as.integer(row$J),
    K_true = as.integer(row$K_true),
    weight_scheme = as.character(row$weight_scheme),
    dominant_share = as.numeric(row$dominant_share),
    dominant_location_mode = as.character(row$dominant_location_mode),
    I_target = as.numeric(row$I_target),
    n_mean = as.numeric(row$n_mean),
    replicate = as.integer(row$replicate),
    sigma_tau = as.numeric(d$sigma_tau), within_sd_raw = as.numeric(d$within_sd_raw),
    cluster_span_d = as.numeric(d$cluster_span_d),
    n_cv = as.numeric(d$n_cv),
    n_min = as.integer(d$n_min),
    kappa = as.numeric(d$kappa), dgp_version = as.character(d$version)
  )
}

# Verify the regenerated identity and draw the dataset using its recorded DGP
# seed.
sim_public_data <- function(row, policy) {
  record <- sim_public_record(row, policy)
  id <- simv2_dgp_identity(record)
  if (!identical(id$sha256, row$dgp_sha256)) stop('Production DGP identity differs; do not fit.', call. = FALSE)
  simv2_generate_dataset(record, dgp_seed = row$dgp_seed)
}
