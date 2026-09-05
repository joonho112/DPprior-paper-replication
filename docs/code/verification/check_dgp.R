#!/usr/bin/env Rscript
# Author: JoonHo Lee (jlee296@ua.edu)
# Check the synthetic data-generating rules
#
# Exercise exact allocations, deterministic seeds and dominant-cluster
# placement across the specified designs. These checks target generating
# assumptions rather than Monte Carlo posterior results.
#
# Run: Rscript verification/check_dgp.R
# Reads the frozen manifest and generation policy; writes a check receipt.

# Project setup -------------------------------------------------------------

.file_arg <- grep('^--file=', commandArgs(FALSE), value = TRUE)
.script_dir <- dirname(normalizePath(gsub('~+~', ' ', sub('^--file=', '', .file_arg[1]), fixed = TRUE),
  winslash = '/',
  mustWork = TRUE
))
source(file.path(.script_dir, '..', 'common', 'R', 'bootstrap.R'))
source(rp_path('simulation', 'inputs.R', must_exist = TRUE))
for (file in c('keys.R', 'dgp.R', 'prepare.R')) source(rp_path('refit', 'simulation', file, must_exist = TRUE))
x <- sim_read_inputs()$manifest
policy <- yaml::read_yaml(rp_path('refit', 'simulation', 'policy.yml', must_exist = TRUE))
# Cover every distinct allocation/design-size/informativeness setting once;
# different priors and grand-mean branches share the same data by construction.
x <- x[!duplicated(x[c(
  'J',
  'K_true',
  'weight_scheme',
  'dominant_share',
  'dominant_location_mode',
  'I_target'
)]), ]

# Generate each distinct design and check its realized allocation
checks <- lapply(seq_len(nrow(x)), function(i) {
  row <- x[i, ]
  d <- sim_public_data(row, policy)
  stopifnot(
    nrow(d$df) == row$J, length(unique(d$df$z_true)) == row$K_true,
    abs(mean(d$df$tau_true)) < 1e-12, abs(sd(d$df$tau_true) - .25) < 1e-12,
    all(abs(d$df$se2 - 4 / d$df$n_j) < 1e-12)
  )
  if (row$weight_scheme == 'dominant_exact') stopifnot(max(table(d$df$z_true)) / row$J == .60)
  else stopifnot(diff(range(table(d$df$z_true))) <= 1)
  data.frame(
    J = row$J, K_true = row$K_true, weight_scheme = row$weight_scheme,
    dominant_location_mode = row$dominant_location_mode, I_target = row$I_target,
    realized_informativeness = d$meta$I_realized,
    largest_allocation_share = max(table(d$df$z_true)) / row$J,
    dgp_sha256 = d$meta$dgp_sha256, passed = TRUE
  )
})
checks <- do.call(rbind, checks)

# Save the design checks
rp_write_csv(checks, rp_output_path('outputs', 'verification', 'dgp_checks.csv'))
cat(nrow(checks), 'distinct DGP settings generated and checked; no MCMC.\n')
