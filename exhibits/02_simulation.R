# Author: JoonHo Lee (jlee296@ua.edu)
# Build the simulation tables and figures
#
# Use fit-level metrics for distributions and condition-level summaries for
# means and trajectories. Every display keeps the intended condition set and
# retained-fit denominators; the completed-trajectory OSM table differs from
# the full audit grid.
#
# Called by exhibits/00_build_all.R.
# Reads: data-derived/simulation/ and selected frozen design records.
# Builds OSM Figures F.1–F.4 and Tables F.1–F.5 and F.7–F.11.

source(rp_path('exhibits', 'helpers.R'))

# OSM Figure F.4: distributions across retained fits
primary <- ex_read('simulation', 'headline_primary.csv')
fits <- ex_read('simulation', 'headline_fits.csv')
fits$role <- ex_role(fits$prior)
fits$K_label <- factor(paste('K* =', fits$K_true),
  levels = paste('K* =', sort(unique(fits$K_true)))
)
fits$I_label <- paste('I =', fits$I_target)
p <- ggplot(fits, aes(role, E_K, fill = role)) +
  geom_hline(aes(yintercept = K_true),
    data = unique(fits[c('K_label', 'K_true')]),
    linetype = 2
  ) +
  geom_boxplot(width = .65, outlier.size = .5, linewidth = .3) +
  facet_grid(K_label ~ I_label, scales = 'free_y') +
  scale_fill_manual(values = palette_roles) +
  labs(x = NULL, y = 'Posterior mean cluster count') +
  theme_v3(legend = 'none') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9))
ex_figure('osm-figure-F.4',
  p,
  fits,
  'data-derived/simulation/headline_fits.csv',
  height = 7,
  note = 'N(0,1) grand-mean prior. Boxes use 8,607 converged fits from 9,000 attempts; exact cell denominators are in OSM F.5. Vague denotes Gamma(1,1). DORO-Unif is an external reference.'
)

# OSM Figure F.2: compare complete count-misspecification trajectories
m <- ex_read('simulation', 'misspecification_medium_balanced.csv')
stopifnot(nrow(m) == 30L)
m$role <- ex_role(m$prior)
m$K_label <- factor(paste('K* =', m$K_true),
  levels = paste('K* =', sort(unique(m$K_true)))
)
m$I_label <- paste('I =', m$I_target)
p <- ggplot(m, aes(r, mean_E_K, colour = role, shape = role, linetype = role)) +
  geom_hline(aes(yintercept = K_true),
    data = unique(m[c('K_label', 'K_true')]),
    linetype = 2,
    inherit.aes = FALSE
  ) +
  geom_line() +
  geom_point(size = 2) +
  geom_line(aes(y = r * K_true, group = K_label), colour = 'grey70', linetype = 3) +
  facet_grid(K_label ~ I_label, scales = 'free_y') +
  scale_x_continuous(trans = 'log2', breaks = c(.5, 1, 2)) +
  scale_colour_manual(values = palette_roles) +
  labs(
    x = 'Stated mean / generating count',
    y = 'Posterior mean count',
    colour = NULL,
    shape = NULL,
    linetype = NULL
  ) +
  theme_v3()
ex_figure('osm-figure-F.2',
  p,
  m,
  'data-derived/simulation/misspecification_medium_balanced.csv',
  height = 6,
  note = 'Only complete three-point trajectories at VIF=2.5 are drawn. The missing Dual-Anchor K*=5 trajectory is a calibration exclusion, not a zero-valued result. Grand-mean variance is 100.'
)

# OSM Figure F.3: compare complete study-size trajectories
ds <- ex_read('simulation', 'design_size_balanced.csv')
ds <- ds[!(ds$prior == 'DORO-Unif' & is.finite(ds$r) & ds$r != 1), ]
stopifnot(nrow(ds) == 80L)
ds$role <- ex_role(ds$prior)
ds$K_label <- factor(paste('K* =', ds$K_true),
  levels = paste('K* =', sort(unique(ds$K_true)))
)
ds$I_label <- paste('I =', ds$I_target)
p <- ggplot(ds, aes(J, mean_E_K, colour = role, shape = role, linetype = role)) +
  geom_hline(aes(yintercept = K_true),
    data = unique(ds[c('K_label', 'K_true')]),
    linetype = 2,
    inherit.aes = FALSE
  ) +
  geom_line() +
  geom_point(size = 2) +
  facet_grid(K_label ~ I_label, scales = 'free_y') +
  scale_x_continuous(trans = 'log2', breaks = c(25, 50, 100, 200)) +
  scale_colour_manual(values = palette_roles) +
  labs(
    x = 'Number of units J (log scale)',
    y = 'Posterior mean count',
    colour = NULL,
    shape = NULL,
    linetype = NULL
  ) +
  theme_v3()
ex_figure('osm-figure-F.3',
  p,
  ds,
  'data-derived/simulation/design_size_balanced.csv',
  height = 5.8,
  note = 'Complete J trajectories under grand-mean variance 100. High-informativeness J=200 cells retain 30–93 of 200 fits. Vague denotes Gamma(1,1). DORO-Unif is an external reference.'
)
# Paired changes: right minus left, using joint convergence and common DGP seeds.

# OSM Figure F.1: pair the same generated studies across grand-mean priors
paired <- ex_read('simulation', 'headline_paired.csv')
paired <- paired[paired$metric == 'E_K', ]
stopifnot(nrow(paired) == 135L, min(paired$n_paired) >= 111)
paired$contrast <- paste(paired$right_grand_mean, 'minus', paired$left_grand_mean)
paired$role <- ex_role(paired$prior)
pos <- position_dodge(width = .65)
p <- ggplot(
  paired,
  aes(role, mean_delta, colour = factor(I_target), shape = factor(I_target))
) +
  geom_hline(yintercept = 0, colour = 'grey60') +
  geom_linerange(aes(ymin = q025_delta, ymax = q975_delta), position = pos) +
  geom_point(position = pos, size = 2) +
  facet_grid(contrast ~ K_true, scales = 'free_y') +
  labs(
    x = NULL,
    y = 'Paired difference in posterior mean count',
    colour = 'Informativeness',
    shape = 'Informativeness'
  ) +
  theme_v3() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ex_figure('osm-figure-F.1',
  p,
  paired,
  'data-derived/simulation/headline_paired.csv',
  width = 9,
  height = 8,
  note = 'Each contrast includes 45 conditions with at least 111 jointly converged replications per condition. Whiskers give the empirical 2.5th and 97.5th percentiles of the paired differences.'
)
# Planned suites are recovered from batch/design membership, not hard-coded counts.

# OSM F.1-F.3: recover the planned suites and generating inputs
design <- ex_read('simulation', 'design.csv', 'data-frozen')
status <- ex_read('simulation', 'fit_status.csv', 'data-frozen')
suite <- merge(unique(status[c('design_id', 'batch')]),
  design[c('design_id', 'suite')],
  by = 'design_id'
)
stopifnot(nrow(suite) == 322)
suite$display <- ifelse(suite$batch == 'extension',
  'Additional cells',
  ifelse(suite$suite == 'fallback',
    'Initial set',
    ifelse(suite$suite == 'headline_n001',
      'Matched N(0,.01)',
      ifelse(suite$suite == 'headline_n01', 'Main N(0,1)', 'Central dominant')
    )
  )
)
t <- aggregate(design_id ~ display, suite, length)
names(t) <- c('Suite', 'Cells')
t$Fits <- t$Cells * 200
purposes <- c(
  'Initial set' = 'Prespecified sensitivity and design-size conditions under N(0,100).',
  'Matched N(0,.01)' = 'Matched conditions for the grand-mean prior comparison.',
  'Main N(0,1)' = 'Primary conditions, including the 15 low-information cells.',
  'Central dominant' = 'Location sensitivity with a 60% component.',
  'Additional cells' = 'Additional misspecification, design-size and location conditions.'
)
t <- t[match(names(purposes), t$Suite), ]
t$Purpose <- unname(purposes[t$Suite])
display_suite <- rbind(t, data.frame(Suite = 'Total', Cells = sum(t$Cells),
                                    Fits = sum(t$Fits), Purpose = 'All planned conditions.'))
display_suite <- display_suite[c('Suite', 'Purpose', 'Cells', 'Fits')]
ex_table('osm-table-F.1',
  t,
  display_suite,
  sources = c(
    'data-frozen/simulation/design.csv',
    'data-frozen/simulation/fit_status.csv'
  ),
  note = 'All 322 planned conditions and 64,400 planned fits; a planned fit can be excluded before MCMC.'
)
policy <- yaml::read_yaml(rp_path('refit', 'simulation', 'policy.yml'))
dgp <- data.frame(
  Parameter = c(
    'J',
    'Between-site SD',
    'Within-component SD',
    'Separation span',
    'SE variance scale',
    'Site-size CV',
    'Minimum size',
    'Allocation'
  ),
  Value = c(
    paste(sort(unique(design$J)), collapse = ', '),
    policy$dgp$sigma_tau, policy$dgp$within_sd_raw, policy$dgp$cluster_span_d,
    policy$dgp$kappa, policy$dgp$n_cv, policy$dgp$n_min, 'Exact integer counts'
  ),
  Role = c(
    'Main comparison uses 100 sites. Design-size comparisons vary J.',
    'Sample SD of the generated site effects, using denominator J-1.',
    'Controls overlap among components.',
    'Component means span the interval from -d to d.',
    'Site sampling variance equals this scale divided by site size.',
    'Coefficient of variation before applying the minimum and rounding.',
    'Lower bound on site size.',
    sprintf('Equal or exactly %.0f%% dominant allocation.', 100 * policy$dgp$dominant_share)
  )
)
ex_table('osm-table-F.2',
  dgp,
  sources = c(
    'data-frozen/simulation/design.csv',
    'refit/simulation/dgp.R',
    'refit/simulation/policy.yml'
  ),
  tier = 'design-specification',
  note = 'Constants are implemented in the preserved DGP and checked by verification/check_dgp.R.'
)
i <- unique(design[c('I_target', 'n_mean')])
i <- i[order(i$I_target), ]
i$Status <- 'Frozen input'
ex_table('osm-table-F.3',
  i,
  sources = 'data-frozen/simulation/design.csv',
  tier = 'frozen-design-input',
  note = 'Frozen informativeness calibration inputs. The package verifies their use in the DGP; it does not rerun the historical Monte Carlo search for these values.'
)
# J=100 priors: compact table shows calibrated roles at means 5,10,30 and
# the three external DORO references actually used by the simulation.

# OSM F.4: show the exact prior parameters used for fitting
cal <- ex_read('calibration', 'simulation_J100.csv', 'data-frozen')
cal <- cal[(cal$prior_label %in% c('Vague', 'SSI-Weight')) |
  (cal$prior_label == 'TSMM' & cal$mu_K %in% c(5, 10, 30) & cal$VIF %in% c(1.5, 2.5, 5)) |
  (cal$prior_label == 'Dual-Soft-Selected' & cal$mu_K == 5 & cal$VIF %in% c(1.5, 2.5, 5)), ]
cal <- cal[!duplicated(cal[c('prior_label', 'mu_K', 'VIF', 'a', 'b')]), ]
keep <- c(
  'prior_label',
  'VIF',
  'mu_K',
  'var_K',
  'a',
  'b',
  'prior_EK',
  'p_wsb_gt_05',
  'selected_lambda'
)
cal <- cal[keep]
f <- merge(status[status$replicate == 1, ], design, by = 'design_id')
f <- f[f$J == 100 & f$prior == 'DORO-Unif' & f$r == 1 & f$K_true %in% c(5, 10, 30), ]
f <- f[!duplicated(f[c('K_true', 'a', 'b')]), ]
for (k in seq_len(nrow(f))) cal <- rbind(
  cal,
  data.frame(
    prior_label = 'DORO-Unif',
    VIF = NA,
    mu_K = f$K_true[k],
    var_K = NA,
    a = f$a[k],
    b = f$b[k],
    prior_EK = ex_moments(100, f$a[k], f$b[k])[['mean']],
    p_wsb_gt_05 = ex_tail(f$a[k], f$b[k], .5),
    selected_lambda = NA
  )
)
cal$prior_label[cal$prior_label == 'Dual-Soft-Selected'] <- 'Dual-Anchor'
stopifnot(nrow(cal) == 17L)
ex_table('osm-table-F.4',
  cal,
  sources = c(
    'data-frozen/calibration/simulation_J100.csv',
    'data-frozen/simulation/fit_status.csv',
    'data-frozen/simulation/design.csv'
  ),
  note = 'DORO-Unif is an external reference. At K*=30 the original projection shape rounds to 0.943 in the manuscript, and the independently optimized shape actually used by the fits rounds to 0.942. The accompanying CSV reports the fitting parameters. See simulation/exhibits/doro-projection-reference.csv for both projections.'
)

# OSM F.5: account for all planned fits and main-cell denominators
counts <- as.data.frame(table(status$status_category))
names(counts) <- c('Disposition', 'Fits')
counts <- rbind(data.frame(Disposition = 'Planned', Fits = nrow(status)), counts)
# Two distinct panels are delivered together in one long frame with explicit panel labels.
acct <- data.frame(
  panel = 'All fits',
  K_true = NA,
  I_target = NA,
  prior = counts$Disposition,
  retained = counts$Fits,
  planned = NA
)
acct <- rbind(
  acct,
  data.frame(
    panel = 'Main N(0,1)',
    K_true = primary$K_true,
    I_target = primary$I_target,
    prior = primary$prior,
    retained = primary$n_scientific_eligible,
    planned = primary$n_manifest
  )
)
ex_table('osm-table-F.5',
  acct,
  sources = c(
    'data-frozen/simulation/fit_status.csv',
    'data-derived/simulation/headline_primary.csv'
  ),
  note = 'All-fit dispositions and exact 45-cell main denominators are distinct panels. Main retained total is 8,607 of 9,000.'
)

# OSM F.7: use the 36 printed completed-trajectory rows
m <- ex_read('simulation', 'misspecification_balanced.csv')
stopifnot(nrow(m) == 36L)
m <- m[order(ex_role(m$prior), m$K_true, m$I_target, m$r), ]
cols <- c(
  'prior',
  'K_true',
  'I_target',
  'VIF',
  'r',
  'effective_lambda',
  'n_scientific_eligible',
  'mean_E_K',
  'mean_p_K1',
  'mean_tau_rmse',
  'coverage95_K'
)
ex_table('osm-table-F.7',
  m,
  transform(m[cols], retained_out_of_planned = paste0(m$n_scientific_eligible, '/', m$n_manifest)),
  sources = 'data-derived/simulation/misspecification_balanced.csv',
  note = 'The 36 completed cells in the current OSM: TSMM/Dual-Anchor at VIF=2.5 and the six displayed Dual-Anchor VIF=5 cells. The full medium-confidence audit contains 60 cells, 56 reportable; see data-derived/simulation/misspecification_medium.csv for that separate population.'
)

# OSM F.8-F.9: place study sizes in columns while retaining each denominator
wide_ds <- function(metric) {
  key <- unique(ds[c('prior', 'K_true', 'I_target')])
  key <- key[order(ex_role(key$prior), key$K_true, key$I_target), ]
  for (j in c(25, 50, 100, 200)) {
    z <- ds[ds$J == j, c('prior', 'K_true', 'I_target', metric, 'n_scientific_eligible')]
    z$value <- paste0(
      ex_f(z[[metric]], if (metric == 'mean_E_K') 2 else 3),
      ' (',
      z$n_scientific_eligible,
      ')'
    )
    z <- z[c('prior', 'K_true', 'I_target', 'value')]
    names(z)[4] <- paste0('J=', j)
    key <- merge(key, z, by = c('prior', 'K_true', 'I_target'), all.x = TRUE, sort = FALSE)
  }
  key[order(ex_role(key$prior), key$K_true, key$I_target), ]
}
ex_table('osm-table-F.8',
  ds,
  wide_ds('mean_E_K'),
  sources = 'data-derived/simulation/design_size_balanced.csv',
  note = 'Posterior mean count, with admitted n in parentheses. Each denominator is 200; 20 prior/K*/I trajectories are shown.'
)
ex_table('osm-table-F.9',
  ds,
  wide_ds('mean_p_K1'),
  sources = 'data-derived/simulation/design_size_balanced.csv',
  note = 'Posterior single-cluster probability, with admitted n in parentheses. Each denominator is 200.'
)

# OSM F.10-F.11: display allocation and fixed-lambda sensitivities
u <- ex_read('simulation', 'unequal_weights.csv')
ex_table('osm-table-F.10',
  u,
  u[c(
    'prior',
    'K_true',
    'I_target',
    'dominant_location_mode',
    'n_scientific_eligible',
    'mean_E_K',
    'mean_p_K1'
  )],
  sources = 'data-derived/simulation/unequal_weights.csv',
  note = 'Exactly 60% dominant allocation; balanced random and central locations are separate rows for each condition.'
)
l <- ex_read('simulation', 'fixed_lambda.csv')
ex_table('osm-table-F.11',
  l,
  l[c(
    'I_target',
    'effective_lambda',
    'prior',
    'n_scientific_eligible',
    'mean_E_K',
    'mean_p_K1',
    'mean_tau_rmse'
  )],
  sources = 'data-derived/simulation/fixed_lambda.csv',
  note = 'Selected lambda .16 versus fixed .50 and .90; comparisons use the declared original cells.'
)
