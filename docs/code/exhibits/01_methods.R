# Author: JoonHo Lee (jlee296@ua.edu)
# Build the methods illustrations
#
# Connect count and population-weight judgments to the worked examples.
# Recalculate the closed-form illustration and catalogue moment checks; use
# the previously verified calibration summaries for the remaining displays.
#
# Called by exhibits/00_build_all.R after bootstrap.
# Builds: main Tables 1-2, Figures 1-3 and OSM Table/Figure G.1.
# No optimizer or posterior sampler runs in this module.

source(rp_path('exhibits', 'helpers.R'))

# Table 1: connect each quantity with the elicitation question
t <- data.frame(
  Quantity = c(
    'E(K_J), Var(K_J)',
    'P(K_J = k)',
    'P(W_SB > .5), P(W_SB > .9)',
    'P(W_max > t)',
    'rho = sum(w_h^2)'
  ),
  Question = c(
    'How many patterns among these J units, and how certain?',
    'What is the chance of exactly k patterns?',
    'Could a randomly selected unit belong to a majority-mass or near-universal-mass population cluster?',
    'Could the largest population cluster exceed mass t?',
    'Will two randomly selected units share a pattern?'
  ),
  Report = c(
    'Expected count and uncertainty interval',
    'Mode and central interval',
    'Probability for the size-biased weight',
    'Bounds or exact probability',
    'Probability of same-cluster membership'
  ),
  check.names = FALSE
)
ex_table('main-table-1',
  t,
  sources = 'provenance/method-contract.csv',
  tier = 'conceptual-specification',
  note = 'Count summaries depend on the number J of units. The population weight functionals answer different questions.'
)

# Table 2: display the verified confidence calibrations
confidence <- ex_read('calibration', 'confidence_levels.csv', 'data-frozen')
confidence$mean_K <- vapply(
  seq_len(nrow(confidence)),
  function(i) ex_moments(100, confidence$a[i], confidence$b[i])[['mean']],
  numeric(1)
)
ex_table('main-table-2', confidence,
  data.frame(
    Setting = confidence$setting,
    Variance = confidence$target_variance,
    'Gamma (shape, rate)' = paste0('(', ex_f(confidence$a, 2), ', ', ex_f(confidence$b, 2), ')'),
    'E(alpha)' = ex_f(confidence$mean_alpha, 2),
    'P(W_SB > .5)' = ex_f(confidence$tail_05, 2),
    'P(W_SB > .9)' = ex_f(confidence$tail_09, 2),
    'P(alpha < .1)' = ex_f(confidence$p_alpha_lt_01, 3),
    check.names = FALSE
  ),
  sources = 'data-frozen/calibration/confidence_levels.csv',
  tier = 'rebuilt-from-verified-calibration',
  note = 'Full-precision calibration can be regenerated with calibration/05_confidence_levels.R. For VIF 1.25 the near-universal tail is .144969, displayed here as .14. The locked manuscript prints .15; see provenance/manuscript-discrepancies.csv.'
)
# Closed-form weight density and exact finite-J count law mixed over Gamma.

# Figure 1: calculate the count law and size-biased weight density
J <- 50L
a <- 1.60
b <- 1.22

# Store unsigned Stirling numbers on the log scale. The recursion and
# log-sum-exp calculation avoid overflow before mixing the count law over
# alpha.
ls <- rep(-Inf, J + 1)
ls[2] <- 0
for (n in 2:J) {
  next_ls <- rep(-Inf, J + 1)
  for (k in 1:n) {
    v <- c(if (k < n) log(n - 1) + ls[k + 1] else -Inf, if (k > 1) ls[k] else -Inf)
    m <- max(v)
    next_ls[k + 1] <- if (is.finite(m)) m + log(sum(exp(v - m))) else -Inf
  }
  ls <- next_ls
}

# Integrate against the Gamma density, then normalize the finite-J PMF.
lo <- qgamma(1e-13, a, rate = b)
hi <- qgamma(1 - 1e-13, a, rate = b)
pmf <- vapply(
  1:J,
  function(k) integrate(function(x) exp(ls[k + 1] + k * log(x) + lgamma(x) - lgamma(x + J)) * dgamma(x, a, rate = b),
    lo,
    hi,
    rel.tol = 1e-9,
    subdivisions = 2000
  )$value,
  numeric(1)
)
pmf <- pmf / sum(pmf)

# The dashed comparison is a shifted negative binomial matched to the induced
# count moments. It is illustrative, not a newly elicited target.
mo <- ex_moments(J, a, b)
nbmean <- mo[['mean']] - 1
target <- dnbinom(0:49, mu = nbmean, size = nbmean^2 / (mo[['var']] - nbmean))
target <- target / sum(target)
x <- data.frame(K = 1:J, induced = pmf, target = target)
w <- seq(.001, .999, length.out = 500)
density <- a * b^a / ((1 - w) * (b - log1p(-w))^(a + 1))
p1 <- ggplot(x[x$K <= 15, ], aes(K, induced)) +
  geom_col(fill = '#4E79A7', width = .75) +
  geom_line(aes(y = target), linetype = 2) +
  geom_point(aes(y = target), shape = 21, fill = 'white') +
  labs(title = 'A. Count distribution', y = 'Probability', x = 'Number of clusters') +
  theme_v3(legend = 'none')
y <- data.frame(w, density)
p2 <- ggplot(y, aes(w, density)) +
  geom_area(data = y[y$w >= .5, ], fill = '#FDAE6B') +
  geom_area(data = y[y$w >= .9, ], fill = '#D94701') +
  geom_line(colour = '#4E79A7') +
  labs(title = 'B. Size-biased weight', x = 'Population weight', y = 'Density') +
  theme_v3(legend = 'none')
frame <- rbind(
  data.frame(panel = 'count', x = x$K, value = x$induced, target = x$target),
  data.frame(panel = 'weight', x = w, value = density, target = NA)
)
ref <- ex_read('calibration', 'unintended_prior.csv', 'data-frozen')
stopifnot(
  max(abs(pmf - ref$induced[ref$panel == 'A'])) < 1e-7,
  max(abs(target - ref$target[ref$panel == 'A'])) < 1e-7
)
ex_figure('main-figure-1',
  p1 + p2,
  frame,
  'data-frozen/calibration/unintended_prior.csv',
  tier = 'recomputed-closed-forms',
  note = 'Gamma(1.60,1.22) is the pinned illustration; it is not the J=50 exact calibration. Target is a shifted negative binomial using the induced moments.'
)
# Recheck each catalogue moment and tail; retain original discrete quantiles.

# Figure 2: compare defaults across the number of study units
d <- ex_read('calibration', 'defaults_by_J.csv', 'data-frozen')
for (i in seq_len(nrow(d))) {
  if (d$family[i] == 'gamma' && is.finite(d$a[i]) && is.finite(d$b[i])) {
    mo <- ex_moments(d$J[i], d$a[i], d$b[i])
    stopifnot(
      abs(mo[['mean']] - d$EK[i]) < 1e-5,
      abs(ex_tail(d$a[i], d$b[i], .5) - d$wsb_tail_50[i]) < 1e-9
    )
  }
}
d <- d[is.finite(d$EK), ]
d$legend_label[d$family == 'point_mass'] <- 'alpha = 1'
d$legend_label[d$curve == 'tsmm_transported'] <- 'TSMM at J=500'
d$legend_label[d$curve == 'tsmm_recalibrated'] <- 'TSMM recalibrated'
d$group <- interaction(d$legend_label, d$curve, drop = TRUE)
a <- ggplot(d, aes(J, EK, colour = legend_label, linetype = legend_label, group = group)) +
  geom_line() +
  scale_x_log10(breaks = c(10, 50, 100, 500, 5000)) +
  labs(
    title = 'A. Expected cluster count',
    x = 'Number of units J (log scale)',
    y = 'Prior mean count',
    colour = NULL,
    linetype = NULL
  ) +
  theme_v3()
b <- ggplot(
  d,
  aes(J, wsb_tail_50, colour = legend_label, linetype = legend_label, group = group)
) +
  geom_line() +
  scale_x_log10(breaks = c(10, 50, 100, 500, 5000)) +
  labs(
    title = 'B. Majority probability',
    x = 'Number of units J (log scale)',
    y = 'P(W_SB > .5)',
    colour = NULL,
    linetype = NULL
  ) +
  theme_v3()
transport <- d[d$curve == 'tsmm_transported' & is.finite(d$q05), ]
if (nrow(transport)) a <- a +
  geom_ribbon(
    data = transport,
    aes(ymin = q05, ymax = q95),
    alpha = .12,
    fill = 'grey',
    colour = NA,
    show.legend = FALSE
  )
a <- a + guides(colour = 'none', linetype = 'none')
ex_figure('main-figure-2',
  ((a + b) + plot_layout(guides = 'collect')) & theme(legend.position = 'bottom') &
    guides(colour = guide_legend(nrow = 3), linetype = guide_legend(nrow = 3)),
  d,
  'data-frozen/calibration/defaults_by_J.csv',
  height = 5.6,
  note = 'Catalogue moments/tails independently rechecked; calibration parameters and discrete quantiles are frozen. Quantiles stop at J=500. Gamma(2,4) and Gamma(1,3) are Paganin et al. (2023) settings.'
)

# Figure 3: display the six elicitation decisions
steps <- data.frame(
  step = 1:6,
  label = c(
    'Define the J units',
    'State count judgments',
    'Convert to count moments',
    'Calibrate the Gamma prior',
    'Check size-biased majority probability',
    'Retain TSMM or select Dual-Anchor'
  )
)
p <- ggplot(steps, aes(1, 7 - step)) +
  geom_label(aes(label = paste(step, label, sep = '. ')),
    size = 4,
    linewidth = .3,
    label.padding = grid::unit(.35, 'lines')
  ) +
  geom_segment(
    data = steps[1:5, ],
    aes(x = 1, xend = 1, y = 6.7 - step, yend = 6.3 - step),
    arrow = grid::arrow(length = grid::unit(.12, 'inches'))
  ) +
  coord_cartesian(xlim = c(0, 2), ylim = c(.5, 6.5), clip = 'off') +
  theme_void()
ex_figure('main-figure-3',
  p,
  steps,
  c('calibration/policy.yml', 'provenance/method-contract.csv'),
  height = 6,
  tier = 'conceptual-specification',
  note = 'Six-step workflow. Tail <= .40 retains the count solution; otherwise select the largest eligible lambda. The hard .25 bound is a separate sensitivity.'
)

# OSM G.1: show the worked prior comparison and full lambda grid
g <- ex_read('calibration', 'dual_anchor_grid.csv', 'data-frozen')
c <- ex_read('calibration', 'count_calibration.csv', 'data-frozen')
h <- ex_read('calibration', 'hard_sensitivity.csv', 'data-frozen')
c <- c[c$accepted_for_analysis, ]
sel <- g[g$selected, ]
worked <- data.frame(
  Prior = c('TSMM', 'Dual-Anchor', 'Dual-Anchor (hard)'),
  a = c(c$a, sel$a, h$a),
  b = c(c$b, sel$b, h$b),
  E_K = c(c$mean_K, sel$mu_K, h$prior_EK),
  Var_K = c(c$variance_K, sel$var_K, h$prior_varK),
  tail = c(ex_tail(c$a, c$b, .5), sel$achieved_weight, h$p_wsb_gt_05)
)
ex_table('osm-table-G.1',
  worked,
  sources = c(
    'data-frozen/calibration/count_calibration.csv',
    'data-frozen/calibration/dual_anchor_grid.csv',
    'data-frozen/calibration/hard_sensitivity.csv'
  ),
  tier = 'rebuilt-from-verified-calibration',
  note = 'J=50; count mean 5 and variance 10. Optional calibration scripts regenerate these inputs.'
)
g$state <- ifelse(g$selected,
  'Selected',
  ifelse(g$soft_policy_eligible, 'Eligible', 'Above trigger')
)
p <- ggplot(g, aes(lambda, achieved_weight)) +
  geom_hline(yintercept = c(.25, .40), linetype = c(3, 2), colour = 'grey45') +
  geom_line(colour = 'grey60') +
  geom_point(aes(colour = state, shape = state), size = 2.8) +
  scale_colour_manual(values = c('Selected' = '#D55E00', 'Eligible' = '#0072B2', 'Above trigger' = 'grey60')) +
  labs(x = 'Trade-off weight lambda', y = 'P(W_SB > .5)', colour = NULL, shape = NULL) +
  theme_v3()
ex_figure('osm-figure-G.1',
  p,
  g,
  'data-frozen/calibration/dual_anchor_grid.csv',
  note = 'All 29 verified candidates. Lambda=.30 is selected; .40 is the trigger, .25 the separate soft target.'
)
