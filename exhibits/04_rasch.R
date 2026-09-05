# Author: JoonHo Lee (jlee296@ua.edu)
# Build the Rasch count and precision exhibits
#
# Show occupied-group counts in the main figure and keep model-density
# discussion in the supplement. The WLE table uses fixed-item scoring;
# posterior density bands come from the original retained fits.
#
# Called by exhibits/00_build_all.R.
# Builds: main Figure 8, OSM Figure H.1 and Tables H.8-H.13.
# Reads: frozen and reconstructed Rasch aggregates.

source(rp_path('exhibits', 'helpers.R'))

# Figure 8: show the main-arm count distributions
r <- ex_read('rasch', 'posterior_reportable.csv')
r$label <- ex_rasch_role(r)
k <- ex_read('rasch', 'pmf.csv')
k <- k[k$arm == 'main', ]
k$label <- unname(ex_rasch_labels[k$role])

# Trim only the plotting range. The numerical CSV below retains the complete
# count support.
support <- range(c(5, k$K[k$prob > .005]))
shown <- k[k$K >= support[1] & k$K <= support[2], ]
p <- ggplot(shown, aes(K, prob, colour = label, shape = label, linetype = label)) +
  geom_vline(xintercept = 5, linetype = 2, colour = 'grey50') +
  geom_line(linewidth = .65) +
  geom_point(size = 2) +
  scale_colour_manual(values = palette_roles) +
  scale_shape_manual(values = shape_roles) +
  scale_linetype_manual(values = linetype_roles) +
  scale_x_continuous(breaks = seq(support[1], support[2], 1)) +
  labs(
    x = 'Number of latent groups K (500 examinees)',
    y = 'Posterior probability',
    colour = NULL,
    shape = NULL,
    linetype = NULL
  ) +
  theme_v3() +
  guides(
    colour = guide_legend(nrow = 2),
    shape = guide_legend(nrow = 2),
    linetype = guide_legend(nrow = 2)
  )
ex_figure('main-figure-8',
  p,
  k,
  'data-derived/rasch/pmf.csv',
  height = 4.2,
  note = 'Five converged main-arm priors. The dashed line marks the stated count of five. The plotted range includes every count with probability above .005; the CSV retains full support.'
)

# OSM Figure H.1: show population model densities
d <- ex_read('rasch', 'density.csv')
d <- d[d$arm == 'main', ]
d$label <- unname(ex_rasch_labels[d$role])
g <- ex_read('rasch', 'gaussian.csv')
g$label <- 'Gaussian benchmark'
range_x <- range(c(d$x[d$mean > .005], g$x[g$mean > .005]))

# Show uncertainty bands for the two focal priors and Gaussian benchmark. Use
# matching colour and line scales to keep one shared legend.
bands <- d[d$role %in% c('tsmm', 'dasoft'), ]
line_data <- rbind(d[c('x', 'mean', 'label')], g[c('x', 'mean', 'label')])
levels_density <- c(unname(ex_rasch_labels), 'Gaussian benchmark')
line_data$label <- factor(line_data$label, levels = levels_density)
p <- ggplot() +
  geom_ribbon(
    data = g,
    aes(x, ymin = lower, ymax = upper),
    fill = 'grey70',
    alpha = .3,
    show.legend = FALSE
  ) +
  geom_ribbon(
    data = bands,
    aes(x, ymin = lower, ymax = upper, fill = label),
    alpha = .18,
    show.legend = FALSE
  ) +
  geom_line(data = line_data, aes(x, mean, colour = label, linetype = label), linewidth = .65) +
  scale_colour_manual(
    values = c(palette_roles, 'Gaussian benchmark' = 'grey40'),
    breaks = levels_density,
    name = NULL
  ) +
  scale_fill_manual(values = palette_roles, guide = 'none') +
  scale_linetype_manual(
    values = c(linetype_roles, 'Gaussian benchmark' = '22'),
    breaks = levels_density,
    name = NULL
  ) +
  coord_cartesian(xlim = range_x) +
  labs(x = 'Latent ability', y = 'Model density') +
  theme_v3() +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE), linetype = guide_legend(nrow = 2, byrow = TRUE))
ex_figure('osm-figure-H.1',
  p,
  d,
  c('data-derived/rasch/density.csv', 'data-derived/rasch/gaussian.csv'),
  width = 8.5,
  height = 5,
  note = 'Posterior mean fitted population density, with pointwise bands for TSMM, Dual-Anchor and the Gaussian benchmark. This is not the empirical density of person posterior means.'
)

# OSM H.8-H.9: item endorsements and conditional WLE precision
items <- ex_read('rasch', 'item_endorsements.csv', 'data-frozen')
ex_table('osm-table-H.8',
  items,
  items[c('key', 'label', 'excluded', 'full_value', 'sample_value')],
  sources = 'data-frozen/rasch/item_endorsements.csv',
  note = 'Full file N=2,495; sampled N=500. Excluded=1 marks the three nonwords, removed after sampling; 13 real words enter the model.'
)
w <- ex_read('rasch', 'wle_scores.csv')
w <- w[w$role == 'Dual-Anchor', ]
ex_table('osm-table-H.9',
  w,
  w[c('raw_score', 'n', 'wle', 'standard_error')],
  sources = c(
    'data-derived/rasch/wle_scores.csv',
    'data-frozen/rasch/wle_item_difficulties.csv'
  ),
  tier = 'independently-recomputed-WLE',
  note = 'Warm estimates conditional on primary Dual-Anchor posterior mean item difficulties. No item refit. Reliability=.7120824; the 63 perfect-score respondents have SE=1.733086.'
)

# OSM H.10: list the six distinct fitted priors
pr <- ex_read('rasch', 'priors.csv', 'data-frozen')
key <- paste(r$arm, r$role)
pr <- pr[match(key, paste(pr$arm, pr$role)), ]
pr$label <- ex_rasch_role(pr)
ex_table('osm-table-H.10',
  pr,
  pr[c('label', 'a', 'b', 'E_K', 'q05', 'q95', 'Pr_K_2_to_10', 'wsb_gt_05')],
  sources = 'data-frozen/rasch/priors.csv',
  note = 'Six distinct fitted priors. Count 25 is the sensitivity TSMM prior shared by its Dual-Anchor roles. The 2–10 probability evaluates the main judgment, not the Count-25 target. Literature default: Paganin et al. (2023).'
)

# OSM H.11: keep all ten labeled diagnostic outcomes visible
diag <- ex_read('rasch', 'fit_diagnostics.csv', 'data-frozen')
diag$label <- ifelse(diag$arm == 'sens',
  paste0('Sens. ', unname(ex_rasch_labels[diag$role])),
  unname(ex_rasch_labels[diag$role])
)
ex_table('osm-table-H.11',
  diag,
  diag[c(
    'arm',
    'label',
    'attempt',
    'rhat_max_gated',
    'ess_bulk_min_gated',
    'ess_tail_min_gated',
    'gate_pass'
  )],
  sources = 'data-frozen/rasch/fit_diagnostics.csv',
  note = 'All ten final roles remain visible. Four repeated-prior sensitivity fits failed initial diagnostics and are excluded from posterior comparison. Five third-schedule fits are recorded departures from the original two-schedule plan.'
)

# OSM H.12-H.13: summarize counts and distinguish observed from population
# weights
ex_table('osm-table-H.12',
  r,
  r[c(
    'label',
    'E_K',
    'E_K_mcse',
    'q025_K',
    'q975_K',
    'p_K1',
    'p_K1_mcse',
    'alpha_mean',
    'alpha_q025',
    'alpha_q975'
  )],
  sources = 'data-derived/rasch/posterior_reportable.csv',
  note = 'Six distinct converged fits, each using 16,000 draws. MCSE describes simulation uncertainty of each posterior summary; apparent zero probabilities can reflect rounding.'
)
ex_table('osm-table-H.13',
  r,
  r[c(
    'label',
    'observed_largest_share_mean',
    'observed_largest_share_q025',
    'observed_largest_share_q975',
    'population_largest_atom_weight_mean',
    'population_largest_atom_weight_q025',
    'population_largest_atom_weight_q975',
    'p_population_largest_atom_weight_gt_05'
  )],
  sources = 'data-derived/rasch/posterior_reportable.csv',
  note = 'Observed share and population atom weight report means and equal-tail 95% intervals. Population calculations include unoccupied mass.'
)
