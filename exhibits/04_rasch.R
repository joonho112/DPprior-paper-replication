# Author: JoonHo Lee (jlee296@ua.edu)
# Build the Rasch count and precision exhibits
#
# Rebuild the inherited 104 OSM Rasch count Figure H.1 and Tables H.10–15.
# The original model-role codes in the frozen summaries identify the fits.
source(rp_path('exhibits', 'helpers.R'))

# OSM Figure H.1: show the main-arm count distributions
r <- ex_read('rasch', 'posterior_reportable.csv')
rasch_labels <- c(vague = 'Vague', litdef = 'Gamma(1, 3) reference', tsmm = 'TSMM',
                  dasoft = 'Dual-Anchor', dahard = 'Dual-Anchor (hard)')
rasch_label <- function(x) ifelse(x$arm == 'sens', 'Count 25', unname(rasch_labels[x$role]))
r$label <- rasch_label(r)
k <- ex_read('rasch', 'pmf.csv')
k <- k[k$arm == 'main', ]
k$label <- unname(rasch_labels[k$role])

# Trim only the plotting range. The numerical CSV below retains the complete
# count support.
support <- range(c(5, k$K[k$prob > .005]))
shown <- k[k$K >= support[1] & k$K <= support[2], ]
# Keep palette keys aligned with the manuscript's external-reference label.
current_palette <- function(values) {
  names(values)[names(values) == 'Literature default'] <- rasch_labels[['litdef']]
  values
}
p <- ggplot(shown, aes(K, prob, colour = label, shape = label, linetype = label)) +
  geom_vline(xintercept = 5, linetype = 2, colour = 'grey50') +
  geom_line(linewidth = .65) +
  geom_point(size = 2) +
  scale_colour_manual(values = current_palette(palette_roles)) +
  scale_shape_manual(values = current_palette(shape_roles)) +
  scale_linetype_manual(values = current_palette(linetype_roles)) +
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
ex_figure('osm-figure-H.1',
  p,
  k,
  'data-derived/rasch/pmf.csv',
  height = 4.2,
  note = 'Five converged main-arm priors. The dashed line marks the stated count of five. The plotted range includes every count with probability above .005; the CSV retains full support.'
)

# OSM H.10-H.11: item endorsements and conditional WLE precision
items <- ex_read('rasch', 'item_endorsements.csv', 'data-frozen')
ex_table('osm-table-H.10',
  items,
  data.frame(Entry = items$key, Word = items$label,
    Status = ifelse(items$excluded == 1L, 'excluded (non-word)', 'analyzed'),
    'Full file' = sprintf('%.3f', items$full_value), Sample = sprintf('%.3f', items$sample_value),
    check.names = FALSE),
  sources = 'data-frozen/rasch/item_endorsements.csv',
  note = 'Full file N=2,495; sampled N=500. Excluded=1 marks the three nonwords, removed after sampling; 13 real words enter the model.'
)
w <- ex_read('rasch', 'wle_scores.csv')
w <- w[w$role == 'Dual-Anchor', ]
ex_table('osm-table-H.11',
  w,
  w[c('raw_score', 'n', 'wle', 'standard_error')],
  sources = c(
    'data-derived/rasch/wle_scores.csv',
    'data-frozen/rasch/wle_item_difficulties.csv'
  ),
  tier = 'independently-recomputed-WLE',
  note = 'Warm estimates conditional on primary Dual-Anchor posterior mean item difficulties. No item refit. Reliability=.7120824; the 63 perfect-score respondents have SE=1.733086.'
)

# OSM H.12: list the six distinct fitted priors
pr <- ex_read('rasch', 'priors.csv', 'data-frozen')
key <- paste(r$arm, r$role)
pr <- pr[match(key, paste(pr$arm, pr$role)), ]
pr$label <- rasch_label(pr)
parameters <- sprintf('(%.4f, %.4f)', pr$a, pr$b)
integer_priors <- pr$role %in% c('vague','litdef')
parameters[integer_priors] <- sprintf('(%d, %d)', as.integer(pr$a[integer_priors]), as.integer(pr$b[integer_priors]))
parameters[pr$role == 'dahard'] <- 'alpha approximately 2'
prior_display <- data.frame(
  Hyperprior = pr$label, 'Gamma (shape, rate)' = parameters,
  'Expected count [90% interval]' = sprintf('%.2f [%d, %d]', pr$exact_E_K, pr$q05, pr$q95),
  'Pr(2 to 10 clusters)' = sprintf('%.3f', pr$Pr_K_2_to_10),
  'Majority probability' = sprintf('%.3f', pr$wsb_gt_05), check.names = FALSE
)
ex_table('osm-table-H.12',
  pr,
  prior_display,
  sources = 'data-frozen/rasch/priors.csv',
  note = 'Six distinct fitted priors. Count 25 is the sensitivity TSMM prior shared by its Dual-Anchor roles. The 2–10 probability evaluates the main judgment, not the Count-25 target. Gamma(1, 3) reference from Paganin et al. (2023).'
)

# OSM H.13: keep all ten labeled diagnostic outcomes visible
diag <- ex_read('rasch', 'fit_diagnostics.csv', 'data-frozen')
diag$label <- unname(rasch_labels[diag$role])
ex_table('osm-table-H.13',
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

# OSM H.14-H.15: summarize counts and distinguish observed from population
# weights
ex_table('osm-table-H.14',
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
ex_table('osm-table-H.15',
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
