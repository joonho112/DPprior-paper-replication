# Calibrate counts and inspect population weights

Author: JoonHo Lee (jlee296@ua.edu)

These small examples recalculate the worked example in OSM Appendices B and G: 50 exchangeable units, an expected count of 5, and count variance 10. They use `Gamma(shape = a, rate = b)` throughout. They do not fit outcome data or run MCMC.

From the repository root:

```sh
Rscript 00_setup.R --profile calibration
Rscript calibration/01_count_calibration.R
Rscript calibration/02_weight_diagnostics.R
Rscript calibration/03_dual_anchor.R
Rscript calibration/04_hard_and_failures.R
Rscript verification/check_calibration.R
```

The count and weight examples take seconds; the full 29-point grid can take minutes. Generated CSVs go to `data-derived/calibration/`; receipts go to `outputs/calibration/` and `outputs/verification/`. The printed manuscript values in `verification/expected/` are fixed reference inputs, these scripts leave them unchanged. Rebuilding the paper from frozen inputs is a separate path and does not require recalibrating this grid.

## What each step establishes

1. **Count calibration:** A1 supplies an initializer. Its exact mean (4.415) and variance (5.618) miss the requested values; A2-MN refines it to TSMM, with mean 5 and variance 10. Fitting uses 160 quadrature nodes and verification uses 320. Initialization is not acceptance.
2. **Weight diagnostics:** a size-biased population weight is the weight encountered by a randomly drawn member of the population. Its tail probability differs from the probability that the largest population weight exceeds the threshold. The latter is integrated independently at .5 and .9, where the exact identity applies. Neither is the largest observed allocation fraction in a finite sample. That fraction requires allocations and is not calculated here.
3. **Dual-Anchor:** the TSMM majority tail exceeds .40, so the adjustment is triggered. Every candidate uses the same count scales, 5 and 10, and the same soft target, .25. We select the largest eligible lambda in the prespecified 29-point grid. The worked choice is .30, with majority tail .398: it exits the risk band but does not attain the soft target. Comparing total losses across different lambda values is not the selection rule.
4. **Hard sensitivity and failures:** the separate hard bound is .25. A verified active constraint is allowed for this sensitivity fit; it is not a blanket permission to accept numerical boundaries. Missing, unverified, approximate, invalid, and unclassified results are rejected. A separate one-point grid containing only lambda=1 exercises the no-eligible return path; it does not replace the paper's 29-point grid.

`policy_helpers.R` retains the manuscript's scientific policy functions. `DPprior` supplies the numerical algorithms; `policy.yml` supplies the study's acceptance and selection choices. An approximate result may be retained in the diagnostic grid, but cannot silently become the selected prior.

## Verification scope

The verifier checks the three priors against rounded values transcribed from the locked OSM source, using half a unit in the last printed decimal as tolerance. It also integrates conditional count moments against the Gamma density using base R adaptive integration, independently of DPprior's quadrature. It checks loss normalization, all 29 grid rows, the largest eligible lambda, weight bounds, and rejection outcomes. This validates the worked example and acceptance paths; it does not establish performance for every possible elicitation target.

The November methods runner, `Rscript calibration/v104/build.R`, also executes the published OSM G.2 code and reconstructs Tables 1–2, E.1 and G.1 and Figures 1–3 and G.1. 
