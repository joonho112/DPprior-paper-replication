# Rasch counts, conditional WLE and model densities

Author: JoonHo Lee (jlee296@ua.edu)

The default path uses score/item/fit aggregates. It includes no response matrix, respondent identifiers, ages or person posterior records. The optional full-refit path must obtain the IRW source separately and apply `data-contract.yml`.

```sh
Rscript 00_setup.R --profile core
Rscript rasch/01_wle.R
Rscript rasch/02_posterior_summaries.R
Rscript rasch/03_density.R
Rscript verification/check_rasch.R
```

Generated CSVs are in `data-derived/rasch/`; calculation and comparison receipts are in `outputs/rasch/` and `outputs/verification/`.

1. **Conditional WLE:** solve Warm's equation at scores 0–13 under each of six fixed item-difficulty vectors. Combine those scores with their frequencies using the unbiased variance denominator 499. The primary reliability is .71208 and the perfect-score SE is 1.73309. No item model is estimated by this command. The scoring functions in `common/R/wle.R` use base R; shared file/hash/receipt utilities also use the core profile packages.
2. **Posterior summaries:** retain the six passing distinct-prior fits. Recalculate score-weighted changes from Vague, including RMSD and Monte Carlo uncertainty, using 14 score-group rows per fit. Main-arm Dual-Anchor has RMSD .01227 logits; Count-25 TSMM has posterior mean count 11.71881, largest population atom weight .68749, and RMSD .02098. These are different comparisons.
3. **Model densities:** prepare the posterior mean model density with the original pointwise bands on the 961-point grid. Check grid integrals, original outside-grid mass checks and the count PMF's normalization and mean. Add the fitted Gaussian benchmark. These curves are not an empirical distribution of person EAP estimates. Their uncertainty bands remain frozen joint model outputs and are not recomputed from draws in this path.

The ten labeled fitting roles correspond to six distinct priors. Four repeated-prior fits did not pass their initial diagnostics and were exempt from reruns; they remain in the fit registry and diagnostics, but do not enter reported posterior comparisons or density/PMF frames. Main TSMM passed the declared second schedule. Five other distinct-prior fits required a third schedule of 120,000 iterations, 60,000 warmup and thinning 15. That third schedule departed from the original one-rerun plan. Earlier draws were not pooled with later attempts. Each final passing fit has 16,000 retained draws across four chains.

The `schedule_overridden` field inherited from the software concerns an override of a particular job's settings. It must not be read as “no departure from the original analysis plan.” The public registry has a separate `original_two_schedule_deviation` field to make the five third-schedule departures explicit.

The public-source order matters: sort the 2,495 source IDs as character values, sample 500 row positions with seed 81770971, and only then remove the three nonwords VCL6, VCL9 and VCL12. Sixteen item endorsement aggregates remain available for OSM Table H.8; the response model and WLE use only the 13 real words. See `DATA_ACCESS.md` for OSPP/IRW attribution and upstream terms.

`verification/check_rasch.R` checks the newly generated outputs. It does not count the historical 746-claim ledger's PASS rows as 746 new calculations.

## Vocabulary changes in the November manuscript

The three-application results use fourteen score groups for the 500 respondents and four-chain or split-chain summaries of the finite distribution. Tables H.3–H.4 and H.16–H.18 and Figures 5 and H.2–H.4 use these aggregate inputs. See the [application exhibit guide](../applications/v104/README.md).
