# Application input and posterior-summary codebook

Author: JoonHo Lee (jlee296@ua.edu)

Frozen files live in `data-frozen/applications/`. Original and exported hashes are in `provenance/application-data-export.json` and `application-posterior-export.json`. Build scripts do not update these files. Numerical checks use original publication frames under `verification/expected/`, separately from computed results.

## Data and model units

`star_multilevel.csv` and `star_hc3.csv` each contain 79 **schools**, `bangert_drowns_primary.csv` contains 48 **published studies**, and `ocrs_descriptive_primary.csv` contains 24 **schools** derived from the official public-use release. None contains pupil/respondent rows. `unit` is the within-application index, `tau_hat` the first-stage standardized effect, `se` its standard error, and `se2` its sampling variance. Source study/school labels, counts and descriptive location/grade/length fields support alignment and the reported profiles. Missing moderator values are preserved rather than imputed.

## Posterior inputs

| File | Unit and key | Contents |
|---|---|---|
| `fit_summary.csv` | 35 fits; `execution_fit_key` | Frozen count and weight summaries, role and final-attempt metadata |
| `unit_summary.csv` | 1,905 fit-by-school/study rows; fit key + `unit` | Posterior effect mean/SD/quantiles and source aggregate fields |
| `diagnostic.csv` | fit + attempt + variable | R-hat, bulk/tail ESS, MCSE and original pass/fail indicators |
| `attempts.csv` | fit + attempt | Original schedules, diagnostic decisions and failed-variable names; no host/runtime/path metadata |
| `base_manifest.csv` | 15 priors; `fit_key` | Gamma shape/rate, calibrated count summaries, selected lambda and scientific boundary class |
| `execution_manifest.csv` | 35 executions; `execution_fit_key` | Model scale, source/analysis counts, schedules and synthetic random seeds; no private paths |
| `normal_benchmark.csv` | three applications | Normal random-effects context, independently recalculated by the public-input path |
| `star_profiles.csv` | fit + location level | 20 frozen joint-posterior location-profile summaries |
| `bd_profiles.csv` | fit + moderator | 10 frozen grade/length slope summaries and complete-case denominators |

`execution_fit_key` binds an analysis specification and prior; `base_fit_key` binds the calibrated prior shared across specifications. `analysis_spec` distinguishes `primary_scale_aligned`, `wide_scale_sensitivity` and `star_hc3_uncertainty`. `prior_order` is the publication ordering; `prior_role`/`scientific_role` preserve the external-reference and hard-sensitivity distinctions.

`E_K`, `sd_K`, `median_K`, `q025_K`, `q975_K`, `p_K1` and `p_K_le2` summarize posterior occupied counts. `alpha_mean` is the posterior concentration mean. `retained_partition_draws=16000` is the final retained draw count, not a number of independent samples.

Fields beginning `observed_largest_` summarize the largest allocation count/share among the fixed observed units. `population_largest_occupied_weight_` covers occupied atoms; `residual_unoccupied_mass_` covers the mass remaining outside them. `population_largest_atom_weight_` includes residual expansion. `alpha_only_prior_form_p_first_size_biased_weight_gt_05` is a distinct alpha-only diagnostic, not an observed or full posterior population-dominance probability. These are not interchangeable estimands.

Unit quantiles (`q025`, `q50`, `q975`) are marginal posterior effect quantiles. The strict equal-tail interval excludes zero when `q025 > 0` or `q975 < 0`. The scripts recompute RMSE, absolute shifts, correlations, and interval-decision changes from these unit summaries. Sensitivity pairing is by application, prior and unit; every expected unit must be present.

Profile intervals are **joint-posterior aggregate summaries from the original collector**. Averaging marginal unit interval endpoints would not reconstruct them, so the code preserves the frozen profile intervals and rebuilds only their publication metadata. These are descriptive location profiles and unadjusted moderator slopes, not inferred DP clusters or causal moderator estimates. School/study coclustering pairs and raw posterior payloads are not included.

Final model counts are 15 primary + 15 wider-scale + 5 HC3 = 35, with 25 initial passes and 10 successful declared reruns. Earlier failed attempts remain in `attempts.csv` and diagnostics; all final fits passed. Missing fields that are inapplicable to a different application remain missing. Source-N fields retain the historical manifest, while the source-preparation receipts give newly checked raw observation counts, including OCRS's 1,737 rows.
