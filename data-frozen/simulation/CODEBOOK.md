# Frozen simulation inputs

Author: JoonHo Lee (jlee296@ua.edu)

All rows describe synthetic simulations. They contain no participant records. Source hashes, export hashes, row counts and omitted fields are recorded in `provenance/simulation-export.json`. These inputs summarize the manuscript's completed fitting runs; reading them does not rerun those fits.

| File | Rows | Key | Role |
|---|---:|---|---|
| `design.csv` | 322 | `design_id` | One row per suite and conceptual condition; repeated design metadata |
| `fit_status.csv` | 64,400 | `fit_key` | Every planned replicate, its terminal status, fitting schedule and specified prior |
| `fit_metrics.csv` | 57,151 | `fit_key` | Only checksum-admitted fits that passed the production diagnostic rules |
| `cell_denominators.csv` | 322 | `suite`, `conceptual_cell_key` | Independently recorded collector denominators |
| `calibration.csv` | 50 | `calibration_id` | Calibration outcomes referenced by these conditions, including failures |

`design_id` combines `suite` and `conceptual_cell_key`. `replicate` is unique within a design. A `fit_key` identifies a planned fit; `dgp_sha256` identifies its synthetic data specification and is retained to verify pairing across grand-mean priors. Hashes are identifiers, not participant information. `batch` distinguishes the production and extension collections.

## Design and status fields

`J` is the number of exchangeable units and `K_true` the true occupied count. `I_target` is the specified informativeness level; `n_mean` is the mean sampling-size input. `weights`, `weight_scheme`, `dominant_share` and `dominant_location_mode` distinguish equal counts from exact dominant counts, including central versus balanced-random dominant locations. A `.60` dominant allocation share in this DGP is not a prior probability of weight dominance.

`prior`, `r`, `mu_K`, `VIF`, `dual_lambda` and `prior_calibration_id` identify the prior specification. `r` multiplies the true count to give the stated mean in misspecification conditions. `a` and `b` in the status file are Gamma shape and **rate**. Missing parameters for a rejected calibration are intentionally missing. `grand_mean_variance` is a variance; the label `N(0,100 variance)` is explicit to prevent a precision interpretation. The three branch variances are .01, 1 and 100. `is_headline` identifies the main comparison family; `in_convergence_scope` and `is_active_dual` preserve the original selection flags.

`runnable=FALSE` means calibration prevented fitting. `status_category` is `success`, `final_diagnostic_failure` or `dependency_rejection`. `failure_type` and `dependency_code` retain the typed reasons. `scientific_eligible` is the final admission decision, `attempt` is the selected terminal attempt, and `retry` marks a retried fit. `n_chains`, `n_iter`, `n_warmup` and `thin` record the planned schedule; they are not a command to refit. `dgp_seed` is synthetic simulation reproducibility metadata.

## Fit metrics

| Field | Meaning |
|---|---|
| `E_K`, `sd_K` | Posterior mean and SD of occupied count |
| `q025_K`, `q975_K` | Posterior 95% count interval endpoints |
| `p_K1`, `p_K_le2` | Posterior probabilities of one and at most two occupied groups |
| `alpha_mean` | Posterior mean of DP concentration |
| `K_bias` | `E_K - K_true` |
| `tau_rmse` | Within-replicate effect-estimation RMSE, as computed by the original verified collector |
| `cover95_K` | Indicator that the posterior count interval contains `K_true` |

The metric columns are finite for every retained row. Missing estimates in a generated cell summary mean no fit was admitted; they do not mean zero error or zero posterior probability. Cell summaries average retained fits and report their denominators. Between-replicate quantiles use R quantile type 8, as in the manuscript synthesis. Paired contrasts use only replicates retained in both branches and verify matching data hashes.

The accounting is 64,400 planned − 2,800 calibration exclusions − 4,449 diagnostic failures = 57,151 retained. Status-derived counts must match every cell of the collector denominator file before aggregation proceeds. Original hosts, execution paths, raw posterior payloads and deployment metadata are omitted.
