# Frozen Rasch inputs

Author: JoonHo Lee (jlee296@ua.edu)

All exported inputs are item, raw-score group, fit or model-grid summaries. Source and export fingerprints, row counts and filtering decisions are recorded in `provenance/rasch-export.json`. No raw responses, respondent IDs, ages, person EAP rows or person-specific diagnostic rows are exported.

| File | Key/unit | Meaning |
|---|---|---|
| `score_counts.csv` | `raw_score` 0–13 | Frequencies 3,1,2,4,2,3,9,17,52,70,116,89,69,63; total 500 |
| `item_counts.csv` | 13 `item` values | Analyzed real-word endorsement counts and proportions |
| `item_endorsements.csv` | 16 `key` values | Word labels, nonword flags, full-file and sample proportions for OSM H.8 |
| `wle_item_difficulties.csv` | role + item | Six fixed 13-item posterior-mean difficulty vectors, centered to sum zero |
| `prior_and_fit_registry.csv` | arm + role | Ten roles, six prior IDs, eligibility, final schedules and plan-deviation flags |
| `priors.csv` | arm + role, including an unfitted review row | Frozen prior implications, calibration status, lambda and scientific role |
| `achieved_judgments.csv` | arm + role | The six stated/achieved judgment summaries |
| `posterior_summary.csv` | ten arm + role combinations | Original fit-level summaries and diagnostics; use `gate_pass` before comparing |
| `fit_diagnostics.csv` | ten arm + role combinations | Grouped convergence measures and attempt notes, with private operational fields removed |
| `score_groups.csv` | arm + role + `r` | Score-group means, uncertainty, counts, MCSE and per-chain summary strings |
| `count_pmf.csv` | passing arm + role + K | Six passing fits' posterior occupied-count probabilities |
| `model_density.csv` | passing arm + role + x | Six passing fits' posterior mean model densities and pointwise lower/upper bands |
| `gaussian_density.csv` | x | Fitted Gaussian model density and pointwise uncertainty |
| `gaussian_diagnostics.csv` | attempt + group + variable | Benchmark item/model/score/density diagnostics; individual theta rows omitted |

`arm=main` uses the primary count judgment; `arm=sens` uses Count-25. Role names map to Vague (`vague`), Literature default (`litdef`), TSMM (`tsmm`), Dual-Anchor (`dasoft`) and the hard sensitivity (`dahard`). Sensitivity labels can repeat the same prior; role count is not distinct-prior count. `prior_id` preserves the original mapping. Small numerical differences in repeated calibration parameters do not make an additional substantive prior.

For WLE, `posterior_mean_difficulty` is beta in `logistic(theta-beta)`, with discrimination fixed at one. Difficulty vectors are fixed inputs; uncertainty in their estimation is not added to conditional WLE SEs. `source_fit_sha256` identifies the item-estimate source without exposing its filesystem path. The WLE score equation is `r-sum(p)+I'/(2I)=0`; SE is `1/sqrt(I)`. Reliability is `1-mean(SE^2)/var(WLE)`, with person frequency weights and sample-variance denominator N−1. Extreme scores remain in both terms.

For score-group comparisons, `EAP_r` is the posterior mean for raw-score group r and `n_r` its frequency. `width_r` is its group interval width; `mcse_EAP_r` measures Monte Carlo error of the estimated mean. The difference MCSE uses the original independent-run propagation rule `sqrt(mcse_r^2+mcse_reference^2)`. It is not the person's posterior SD. Weighted RMSD is `sqrt(sum(n_r*delta_r^2)/500)`. Per-chain strings are rounded original summaries and are not sufficient to reconstruct unrounded chain-level contrasts; the default calculation does not claim to do so.

Posterior `E_K` and count interval/probability columns refer to occupied groups among 500 responses. `observed_largest_share` refers to the observed allocation fraction. `population_largest_atom_weight` includes residual population-weight expansion, whereas the `alpha_only_prior_form` diagnostic conditions only on concentration draws. They are distinct quantities.

Density `mean`, `lower` and `upper` are model-density posterior summaries at x; the bands are pointwise, not simultaneous. Their integrals need not equal one individually. The mean curve's grid mass and the original outside-grid mass checks are retained. Failed replicate curves are omitted from density/PMF exports and remain visible in diagnostic records.

The original item and score aggregates carry the IRW/OSPP attribution and qualified upstream terms described in `DATA_ACCESS.md` and `LICENSE-SCOPE.md`. Fit summaries are derived research outputs. The full-file endorsement rates are frozen reviewed aggregates; they are not recomputed from a redistributed full response file.
