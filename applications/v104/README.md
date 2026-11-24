# Joint-draw and population summaries for version 104

From a fresh checkout, run `Rscript exhibits/00_build_all.R` to generate the upstream application and Rasch summaries along with the exhibits. After that, `Rscript applications/v104/build.R` rebuilds the seven version-104 application tables, six figure PDFs and their numerical data. The default calculation uses the public September application and Rasch summaries and the additional aggregate inputs in `data-frozen/applications104/`. Its `input-contract.csv` gives the columns, compound key, file size, row count and SHA-256 of each additional input.

| Input | Observational unit and calculation |
| --- | --- |
| `rank-bin-sums.csv` | One percentile interval per fit pair, application and chain block. Sort all retained latent values after pooling the equally weighted observed units within each fit. Pair values by rank, square their differences and sum within each of 100 equally populated bins. Full and middle-90% RMS use bins 1–100 and 6–95, respectively, and divide by their actual rank counts. Different priors are never paired by MCMC draw number. |
| `percentile-values.csv` | Pooled Gamma and Dual inverse finite-CDF values at percentiles 0.005 through 0.995 in steps of 0.005. Each requested percentile is an exact empirical rank boundary. |
| `chain-mean-by-unit-or-score.csv` | Posterior means after pooling chains 1–2 or 3–4. STAR and writing-to-learn use anonymous unit order and unit weight 1. Vocabulary uses raw scores 0–13 and their respondent frequencies. The finite CDF gives each of the 500 latent abilities the same weight. |
| `finite-cdf.csv`, `spread-functionals.csv`, `spread-by-count.csv` | Observed units in retained draws. School/study CDFs have 161 grid points, ability CDFs have 521. The discrete pointwise limits and within-draw IQR use type 1 quantiles. The SD uses denominator J. Posterior intervals for SD and IQR use type 7 quantiles. Preserve the reported node statuses, split R-hat, effective sample sizes and Monte Carlo SEs. |
| `interval-endpoint-mcse.csv` | Three school/study lower endpoints examined near zero. Join on application and analytic unit index to the existing public school/study interval rows. |
| `population-density-normal.csv`, `density-runs.csv` | Six additional population-density runs, each with 321 points from −4 to 4 in steps of 0.025. The main comparison uses the fixed 321-node grid for integrated distances and displays the central range. These six runs retained mixture parameters to describe a new exchangeable unit. Node-level rounding warnings apply. Vocabulary uses the existing 961-point DPM and Gaussian grids. |
| `pooling-*.csv` | Anonymous unit-to-middle sharing averages, MCSEs of the upper-minus-middle contrast, chain halves, cutoff comparisons and counts 1–4. The 90th percentile and middle-half cutoffs use type 7 observed-effect quantiles. Exclude the focal unit from its middle peers. |

The public school/study ordering matches the previously released aggregate fits. Figure 6 names only the two marked units as printed in the manuscript. School coordinates use primary classroom-adjusted inputs. The schoolwise OLS/HC3 and wider-scale results remain separate. The writing-to-learn coordinates refer to published studies and retain the metadat attribution. See [data access](../../DATA_ACCESS.md) for source terms and the separate OCRS, STAR and IRW access routes.

## Optional extraction from existing joint draws

The following commands accept local manifests and write to the output directory supplied as their last argument. Install `posterior` for the pooling MCSE calculation. Use a separate output directory when checking a new extraction.

```sh
Rscript applications/v104/export_rank_summaries.R NORMAL.csv RASCH.csv extracted
Rscript applications/v104/export_pooling.R NORMAL.csv extracted
```

`NORMAL.csv` identifies a primary-scale accepted fit by `app` (star or bd), `prior_label` (Vague, TSMM or Dual-Soft-Selected), `analysis_spec`, `execution_fit_key`, `input_unit_order_match`, `chain_row_blocks`, `artifact_path` and `artifact_sha256_actual`. Pooling also requires `input_file` and `input_sha256`, where the input contains `unit` and `tau_hat`. Archives contain named 16,000-by-J retained effect matrices in `fit$retained$tau_j`, with rows 1–4000, 4001–8000, 8001–12000 and 12001–16000 denoting separate chains. Pooling also uses same-size assignments in `fit$retained$z`. The extraction checks the stored fit key, final diagnostic decision, byte hash and unit order.

`RASCH.csv` identifies a reportable main-arm archived fit by `arm`, `prior_role` (vague, tsmm, dasoft), `v5_reportable`, `gate_pass`, `fit_key`, `combined_path` and `combined_sha256_actual`. The file contains a 4,000-by-4-by-500 ability array, chain IDs and retained counts, consistent person order and raw scores. Those private objects are used transiently to compute rank-bin sums, rank profiles and 14 score-level means. The output omits identifiers, response vectors, individual abilities and allocations. The default public build requires only the exported summaries.

For vocabulary the full Gamma–Dual quantile RMS is 0.0181 logits. Splitting each prior across two pairs of chains gives 0.0150 and 0.0167 logits. This descriptive comparison limits attribution of the upper-tail difference to the prior choice.
