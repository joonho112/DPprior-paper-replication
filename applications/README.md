# School and study applications

Author: JoonHo Lee (jlee296@ua.edu)

There are two independently runnable paths. Rebuilding the posterior tables reads frozen **school/study and model summaries**. Preparing source inputs uses public package datasets and the explicitly supplied OCRS public-use CSV. Neither path runs posterior MCMC.

## Rebuild the reported posterior results

```sh
Rscript 00_setup.R --profile rebuild
Rscript applications/03_summaries.R
Rscript applications/04_sensitivity.R
Rscript applications/06_supplemental.R
Rscript verification/check_applications.R
```

The inputs cover 35 admitted fits: 15 primary, 15 wider-scale and five STAR HC3 fits. Twenty-five passed at the initial schedule and ten used the declared rerun. Each final summary uses 16,000 partition draws. The code recomputes unit stability, intervals excluding zero, and sensitivity differences; it checks the reconstructed tables against original publication outputs. Posterior count/weight summaries and joint moderator-profile intervals are frozen outputs of the original sampler. This path does not claim to recompute them from MCMC draws.

Observed largest allocation share, largest population atom weight after residual expansion, and the alpha-only prior-form tail have distinct columns. In particular, the alpha-only calculation is not a posterior probability of observed dominance. DORO-Unif remains an external-reference comparator and the hard-bound prior a sensitivity analysis.

## Prepare public data and check the first stage

```sh
Rscript 00_setup.R --profile public-data
Rscript applications/01_prepare_public_data.R
Rscript applications/02_first_stage.R
Rscript applications/05_prepare_ocrs.R --input "/your/data/OCRS_publicuse_impactanalysisdata.csv"
Rscript verification/check_application_inputs.R
```

The placeholder path must be replaced by the downloaded [AIR/ICPSR public-use V1 file](https://www.openicpsr.org/openicpsr/project/202181/version/V1/view). See `DATA_ACCESS.md` for source attribution and access. The loader verifies its SHA-256 and schema before processing it. It does not use a developer's private path or download files during setup.

- **STAR:** `mlmRev` 1.0-9 supplies 6,325 kindergarten records, of which 5,786 have complete required fields, across 337 classrooms and 79 schools. Reading and mathematics are standardized over complete cases; their average is standardized again. Small classes are compared with the pooled regular arms, including regular-with-aide. The primary first stage models classroom dependence; school OLS with HC3 uncertainty is a separate sensitivity.
- **Writing-to-learn:** `metadat` 1.6-0 supplies 48 published study-level effects and sampling variances. We preserve its bias-corrected standardized mean differences and variance construction. The sample-size assumptions are documented in `DATA_ACCESS.md`.
- **OCRS:** the impact-analysis CSV supplies 1,737 subject-observation rows. Nonmissing score and assignment leave 1,063 rows across 24 schools. We standardize the score over retained rows and fit schoolwise assignment contrasts. This descriptive complete-case example does not reconstruct the original blocked/covariate-adjusted ITT estimator. The counts are observation rows, not asserted unique-student counts across subjects.

The data-preparation path writes only school/study aggregates to `data-derived/applications/`. It checks them against the locked analysis inputs, then independently recalculates conventional normal random-effects benchmarks. The normal model summarizes continuous heterogeneity; it does not estimate DP cluster counts. The optional posterior-refit commands are documented separately under `refit/` as they become available.

All generated tables go to `data-derived/applications/`; verification receipts go to `outputs/verification/`. See `CODEBOOK.md` for frozen inputs and their limits.

## Revised cross-application comparisons

The [application exhibit runner](exhibits/README.md) uses the reviewed cross-application inputs for Table 4, Figures 5 and 6, and OSM H.2–H.5 figures and H.3–H.4/H.16–H.19 tables. `Rscript applications/exhibits/build.R` runs after the normal-normal and Rasch summary steps. The complete exhibit runner dispatches these dependencies in order.
