# Dirichlet process prior elicitation: replication materials

JoonHo Lee (jlee296@ua.edu)

This repository accompanies **Design-Conditional Prior Elicitation for Dirichlet Process Mixtures: A Unified Framework for Cluster Counts and Weight Control**. The [preprint is available on arXiv](https://arxiv.org/abs/2602.06301).

We provide code for calibrating concentration priors, examining mixture weights, and reproducing simulation and application results. The standard build creates **41 tables and figures** from the supplied summaries. Separate scripts prepare source data, calculate conditional WLE reliability and fit selected models.

## Rebuild the results

We used R 4.6.0. From this directory, check the required packages:

~~~sh
Rscript 00_setup.R --profile rebuild
~~~

To inspect the installation plan, run:

~~~sh
Rscript common/install_dependencies.R --profile rebuild
~~~

Add `--execute` to install the specified source versions into `.library/`. A compiler and system libraries may be required. Then rebuild and verify the results:

~~~sh
Rscript exhibits/00_build_all.R
Rscript verify_reproduction.R
~~~

Intermediate summaries are written to `data-derived/` and completed tables and figures to `outputs/`. Tables are provided as CSV, HTML and LaTeX; figures as CSV, PDF and PNG. The [reproduction map](provenance/reproduction-map.csv) identifies the input and script for each result. The supplied inputs in `data-frozen/` remain unchanged.

## Read the guide

The Quarto Book explains the analyses, commands and interpretation. With Quarto 1.9.37, run:

~~~sh
quarto render docs
python3 verification/check_book.py
~~~

Open `docs/_book/index.html`. The examples are included in the repository, so the guide can be rendered before running the numerical calculations.

| Task | Guide chapter | Dependency profile |
|---|---|---|
| Rebuild tables and figures | `docs/01-start.qmd` | `rebuild` |
| Calibrate counts and examine weights | `docs/03-prior-elicitation.qmd` | `calibration` |
| Reconstruct the simulation comparisons | `docs/04-simulation.qmd` | `rebuild` |
| Prepare STAR, writing-to-learn or OCRS data | `docs/05-applications.qmd` | `public-data` |
| Calculate WLE reliability | `docs/06-rasch-wle.qmd` | `rebuild` |
| Fit a selected model | `docs/07-verification-refit.qmd` | `refit` |

The index includes analyses developed after the initial preprint. Its table and figure numbers identify the results supplied with this repository.

## Data and verification

The standard build uses simulation metrics and school, study, score, item and model summaries. To repeat source preparation or fit an application model, obtain the data described in [DATA_ACCESS.md](DATA_ACCESS.md). For example, after downloading the official **AIR/ICPSR public-use OCRS file**, run:

~~~sh
Rscript applications/05_prepare_ocrs.R --input "/your/data/OCRS_publicuse_impactanalysisdata.csv"
~~~

The OCRS analysis estimates descriptive schoolwise contrasts among complete cases. The original study's blocked intention-to-treat analysis has a different target. Student records and person-level posterior draws are not distributed here.

The numerical checks compare the reconstructed results with independent expected values, including 68 selected numbers. These checks and an isolated rebuild passed on macOS arm64. Short model runs tested the fitting code; full production refits have not been repeated for this package. See [verification](verification/README.md) and the [platform record](provenance/platform-matrix.csv).

Table 2 contains a rounding difference: the tail probability from unrounded parameters is .144969, which gives .14, while the printed reference gives .15. The rebuilt table uses .14; the [numerical note](provenance/manuscript-discrepancies.csv) records the comparison.

## Citation

Lee, J. (2026). *Design-Conditional Prior Elicitation for Dirichlet Process Mixtures: A Unified Framework for Cluster Counts and Weight Control*. arXiv:2602.06301. [Preprint](https://arxiv.org/abs/2602.06301).

[CITATION.cff](CITATION.cff) provides the citation metadata. The DOI [10.48550/arXiv.2602.06301](https://doi.org/10.48550/arXiv.2602.06301) identifies the preprint.

## Terms of use

Original replication code is licensed under MIT. Documentation, author-created outputs and source data have separate terms: see [LICENSE-SCOPE.md](LICENSE-SCOPE.md), [LICENSE-DATA](LICENSE-DATA) and [DATA_ACCESS.md](DATA_ACCESS.md).
