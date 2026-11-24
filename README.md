# Dirichlet process prior elicitation

JoonHo Lee (jlee296@ua.edu) · The University of Alabama

This replication package accompanies the third arXiv version of *Design-Conditional Prior Elicitation for Dirichlet Process Mixtures*. It rebuilds **43 tables and 19 figures** from the article and online supplement, including the 50-site calibration example, the synthetic multisite simulation and the three application comparisons. [Preprint and supplement](https://arxiv.org/abs/2602.06301v3)

## Start with the published results

After extracting the ZIP or downloading the repository, open the [results index](docs/results.html) in a browser. It links all 43 tables and 19 figures to their numerical data and analysis code. For the simulation, start with [Table 3](docs/artifacts/main-table-3.html) and [Figure 4](docs/artifacts/main-figure-4.pdf). For the three applications, see [Table 4](docs/artifacts/main-table-4.html) and [Figure 5](docs/artifacts/main-figure-5.pdf). [Figure 6](docs/artifacts/main-figure-6.pdf) follows the upper-tail schools and studies.

## Reproduce the results

Use R 4.6.0 and a Python installation with venv. Install TeX Live with the required fonts, Fontconfig, Poppler and Quarto as described in the [toolchain guide](common/TOOLCHAIN.md). From the repository root, create the Python environment and install the locked R packages. The examples use Python 3.13. Python 3.9–3.13 can run the pinned plotting packages.

~~~sh
python3.13 -m venv .venv
.venv/bin/python3 -m pip install -r common/figure-requirements.txt
Rscript common/install_dependencies.R --profile rebuild --execute
Rscript 00_setup.R --profile rebuild
Rscript exhibits/00_build_all.R
~~~

For an independent numerical check, the optional command is `Rscript verify_reproduction.R`.

Open `outputs/v104/main-table-3.html` for the cluster-count comparison at low informativeness, `outputs/v104/main-table-4.html` for the three applications, and `outputs/v104/main-figure-4.pdf` through `main-figure-6.pdf` for the main comparisons. Each exhibit includes a CSV. Tables also have HTML and LaTeX versions, and figures have PDF and PNG versions. The reproduction map gives the inputs, code and verification scope for each result. [Reproduction map](provenance/reproduction-map.csv)

The numerical build uses the public `data-frozen/` files and writes intermediate results under `data-derived/`. It runs the calibration optimizer, reconstructs synthetic-site errors and aggregates the available application summaries. The school, study and vocabulary analyses use different outcome units. Within the simulation, the tables use each prior's converged replications, Figure 4A–B require all five fits to converge and Figure 4C requires the two compared fits to converge.

## Read the replication guide

The package includes the guide’s reviewed exhibit snapshots. To read the guide from a fresh checkout, run the following commands, then open `docs/_book/index.html`.

~~~sh
quarto render docs
~~~

After rebuilding and verifying the numerical outputs, maintainers can refresh the guide and compare every rendered asset with its newly built source.

~~~sh
python3 docs/sync_assets.py --execute
quarto render docs
python3 verification/check_book.py --current
~~~

| Task | Entry point or guide |
|---|---|
| Repeat the full worked calibration and the separate hard bound | `Rscript calibration/v104/worked.R` |
| Rebuild all methods displays, including the nine application interval probabilities | `Rscript calibration/v104/build.R` |
| Inspect data access and the school, study and score-level inputs | [Data access](DATA_ACCESS.md) |
| Prepare source data or fit a particular model | [Refit entry points](refit/README.md) |
| Check the installed R packages for a task | `Rscript 00_setup.R --profile calibration`, `public-data`, `refit` or `book` |
| Run the build in a new, allowlisted directory | [Cold-start instructions](common/TOOLCHAIN.md#test-a-fresh-directory) |

The standard build uses previously fitted simulation and application summaries. Student and respondent records, individual latent draws and joint posterior draws require separate access for the applicable optional routes. The package includes the four selected simulation replay results used in the supplement. [Data access](DATA_ACCESS.md)

## Citation and licensing

Lee, J. (2026). *Design-Conditional Prior Elicitation for Dirichlet Process Mixtures: A Unified Framework for Cluster Counts and Weight Control*. arXiv:2602.06301v3. [Preprint](https://arxiv.org/abs/2602.06301v3) · [Citation metadata](CITATION.cff)

The original replication code is MIT-licensed. The documentation, figures, and third-party data have the terms listed in the [license guide](LICENSE-SCOPE.md).
