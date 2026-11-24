# Verification

From the package root, run the build, then the verifier.

```sh
Rscript exhibits/00_build_all.R
Rscript verify_reproduction.R
```

The exhibit check requires 62 named receipts, the 186 corresponding files, and the G.2 example JSON. It checks their identities, input and display hashes, numeric CSV hashes, row counts and column lists against the exhibit contract. Independent numeric checks use fixed references under `verification/expected/`. The same package reproduces historical regression frames for the inherited simulations, applications and vocabulary example. Each result's scientific scope is recorded in the [reproduction map](../provenance/reproduction-map.csv).

The independent simulation check compares exact converged-fit membership, full-precision results and printed manuscript values. The application check covers five fitted priors per application, population and observed weights, score frequencies, chain splits, individual and percentile changes, and both signs used for upper-tail pooling. The checks read the regenerated tables and figures' numeric frames. They also read each rendered HTML and LaTeX table for the revised simulation and application results.

```sh
python3 verification/v104/simulation.py
python3 verification/v104/applications.py
Rscript verification/check_exhibits.R
Rscript verification/check_refit_guards.R
```

For individual results, the checkers accept the package root through `--root` on their Python interfaces. The [comparison rules](comparison-rules.yml) document comparison tolerances. Optional fitting schedules and their actual execution coverage are in the [refit checks](refit-smoke.md). The [discrepancy ledger](../provenance/manuscript-discrepancies.csv) gives both source and computed values where the printed manuscript used a different parameterization.

Run `python3 verification/v104/negative_controls.py` for the six numerical mutation tests. Each uses a disposable copy and checks the specific failure message before restoring the original bytes and rerunning the appropriate verifier. Results are written to `outputs/verification/v104-negative.json`.
