# Simulation recovery for Table 3 and Figure 4

Run `Rscript simulation/v104/build.R` after the six simulation aggregation scripts, or run the package's complete exhibit build. The simulation runner verifies the eight frozen input files with Python 3. Plotting uses R, ggplot2, patchwork, scales, ragg, pdfLaTeX and pdftocairo.

Table 3 and OSM Table F.6 use the accepted count fits at low informativeness. The three generating cluster counts and five concentration priors give 15 rows. The number of attempted fits per cell is 200. The interval-width column gives the average posterior 95% interval width, and the SD column is the between-replication SD of the posterior mean count.

## From simulated sites to distribution errors

The PM and CB matrices in `data-frozen/simulation104/` contain the posterior means and constrained Bayes estimates of the same 100 synthetic sites in the original site order. The truth matrix stores one row per shared data hash. Join each fitted row to the existing simulation ledger by `fit_key` and identify a data set by generating count, informativeness, replicate and `dgp_sha256`. The archived posterior mean of the within-draw site SD uses denominator 100. The generating sample SD is 0.25 with denominator 99, so the true finite-population SD is 0.2487468593.

The code integrates the squared difference of the right-continuous empirical CDFs over consecutive pooled jump intervals. KS is the largest absolute difference at these jumps. Site RMSE uses the paired estimate and truth at each site. Tables F.12–F.14 summarize these errors across each prior's converged fits. The full 8,607 fit-level estimates and 45 cell summaries are in `data-derived/v104/simulation-fit-metrics.csv` and `simulation-cell-means.csv`.

## Matched sets for Figure 4

Panels A and B use the replications where all five priors converged. For each design, Panel B computes 100 times the difference between the largest and smallest prior-specific mean error divided by the smallest mean. Panel C requires both Gamma(1,1) (Vague) and Dual-Anchor. For each data set, the inverse empirical CB CDF evaluated at the pth integer percentage is the pth smallest value among the 100 sites. Tables F.16–F.18 report errors that compare each prior with truth. Signed changes subtract the Gamma estimate from the Dual-Anchor estimate. Absolute changes take the absolute value before averaging. The CSV next to Figure 4 labels each mark by panel, design, denominator, measure and value.

The selected replay covers four fits from two low-information data sets, each with replicate ID 1. Figure F.5 uses the stored posterior-mean finite CDF over 1,201 display points per fit. Table F.15 uses the archived full-jump pooled posterior losses and recalculates the GR losses from the ordered empirical support. The table CSV includes the number of retained joint draws, fit identifiers and the replay-to-archive differences.

Each CSV input's columns, primary key, SHA-256 digest, byte and row counts, and source digests are recorded in `input-schema.json`. Run `python3 simulation/v104/check_inputs.py` to verify this inventory directly. The DORO projection record accompanies OSM Table F.4 and documents the source and runtime projections for target count 30.
