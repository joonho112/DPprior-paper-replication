# Rebuild the current exhibits

Author: JoonHo Lee (jlee296@ua.edu)

Run `Rscript exhibits/00_build_all.R` after checking the `rebuild` profile. It runs the small simulation, application-summary and Rasch-summary scripts, then renders 30 tables and 11 figures. It does not download source data, optimize priors, fit an item model, or launch MCMC.

Figures are PDF/300-dpi PNG pairs. Tables are readable HTML plus portable LaTeX longtable fragments. Each has an adjacent numeric CSV. `outputs/exhibit-receipts/` binds inputs and outputs with SHA-256 hashes. `outputs/key_numbers.csv` holds 68 predeclared headline numbers; `verification/check_exhibits.R` compares them with the manuscript contract independently of generation.

The [reproduction map](../provenance/reproduction-map.csv) lists all 41 current items and their levels of reproduction. Numerical frames and scientific contrasts are preserved; layouts are adapted for the guide and are not pixel replicas of journal pages. Grouped design-size tables retain per-cell denominators. Main Figure 8 contains only occupied-component counts; the fitted density stays in OSM Figure H.1. OCRS Table H.5 is rebuilt from reviewed school/model summaries, with a separate public-use source preparation and optional refit path.

The quick path uses small frozen calibration snapshots produced by the original analysis or verified package calculations. Optional `calibration/01_*.R` through `05_*.R` regenerate the worked solutions. Main Figure 1 is recomputed from closed forms; Figure 2 independently rechecks catalogue Gamma moments and tails while preserving original discrete quantiles. OSM F.3's informativeness inputs are frozen design values, not a rerun of their historical calibration search.

Known difference: main Table 2's VIF=1.25 near-universal tail is .144969 from exact parameters, rounding to .14. The locked manuscript prints .15, consistent with evaluating already rounded parameters. We show the full-precision calculation and preserve the discrepancy in `provenance/manuscript-discrepancies.csv`. No manuscript was changed by this build.
