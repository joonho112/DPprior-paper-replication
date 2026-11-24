# Rebuild the manuscript exhibits

From the package root, run the environment check and the exhibit build.

```sh
Rscript 00_setup.R --profile rebuild
Rscript exhibits/00_build_all.R
Rscript verify_reproduction.R
```

The November manuscript has 43 tables and 19 figures. Each ID in the [exhibit contract](../provenance/artifact-contract.csv) identifies a CSV in `outputs/v104/` and two display files. Tables have HTML and LaTeX versions. Figures have PDF and PNG versions at the sizes used by their renderers. The worked G.2 example also produces `outputs/v104/G2-example.json`. The [reproduction map](../provenance/reproduction-map.csv) connects each printed number and LaTeX label to its runner, inputs, and verification.

Main Table 3 and Figure 4 use the synthetic fit records and site vectors. Main Table 4 and Figures 5–6 use public application summaries. Each exhibit receipt in `outputs/exhibit-receipts/` records its reproduction tier, input hashes, numeric frame hash and the hashes of its two display files. The contract distinguishes definition tables, source artwork, numerical recomputations and summaries of original model fits.

The simulation illustrations use 1,800 planned data sets, with figure-specific convergence sets. Figure 5 compares changes in individual means with changes at the same percentile of the finite-effect distribution. Figure 6 compares a fixed set of upper and middle units under two priors. The [simulation guide](../simulation/v104/README.md) and [application guide](../applications/v104/README.md) describe the populations and units. Documented differences from the manuscript have a [separate record](../provenance/manuscript-discrepancies.csv).
