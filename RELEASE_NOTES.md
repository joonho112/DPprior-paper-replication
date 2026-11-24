# Replication notes for arXiv v3

JoonHo Lee (jlee296@ua.edu) · The University of Alabama

The replication index follows manuscript revision 104, including its online supplement. It covers 43 tables and 19 figures. The local build generates each exhibit from its documented numerical or schematic source. [Preprint](https://arxiv.org/abs/2602.06301v3) · [Reproduction map](provenance/reproduction-map.csv)

## Computation and interpretation

- The worked example runs the published G.2 code with DPprior 2.0.0. Its 29-point grid selects λ = 0.30. A separate hard-bound fit attains a majority probability of 0.250.
- Table 2 calculates the VIF 1.25 severe-tail probability from the full-precision Gamma parameters. The value 0.144969 rounds to the 0.14 printed in this edition. The September manuscript displayed 0.15.
- The synthetic simulation has 8,607 converged main fits out of 9,000 attempts. Tables and panels use the convergence intersection specified for each comparison. The four selected CDF replays represent two data sets.
- The application results distinguish changes for the same school, study or respondent from changes at the same percentile. The Rasch comparison uses the score frequencies of 500 respondents. Its upper-tail change is comparable in size to within-prior chain-split differences.
- The public build uses synthetic-site vectors, calibration settings, and application summaries. Independent numerical checks compare the outputs with locked manuscript values and prior analysis records. Optional refits have separate input and computational requirements. [Data access](DATA_ACCESS.md) · [Verification](verification/README.md)
