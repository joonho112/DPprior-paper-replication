# Replication notes

Author: JoonHo Lee (jlee296@ua.edu)

These materials accompany the [prior-elicitation preprint](https://arxiv.org/abs/2602.06301). The repository includes the simulation extensions and applications described in the guide. Its table and figure index follows the accompanying analyses.

## Calculations

The standard build creates 41 tables and figures and checks 68 selected numbers. It reaggregates simulation fit metrics and reconstructs comparisons from the supplied application summaries. Prior calibration and weighted-likelihood scoring can be repeated separately. Model-fitting scripts support individual synthetic and application fits.

## Verification

The numerical rebuild and guide have been checked on macOS arm64. An isolated run used only the public files and a cache of 26 installed dependencies, with the original home directory and network unavailable. Eleven deliberate-error and control cases also passed. This checks independence from the research workspace; it does not test installation from source in a new environment.

Short fitting runs cover a simulation model, an OCRS model and one Rasch chain. The four-chain Rasch combination has component and dry-run checks. Full production refits have not been repeated for this package. Linux workflows are supplied; their execution status is listed in [the platform record](provenance/platform-matrix.csv).

## Numerical note

For Table 2, VIF=1.25, the tail probability calculated from unrounded parameters is .144969 and rounds to .14. The printed reference gives .15. The rebuilt table uses .14, as recorded in [the comparison](provenance/manuscript-discrepancies.csv).

## Source data

The [data instructions](DATA_ACCESS.md) describe access to STAR, writing-to-learn, OCRS and vocabulary responses. The repository contains the summaries needed for reconstruction. Student records, individual latent draws and local execution outputs remain outside the distributed files.
