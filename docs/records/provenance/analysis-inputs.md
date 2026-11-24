# Inputs for the November preprint

The frozen inventory records a SHA-256 digest for each file, its byte count and CSV fields. Run `python3 verification/check_inputs.py` before rebuilding. Use the per-track contracts to trace an individual input and check its key. The data sources and redistribution terms are documented in [Data access](../DATA_ACCESS.md).

| Material | Public input and unit | Calculations supported |
|---|---|---|
| Generated studies | The site-effects directory contains fit keys, synthetic site vectors and known effects for matched generated datasets. The September disposition ledger also covers failed fits. | Fit-level recovery and percentile error, paired changes and common-convergence comparisons. Direct posterior CDF replay covers two datasets and four fits. |
| Project STAR and writing-to-learn | Reported estimates, fit summaries, percentile bins, aligned school or study summaries and sharing probabilities. Tail membership comes from observed-effect ranks. | Finite-distribution changes, unit changes and co-clustering. For the population densities the six dedicated density runs supply their own grids and diagnostics. |
| Vocabulary checklist | Score frequencies and score-level posterior means, finite-CDF summaries and a population density grid. The individual response matrix, respondent identifiers and posterior allocations stay with the source custodian. | Score-frequency-weighted changes describe the 500 respondents. Inverting the posterior mean finite CDF gives percentiles on the logit scale. |
| Calibration and alternatives | Analytic inputs, hyperprior settings, policy and design summaries, including the wider scale-prior fits and the OCRS descriptive example. | Count moments, tail probabilities, verification of the chosen grid value and sensitivity checks. |

The input files support a rebuild from the stated level of aggregation. The path from separately obtained observations to fitted draws is described under [Data access](../DATA_ACCESS.md) and [refit](../refit/README.md). Both approaches retain the upstream attribution and license terms.
