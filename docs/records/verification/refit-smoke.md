# Optional fitting checks

The November local run used macOS arm64 and R 4.6.0. The `rebuild` profile uses frozen summaries. A user can choose an individual MCMC job through the [refit drivers](../refit/README.md).

## Selected production replay

The four recorded fits in Figure F.5 and Table F.15 were each rerun with their original terminal attempt, four original chain seeds, and synthetic 100-site data. Each passed the original diagnostic gate. Compared with the archived replay, the maximum absolute difference across the pooled posterior CDF and GR ISEL/KS losses was 3.34 × 10⁻¹⁶.

| Generating count | Prior | Attempt | Joint draws | Seconds |
|---:|---|---:|---:|---:|
| 5 | TSMM | 1 | 32,000 | 45.598 |
| 5 | Dual-Anchor | 2 | 240,000 | 119.491 |
| 30 | Gamma(1,1) | 1 | 32,000 | 36.536 |
| 30 | TSMM | 2 | 240,000 | 244.316 |

The [four-fit record](../provenance/selected-replay-verification.csv) gives exact fit keys, convergence, result hashes and numerical differences. Elapsed times include the local execution of the selected terminal schedule. Grid-wide results use the original accepted records.

## Dry runs and software checks

The November run checked the default core environment plus rebuild, calibration, public-data, refit and book profiles. All five single-job interfaces described their selected work without sampling. The Rasch chain dry run used the separately acquired matching IRW source. Omitting that source produced an explicit request for `--input`. Executing the combiner without its four-chain index and original input produced a request for `--index` and `--input`.

A fresh simulation smoke ran one synthetic four-chain fit in 18.904 seconds. The eight synthetic guard checks exercised missing, duplicated or incompatible chains, repeated seeds, nonfinite draws, completed-result reuse and a corrupted result hash. The earlier September checks included the OCRS four-chain smoke (17.721 s), one Rasch short chain (62.397 s), and conditional population-density extraction on 30 retained draws. End-to-end combination of four newly fitted Rasch chains remains untested. See the [refit status ledger](../provenance/refit-status.csv) for dates, scopes and result hashes.
