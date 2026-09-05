# How the original computation becomes a public single-job pipeline

Author: JoonHo Lee (jlee296@ua.edu)

The original study separated prior calibration, semantic data/fit/chain identities, data generation, fitting, diagnostics, collection and publication summaries. The public package preserves those scientific boundaries while replacing private job discovery with explicit inputs. `provenance/code-port-map.csv` records the source-file fingerprints and function/section decisions.

| Stage | Public role | What is preserved |
|---|---|---|
| Calibration | `calibration/` | Fixed policy, numerical verification, typed boundaries and full lambda grid |
| Synthetic DGP | `refit/simulation/dgp.R` | Exact component counts, dominant location, effect rescaling and sampling variance |
| Seeds and identities | `refit/simulation/keys.R` and frozen job manifests | Domain-separated semantic keys and original manifest-assigned seeds |
| Simulation model | `refit/simulation/model.R` | NIMBLE likelihood, DP base measure, four overdispersed initializations |
| Application model | `refit/applications/model.R` | Same fitting family with stated scale choices; occupied/residual population-weight distinction |
| Diagnostics | simulation/application modules; Rasch combine driver | Original R-hat/ESS thresholds, finite/capacity checks and typed failure |
| Rasch chain | `refit/rasch/` public adapter | Pinned DPMirt invocation, fixed prior/base measure, identification and capacity |
| Collection | frozen model/fit summaries | Original admitted-result evidence; new smoke is never substituted for the paper |
| Publication | `simulation/`, `applications/`, `rasch/`, `exhibits/` | Reaggregation, stated denominators and role/estimand distinctions |

Full original runner files are reference material for lifecycle and schema decisions; they are not copied wholesale into the public tree. Hostnames, account settings, private path discovery, cloud scheduling, sync manifests and automated retry orchestration are excluded. The scientific model modules do not initiate a job when sourced. Public drivers default to dry-run and require an explicit execution request.

The completed study's results and a new local job have separate provenance. A short smoke tests compilation, sampling and output shape only; it cannot establish convergence or manuscript agreement. Production schedules use the recorded four-chain seeds/settings. A different schedule has a different run identity, and an existing incompatible output must not be reused as a resumed result.
