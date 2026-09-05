# Publication boundary

Author: JoonHo Lee (jlee296@ua.edu)

We build this package from an explicit file list. Research directories, old replication archives, and private execution logs are not copied wholesale.

| Material | Public treatment | Reproduction supported |
|---|---|---|
| Synthetic simulation fit metrics and terminal status | Export scientific keys, seeds, metrics, and failure categories; remove machine paths and operational metadata | Reaggregation, denominators, paired comparisons |
| Calibration settings and prior summaries | Export numeric targets, shape–rate parameters, full candidate grid, and scientific acceptance status | Fresh calibration and independent diagnostics |
| STAR and writing-to-learn results | Export selected fit-level and profile aggregates; retrieve original inputs from their documented packages when requested | Frozen-result rebuild; optional data preparation and fitting |
| Vocabulary checklist | Export score frequencies, item summaries and fixed item estimates, fit/score-level posterior summaries, count PMFs and model-density grids | Conditional WLE calculation and frozen-result rebuild |
| OCRS public-use release | Retrieve official impact CSV separately; export needed school/model summaries with source attribution | Source-to-input calculation, frozen posterior rebuild, optional refit |
| Unnecessary person rows, ages, response patterns, and pairwise allocations | Withhold | Explicitly outside public snapshot |
| Journal manuscript PDFs, submission correspondence, reviewer archive, OCR literature, private logs, cloud setup, installed R libraries | Exclude | Not required to run this code package |

Every exported data file needs a column allowlist, data-unit description, source fingerprint, and a corresponding row in `provenance/release-allowlist.csv`. An author-created identifier is not enough to make a person-level file necessary or suitable for release. In this Rasch model a person's posterior mean can identify that person's raw score; use score-group aggregates instead.

The public guide is an English Quarto Book under `docs/`. The separate Korean execution record under `log/` is private and excluded from both Git and Pages. Public output metadata uses package-relative paths and scientific source IDs.

Status labels distinguish `recalculated`, `rebuilt-from-frozen`, `refit-tested`, `reference-only`, and `withheld`. Unavailable results stay in the artifact contract. A fit that failed diagnostics is not relabeled converged because it was retained for a sensitivity display.
