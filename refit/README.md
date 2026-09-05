# Optional single-job refitting

Author: JoonHo Lee (jlee296@ua.edu)

Use the frozen-summary rebuild first. These drivers support a separately selected new fit. They never replace the paper's frozen results, and their default is a dry run. Install the pinned `refit` dependency profile and a working C++ toolchain before executing a model.

## Simulation

```bash
Rscript refit/simulation/run_one_fit.R
Rscript refit/simulation/run_one_fit.R --profile smoke --execute
```

Select `--fit-key` from `data-frozen/simulation/fit_status.csv`. The default selects one runnable Vague fit at J=25. `--attempt 1` uses the original initial schedule; `--attempt 2` uses the original rerun schedule. Both use the four assigned seeds in `chain_seeds.csv`. A smoke uses four chains of 200 iterations with 100 warmup iterations. The production schedule is 12,000/4,000 or 120,000/60,000, with thin 1. The DGP preserves exact component occupancy, the realized sample SD .25, and sampling variance 4/n. Run `Rscript verification/check_dgp.R` without sampling to check 29 distinct settings.

## STAR, meta-analysis and OCRS

Prepare public inputs as described in [DATA_ACCESS.md](../DATA_ACCESS.md), then select one application and prior:

```bash
Rscript refit/applications/run_one_fit.R --arm ocrs --role Vague
Rscript refit/applications/run_one_fit.R --arm ocrs --role Vague --profile smoke --execute
```

`--arm star`, `bd`, and `ocrs` select the three applications. `--fit-key` can select an exact primary or sensitivity fit from `data-frozen/applications/execution_manifest.csv`. Input parity is required before fitting. OCRS is the public-use complete-case descriptive analysis of 1,063 observations in 24 schools; it does not reproduce the original study's adjusted ITT analysis. The short profile retains the original four-chain seeds and prior settings while reducing the sampling schedule.

## Rasch

Acquire the public IRW long-table RDS separately. Its canonical 500-by-13 analysis matrix must match the recorded hash. The loader samples 500 rows from character-sorted IDs before dropping the three nonwords. It does not save response rows.

```bash
Rscript refit/rasch/run_one_chain.R --input /path/to/gcbs_brotherton_2013_vcl.rds
Rscript refit/rasch/run_one_chain.R --input /path/to/gcbs_brotherton_2013_vcl.rds --arm main --role vague --attempt 1 --chain 1 --profile smoke --execute
```

Choose an exact job from `data-frozen/rasch/chain_jobs.csv`. The sum-to-zero item constraint, item prior variance 3, DP base measure and M=100 remain fixed. A smoke uses 60 iterations, 30 warmup and thin 1. Production uses the schedule on that job's row. Third attempts are documented departures from the original two-schedule plan, not silent retries.

To combine, provide an index with columns `chain,run_directory`, containing exactly four relative paths under `outputs/refit/rasch/` for the same arm, role, attempt and profile. Example path: `outputs/refit/rasch/RUN_ID`. Do not mix earlier and later attempts.

```bash
Rscript refit/rasch/combine_and_check.R --arm main --role vague --attempt 1
Rscript refit/rasch/combine_and_check.R --arm main --role vague --attempt 1 --profile smoke --index /path/to/four-chains.csv --input /path/to/gcbs_brotherton_2013_vcl.rds --execute
```

The combiner checks hashes and settings, samples population measures conditional on saved draws, and computes model-density and score-group diagnostics. The density describes the fitted population distribution, not the empirical distribution of person posterior means. The compact combined score output contains group means; it does not claim to reproduce person credible intervals.

## Receipts, resuming and scope

Every result has an identity built from settings, data, assigned seeds, code hashes and software pins. `--resume --execute` reuses only a completed, hash-verified result with the same identity. It does not resume an interrupted chain. A failed or incomplete directory is preserved for inspection and is never overwritten automatically. Code changes generate a new identity.

The drivers save locally under `outputs/refit/`, which is excluded from release. Rasch draw files contain person-level latent draws and must remain local. Run one job per process; collect completed receipts explicitly. No scheduler, cloud credentials or automatic fleet launch is included. For long production jobs, submit individual commands to your own execution system and collect their receipts before combining.

See [refit-status.csv](../provenance/refit-status.csv) and [the smoke record](../verification/refit-smoke.md). We tested one simulation fit, one OCRS fit, one Rasch chain, conditional density extraction and eight guard checks. The four-real-chain Rasch combiner has not been executed end to end. These tests establish an executable path; they do not establish production convergence or numerical identity with the paper.
