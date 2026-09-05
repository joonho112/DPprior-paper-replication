# Recorded fitting checks

Author: JoonHo Lee (jlee296@ua.edu)

We ran the following checks on macOS arm64 with R 4.6.0 and the specified dependencies. Elapsed times include model construction and compilation within the driver, but exclude installation.

| Calculation | Seconds | Result |
|---|---:|---|
| Simulation: one fit, four short chains | 26.378 | Completed |
| OCRS: one fit, four short chains | 17.721 | Completed using the public-use school inputs |
| Rasch: one short chain | 62.397 | Completed; 30 retained draws and identification checks |
| Conditional density from the Rasch chain | Not separately timed | 30 by 961 values; mean grid mass .9999991 |
| Data generation | Not separately timed | 29 distinct settings checked |
| Chain identity and reuse of results | Not separately timed | Eight checks with synthetic fixtures |

All fit drivers and the Rasch combiner also passed dry runs. The four-chain Rasch combination has component tests, including the density calculation above, but has not been run with four fitted chains. Full production refits have not been repeated.

The hashes in `provenance/refit-status.csv` identify the outputs of these checks. Run IDs include source-code hashes, so editing comments or formatting gives a new ID even when calculations are unchanged. Earlier runs remain records of the code used at that time. `verification/check_refit_guards.R` checks reuse of completed results and rejection of an altered result.

Short chains test that the fitting code runs. The supplied posterior summaries come from the original analysis fits, assessed using their production schedules and diagnostic rules.
