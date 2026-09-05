# Rebuild simulation summaries

Author: JoonHo Lee (jlee296@ua.edu)

This path reads frozen summaries from completed synthetic fits. It recalculates averages, uncertainty summaries, paired contrasts and plotting frames; it does not run MCMC. See `data-frozen/simulation/CODEBOOK.md` for the units and status definitions.

```sh
Rscript 00_setup.R --profile rebuild
Rscript simulation/01_reaggregate.R
Rscript simulation/02_primary_and_paired.R
Rscript simulation/03_misspecification.R
Rscript simulation/04_design_size.R
Rscript simulation/05_unequal_weights.R
Rscript simulation/06_fixed_lambda.R
Rscript verification/check_simulation.R
```

Run these from the repository root. Outputs are written under `data-derived/simulation/` and receipts under `outputs/`. The full computation takes seconds to minutes, depending on CSV reading and the machine. Frozen source hashes are checked before the compact inputs are joined.

1. **Reaggregate:** rebuild all 322 cells from 57,151 fit rows; reconcile 64,400 terminal statuses against the original denominators; compare all 63 cell-summary columns with the frozen manuscript reference.
2. **Primary and paired:** retain the 45 main cells under grand-mean variance 1, with 8,607 admitted fits from 9,000 planned. Compare the three grand-mean variances using matching replicates and data hashes. A pair is used only when both fits were admitted.
3. **Misspecification:** preserve all 60 medium-confidence comparison cells, including four calibration exclusions; expose all 14 calibration-blocked cells across the full design. Complete trajectories are selected for connected plots so a missing endpoint is not silently bridged.
4. **Design size:** retain complete trajectories across J=25,50,100,200 and separately report all J=200 cells. Some DORO elicited-mean variants share the same prior/count/informativeness combination at J=100, so row counts and distinct combinations differ.
5. **Unequal weights:** compare central and balanced-random locations with an exact .60 dominant allocation specification. This step verifies the design fields and rebuilds summaries; the optional DGP path verifies generated allocation counts.
6. **Fixed lambda:** compare the policy-selected value with .5 and .9 at two informativeness levels.

`aggregation.R` retains the numerical aggregation and subset-selection functions from the manuscript. The public adapter replaces archive path discovery with explicit hashed inputs. No reference result is used to compute a new estimate: files under `verification/expected/` are read only for comparison. Figure rendering is handled by the exhibit build, separately from these numerical steps.
