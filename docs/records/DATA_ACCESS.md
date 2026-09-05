# Data access

Author: JoonHo Lee (jlee296@ua.edu)

The default replication path uses the smallest frozen summaries needed to rebuild the paper's results. It does not download data or fit models during setup. The optional data-preparation and refit paths obtain source data separately. Their implementation and execution status are recorded in the replication guide.

## Project STAR

The analysis uses `mlmRev::star`, with the kindergarten complete-case and classroom-dependence rules documented in the paper. We do not redistribute the pupil-level source table. Obtain it through **mlmRev 1.0-9**; the loader must check its version, schema, sample counts, and prepared-data fingerprint before fitting. The package is distributed under **GPL (>= 2)**; this is a package-license record, not a new license assigned to the original study data. See the [maintainer's DESCRIPTION](https://github.com/annahuynhly/mlmRev/blob/main/DESCRIPTION) and [STAR documentation](https://cran.r-universe.dev/mlmRev/doc/manual.html#star).

Frozen public results are school/study-model aggregates and the printed location profiles. Reproducing the first stage requires obtaining the source package. The primary first stage includes classroom dependence; the schoolwise HC3 sensitivity is a separate analysis.

## Writing-to-learn meta-analysis

Obtain `metadat::dat.bangertdrowns2004` from **metadat 1.6-0**, the version used in the analysis. The dataset contains 48 study-level effects. Its bias-corrected standardized mean differences and sampling variances follow the package's construction; the latter assume equal group sizes because the original article reports total sample size. See the [dataset documentation](https://wviechtb.github.io/metadat/reference/dat.bangertdrowns2004.html).

The package is distributed under **GPL (>= 2)** ([DESCRIPTION](https://github.com/wviechtb/metadat/blob/master/DESCRIPTION)). Its current development version differs from the study version, so an unpinned installation is not an exact-data guarantee. The replication loader reads and checks the source package table. The reviewed study-level analysis input is included with its source attribution and upstream terms; it is not relicensed under the repository's code license.

## Vocabulary checklist

The source is Item Response Warehouse table `gcbs_brotherton_2013_vcl`: vocabulary checklist responses collected alongside the 2016 Open-Source Psychometrics Project Generic Conspiracist Beliefs Scale survey. The 2013 reference names the scale, not the year these responses were collected. The [original data index](https://openpsychometrics.org/_rawdata/) lists 2,495 respondents for the October 2016 survey.

The [IRW Data Dictionary](https://docs.google.com/spreadsheets/d/1nhPyvuAm3JO8c9oa1swPvQZghAvmnf4xlYgbvsFH99s/edit?gid=0), checked on 2026-09-04, marks this table **Public**, with **CC BY 4.0** as its derived license and **Missing (NA)** as its original-license field. We report both fields. The original-license field does not mean that the data are in the public domain. No response-level IRW data, original respondent IDs, ages, or person posterior rows are redistributed here.

The frozen calculation path uses 14 raw-score frequencies, 13 item summaries, six fixed item-difficulty vectors, and fit/score-level model summaries. Attribute the Open-Source Psychometrics Project for collection, IRW for distribution, and Brotherton, French, and Pickering (2013) for the accompanying scale. Our modifications are the fixed 500-person sample, removal of nonwords VCL6/VCL9/VCL12 after sampling, and aggregation. Retain this attribution and the [CC BY 4.0 terms](https://creativecommons.org/licenses/by/4.0/) for the IRW-derived material, subject to upstream rights.

For a full refit, obtain the IRW table through its [documented access route](https://itemresponsewarehouse.org/metadata.html). Access may require an account. Provide the retrieved file to the local loader; do not place credentials in scripts. The analysis expects 2,495 by 16 complete responses before sampling and 500 by 13 afterward. The canonical historical long-table RDS has SHA-256 `431ce8ff8efe821c26cd078ece0e3e9f08c5bab0c2a0f630dd1ecf06d583e621`. A newly serialized equivalent table may have different bytes; the loader must distinguish byte identity from a verified canonical-content match. Sample string-sorted IDs with seed 81770971 under the stated R RNG settings; integer sorting produces a different sample.

Conditional WLE reliability holds the exported item difficulties fixed. It does not refit those difficulties or propagate their uncertainty.

## Online Credit Recovery Study (OCRS)

OCRS has an official **public-use data release** from the American Institutes for Research, deposited as **ICPSR 202181, version V1**, released June 28, 2024. The [AIR project page](https://www.air.org/project/assessing-efficacy-online-credit-recovery-student-learning-and-high-school-graduation) and [IES award page](https://ies.ed.gov/use-work/awards/assessing-efficacy-online-credit-recovery-student-learning-and-high-school-graduation) confirm public-use availability. Obtain `OCRS_publicuse_impactanalysisdata.csv` from the [official deposit](https://www.openicpsr.org/openicpsr/project/202181/version/V1/view), with its public-use documentation. We accept an explicit local path to that file; no private research path is built into the loader.

The paper's descriptive analysis uses three fields: `sts_irtscore`, `ragrp`, and `studyschids`. From 1,737 source observations, it retains 1,063 with nonmissing score and assignment, standardizes the score across retained observations, and regresses that score on assignment within each of 24 schools. Schools need at least ten observations and both assignment groups. These steps reproduce the existing analysis input to numerical precision.

The unit is a source observation, not necessarily a unique student across subjects: the release documentation distinguishes 1,737 subject-level observations from 1,683 unique students. Our complete-case, schoolwise descriptive contrast is **not a reconstruction of the original study's blocked ITT analysis**. Preserve this distinction when interpreting the example.

The package includes an explicit source-to-school-input preparation path, selected school/model summaries, posterior aggregate rebuilding, and an optional single-model refit path. Actual refit execution is labeled separately from input preparation and frozen-summary rebuilding. Public-use availability is not itself a new MIT or CC BY grant for the original files. We link to the official deposit rather than duplicate student-level files or the copyrighted AIR documentation. Only the impact analysis CSV is required for this paper; the class, survey, and test response files are not needed.

Run the implemented OCRS preparation path with:

```sh
Rscript applications/05_prepare_ocrs.R --input "/your/data/OCRS_publicuse_impactanalysisdata.csv"
```

It writes a 24-school aggregate and a source/preparation receipt. Rebuilding the frozen OCRS posterior table uses `applications/03_summaries.R` and `06_supplemental.R` and does not require redownloading the raw CSV. A successful source-preparation receipt does not imply a new posterior refit.

## Scope of the license record

These are source-specific records checked for this package, not a blanket license for all source materials. `provenance/data-rights.csv` records the evidence and public unit; `provenance/withholding.csv` records exclusions. `LICENSE-SCOPE.md` separates code, original documentation, synthetic outputs, and material carrying upstream terms.
