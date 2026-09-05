# Verification

Author: JoonHo Lee (jlee296@ua.edu)

After rebuilding the outputs, run `Rscript verify_reproduction.R`. We compare 255 simulation columns and 290 application columns with independently stored results, check the WLE, score and density calculations, and compare 68 selected numbers. All 41 tables and figures have input/output checks. OSM Table F.7 is also compared with the 36 printed reference rows.

`python3 verification/check_book.py` checks the static examples, copied scripts and local links. Numerical data and figure geometry are checked separately from PDF creation metadata, which can vary between runs.

`python3 verification/negative_controls.py` introduces five numerical errors and tests six disclosure cases, including a clean control. The altered copies are temporary; the original expected values remain unchanged.

`sh verification/cold_start.sh --execute` rebuilds the package in a separate directory using only the public file list and a cache of 26 installed dependencies. Personal R profiles are disabled. On macOS, the original home directory and network are also unavailable during the test. The command checks setup, the numerical build, verification and guide rendering. It does not install dependencies from source.

`python3 verification/disclosure_scan.py /path/to/candidate` compares the files with the allowlist and inspects CSV units, text, PDF and image metadata, symlinks and archive contents. Use `--site-only` to check the rendered guide. The public files contain aggregate analysis inputs; respondent records and individual latent draws remain outside the package.

The [validation record](../provenance/validation-summary.json) gives the recorded checks, and the [platform record](../provenance/platform-matrix.csv) lists tested environments. The [Table 2 note](../provenance/manuscript-discrepancies.csv) retains the rounding difference. Full production posterior fits have not been repeated for this package.
