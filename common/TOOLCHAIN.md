# Runtime for the 104 edition

The full rebuild uses R 4.6.0, a project R library, Python, pdfLaTeX and Poppler. Install the command-line programs before running setup. The tested macOS installation used TeX Live 2025, Poppler 26.10.0, Fontconfig 2.18.3 and Quarto 1.9.37. TeX Live needs the standalone, graphics, pgf, amsmath, amsfonts, psnfss and lato packages. Make sure `pdflatex`, `kpsewhich`, `fc-match`, `pdftocairo` and `pdftoppm` are on PATH. The guide also needs `quarto`.

Run these commands in the repository root. For the Python environment, use Python 3.9–3.13 with a wheel for Pillow 11.3.0. The verified macOS run used Python 3.9.6. The commands below use Python 3.13 as an example.

~~~sh
python3.13 -m venv .venv
.venv/bin/python3 -m pip install -r common/figure-requirements.txt
Rscript common/install_dependencies.R --profile rebuild --execute
Rscript 00_setup.R --profile rebuild
~~~

Python and R installation download packages. Subsequent builds and verification can run offline. Exact versions for the renderer are ReportLab 4.5.1, Pillow 11.3.0 and charset-normalizer 3.4.7. To use an already installed environment, set `DPP_PYTHON` to its interpreter. The plotting tools use the Lato Regular and Bold faces. The runtime checks each TTF against its SHA-256 and tries Fontconfig followed by TeX Live. An installation with the same font files may supply their directory in `DPP_LATO_DIR`.

The R installer restores the locked dependency closure to `.library/`. Source builds need an R-compatible compiler and the system libraries for ragg, systemfonts and textshaping, including FreeType, HarfBuzz, FriBidi, libpng, libtiff and JPEG. The rebuild profile includes DPprior 2.0.0 at commit `16919d3a36f97cad1d257c3610d9e022ecf0f0ab`. Setup checks the project installation and 820 function definitions against the recorded production definitions. To run the manuscript G.2 listing directly in an interactive R session, use `.libPaths(c(normalizePath(".library"), .libPaths()))` before running it from the repository root.

The calibration profile checks only the R calculation stack. The public-data profile checks the packages for first-stage application processing. For an optional Rasch refit, the refit profile also checks DPMirt 0.2.0 at `acde59367a4992b9642fd18beb7b706a735ff216`, its 208 pinned R functions, and NIMBLE 1.4.2. The book profile checks Quarto. Run the corresponding installer and setup commands with the desired profile name.

~~~sh
Rscript common/install_dependencies.R --profile calibration --execute
Rscript 00_setup.R --profile calibration
python3 common/figure_runtime.py
~~~

## Test a fresh directory

A reviewed source checkout includes the exact file inventory in `provenance/release-allowlist.csv`. The following commands export that inventory into separate empty directories with spaces in their names.

~~~sh
python3 verification/cold_start.py --execute
python3 verification/cold_start.py --execute --fresh-install --python python3.13
~~~

The default test copies the 27 locked R packages and the three pinned Python distributions into isolated libraries. The fresh-install test downloads the exact pins. It prefers matching CRAN macOS binaries when available and falls back to the specified source version. DPprior is installed from its locked GitHub commit. To use this faster installation route directly, add `--prefer-binaries` to the R installer command. The default installer uses source packages.

The test renders and checks the committed guide assets, runs the build and numerical verification, then explicitly syncs and checks a newly rendered guide. It measures command elapsed time and the OS-reported peak resident memory. Receipts are written to `outputs/verification/cold_start_cached.json` and `outputs/verification/cold_start_fresh.json`. Installation requires network access. The subsequent execution denies access to the source checkout and network on macOS with sandbox-exec, or on Linux with bubblewrap and user namespaces. The test probes both restrictions before it runs. It also hides globally installed R packages beyond the shipped base and recommended libraries. Use `--allow-unsandboxed` on a host without either isolation facility to record the limits of that test.
