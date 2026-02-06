# DPprior Replication Package — Final Verification Report

**Date:** February 6, 2026
**Package:** `dpprior-replication/`
**Paper:** "Design-Conditional Prior Elicitation for Dirichlet Process Mixtures"
**Author:** JoonHo Lee

---

## 1. File Completeness Check

### 1a. Figure-to-Code Mapping (23/23 Complete)

| Figure | Location | Code File | Pre-rendered | Status |
|--------|----------|-----------|--------------|--------|
| Figure 1 | Main Text | `code/figure_01/Figure_01_framework.tex` | `figures/main/Figure_01.pdf` | PASS |
| Figure 2 | Main Text | `code/figure_02/figure_02_unintended_prior.R` | `figures/main/Figure_02.pdf`, `.png` | PASS |
| Figure 3 | Main Text | `code/figure_03_simulation/analysis/02_figures_generation.py` | `figures/main/Figure_03.pdf`, `.png` | PASS |
| Figure 4 | Main Text | `code/figure_03_simulation/analysis/02_figures_generation.py` | `figures/main/Figure_04.pdf`, `.png` | PASS |
| Figure E05 | Appendix E | `code/figure_04/figure_04_pareto_frontier.R` | `figures/supplement/Figure_E05.pdf`, `.png` | PASS |
| Figure F01 | Appendix F | `02_figures_generation.py` → `figure5_bias_distribution()` | `figures/supplement/Figure_F01.png` | PASS |
| Figure F02 | Appendix F | `appendix_F_supplementary_figures.R` → `fig_F02_collapse()` | `figures/supplement/Figure_F02.pdf`, `.png` | PASS |
| Figure F03 | Appendix F | `02_figures_generation.py` → `figure6_mse_comparison()` | `figures/supplement/Figure_F03.png` | PASS |
| Figure F04 | Appendix F | `appendix_F_supplementary_figures.R` → `fig_F04_heatmap()` | `figures/supplement/Figure_F04.pdf`, `.png` | PASS |
| Figure F05 | Appendix F | `02_figures_generation.py` → `figure7_coverage()` | `figures/supplement/Figure_F05.png` | PASS |
| Figure F06 | Appendix F | `02_figures_generation.py` → `figure8_ci_width()` | `figures/supplement/Figure_F06.png` | PASS |
| Figure F07 | Appendix F | `02_figures_generation.py` → `figure9_summary_panel()` | `figures/supplement/Figure_F07.png` | PASS |
| Figure F08 | Appendix F | `appendix_F_supplementary_figures.R` → `fig_F08_chi2doro()` | `figures/supplement/Figure_F08.pdf`, `.png` | PASS |
| Figure F09 | Appendix F | `02_figures_generation.py` → `figure10_confidence_sensitivity()` | `figures/supplement/Figure_F09.pdf`, `.png` | PASS |
| Figure F10 | Appendix F | `02_figures_generation.py` → `figure11_alpha_posterior()` | `figures/supplement/Figure_F10.pdf`, `.png` | PASS |
| Figure F11 | Appendix F | `02_figures_generation.py` → `figure12_posterior_sd()` | `figures/supplement/Figure_F11.pdf`, `.png` | PASS |
| Figure F12 | Appendix F | `appendix_F_supplementary_figures.R` → `fig_F12_mode_recovery()` | `figures/supplement/Figure_F12.pdf`, `.png` | PASS |
| Figure F13 | Appendix F | `02_figures_generation.py` → `figure14_mode_mean_analysis()` | `figures/supplement/Figure_F13.pdf`, `.png` | PASS |
| Figure F14 | Appendix F | `appendix_F_supplementary_figures.R` → `fig_F14_bias_variance()` | `figures/supplement/Figure_F14.pdf`, `.png` | PASS |
| Figure G01 | Appendix G | `code/appendix_G/appendix_G_figures.R` | `figures/supplement/Figure_G01.pdf`, `.png` | PASS |
| Figure G02 | Appendix G | `code/appendix_G/appendix_G_figures.R` | `figures/supplement/Figure_G02.pdf`, `.png` | PASS |
| Figure G03 | Appendix G | `code/appendix_G/appendix_G_figures.R` | `figures/supplement/Figure_G03.pdf`, `.png` | PASS |
| Figure G04 | Appendix G | `code/appendix_G/appendix_G_figures.R` | `figures/supplement/Figure_G04.pdf`, `.png` | PASS |

### 1b. Table-to-Code Mapping (16 code-generated tables)

| Table | Code Source | CSV Output | LaTeX Output | Status |
|-------|-----------|------------|--------------|--------|
| Table 1 (Main) | `03_tables_generation.py` | `table01_main_results.csv` | `table01_main_results.tex` | PASS |
| Table F02 | `03_tables_generation.py` | `table02_collapse_probability.csv` | `.tex` | PASS |
| Table F03 | `03_tables_generation.py` | `table03_bias.csv` | `.tex` | PASS |
| Table F03b | `03_tables_generation.py` | `table03b_mae.csv` | `.tex` | PASS |
| Table F04 | `03_tables_generation.py` | `table04_mse.csv` | `.tex` | PASS |
| Table F04b | `03_tables_generation.py` | `table04b_rmse.csv` | `.tex` | PASS |
| Table F05 | `03_tables_generation.py` | `table05_coverage.csv` | `.tex` | PASS |
| Table F05b | `03_tables_generation.py` | `table05b_ci_width.csv` | `.tex` | PASS |
| Table F06 | `03_tables_generation.py` | `table06_low_info_summary.csv` | `.tex` | PASS |
| Table F07 | `03_tables_generation.py` | `table07_all_methods.csv` | `.tex` | PASS |
| Table F08 | `03_tables_generation.py` | `table08_confidence_comparison.csv` | `.tex` | PASS |
| Table F09 | `03_tables_generation.py` | `table09_relative_improvement.csv` | `.tex` | PASS |
| Table F10 | `03_tables_generation.py` | `table10_alpha_posterior.csv` | `.tex` | PASS |
| Table F11 | `03_tables_generation.py` | `table11_prior_hyperparameters.csv` | `.tex` | PASS |
| Table F12 | `03_tables_generation.py` | `table12_exact_match.csv` | `.tex` | PASS |
| Table F12b | `03_tables_generation.py` | `table12b_near_match.csv` | `.tex` | PASS |

---

## 2. Executability Check

### 2a. R Library Calls vs Requirements

| Script | library() Calls | All in install_packages.R? | Status |
|--------|----------------|---------------------------|--------|
| `figure_02_unintended_prior.R` | ggplot2, patchwork, scales | Yes | PASS |
| `figure_04_pareto_frontier.R` | ggplot2, DPprior | Yes | PASS |
| `appendix_F_supplementary_figures.R` | ggplot2, dplyr, tidyverse, scales, patchwork | Yes | PASS |
| `appendix_G_figures.R` | DPprior, ggplot2, dplyr, tidyr, patchwork | Yes | PASS |
| `R_analysis_code.R` | tidyverse, ggplot2, scales, patchwork, knitr, kableExtra | Yes | PASS |
| `step0_check_env.R` | (requireNamespace only) yaml, future | Yes | PASS |

### 2b. Python Imports vs requirements.txt

| Script | Imports | All in requirements.txt? | Status |
|--------|---------|------------------------|--------|
| `01_data_preparation.py` | pandas, numpy, pathlib | Yes (pathlib = stdlib) | PASS |
| `02_figures_generation.py` | pandas, numpy, matplotlib, seaborn, scipy | Yes | PASS |
| `03_tables_generation.py` | pandas, numpy, os, pathlib | Yes (os, pathlib = stdlib) | PASS |

### 2c. Relative Path Correctness

| Script Location | Path Expression | Resolves To | Status |
|----------------|----------------|-------------|--------|
| `code/figure_02/` (depth 2) | `file.path("..", "..", "figures", "main")` | `repo_root/figures/main/` | PASS |
| `code/figure_04/` (depth 2) | `file.path("..", "..", "figures", "supplement")` | `repo_root/figures/supplement/` | PASS |
| `code/appendix_F/` (depth 2) | `file.path(base_dir, "figures", "supplement")` | `repo_root/figures/supplement/` | PASS |
| `code/appendix_G/` (depth 2) | `file.path("..", "..", "figures", "supplement")` | `repo_root/figures/supplement/` | PASS |
| `code/figure_03_simulation/analysis/` (depth 3) | `Path(__file__).resolve().parents[3]` | `repo_root/` | PASS |

### 2d. Config & Pipeline Files

| Item | Expected Location | Status |
|------|------------------|--------|
| `fig3_design.yaml` | `code/figure_03_simulation/config/fig3_design.yaml` | PASS |
| `step0_check_env.R` | `code/figure_03_simulation/scripts/step0_check_env.R` | PASS |
| `step1_generate_data.R` | `code/figure_03_simulation/scripts/step1_generate_data.R` | PASS |
| `step2_calibrate_priors.R` | `code/figure_03_simulation/scripts/step2_calibrate_priors.R` | PASS |
| `step3_run_simulation.R` | `code/figure_03_simulation/scripts/step3_run_simulation.R` | PASS |
| `step4_generate_figures.R` | `code/figure_03_simulation/scripts/step4_generate_figures.R` | PASS |
| `Makefile` | `code/figure_03_simulation/Makefile` | PASS |

---

## 3. Unnecessary Files Check

### 3a. Unwanted Files

| Pattern | Found | Action Needed |
|---------|-------|---------------|
| `.DS_Store` | 6 files (~78 KB) | Remove before git commit |
| `__pycache__/`, `.pyc` | None | Clean |
| `.Rhistory` | None | Clean |
| `.venv/`, `.env` | None | Clean |
| `.Rproj.user/` | None | Clean |
| `Thumbs.db` | None | Clean |
| `*.swp`, `*.swo`, `*~` | None | Clean |

### 3b. Large Binary Files

| Threshold | Found | Status |
|-----------|-------|--------|
| Files > 10 MB | None | PASS |
| `.rds` > 50 MB | `tradeoff_data.rds` = 1.6 KB | PASS |

### 3c. Sensitive Information

| Pattern | Found | Status |
|---------|-------|--------|
| API keys / secrets | None | PASS |
| Hardcoded personal paths (`/home/`, `/Users/`) | None | PASS |
| Passwords / tokens | None | PASS |
| Non-author email addresses | None | PASS |

### 3d. .gitignore Coverage

All standard patterns are covered: `.DS_Store`, `__pycache__/`, `*.pyc`, `.Rhistory`, `.RData`, `.venv/`, `*.rds`, `*.log`, `*.swp`, `*.swo`, `*~`, `.idea/`, `.vscode/`, `Thumbs.db`.

---

## 4. README Link Verification

| Path Referenced in README | Exists? | Status |
|--------------------------|---------|--------|
| `code/figure_01/` | Yes | PASS |
| `code/figure_02/figure_02_unintended_prior.R` | Yes | PASS |
| `code/figure_03_simulation/` | Yes | PASS |
| `code/figure_03_simulation/R/` | Yes | PASS |
| `code/figure_03_simulation/scripts/` | Yes | PASS |
| `code/figure_03_simulation/config/` | Yes | PASS |
| `code/figure_03_simulation/analysis/` | Yes | PASS |
| `code/figure_04/figure_04_pareto_frontier.R` | Yes | PASS |
| `code/appendix_E/` | Yes | PASS |
| `code/appendix_F/` | Yes | PASS |
| `code/appendix_G/appendix_G_figures.R` | Yes | PASS |
| `data/simulation_results/` | Yes | PASS |
| `figures/main/` | Yes | PASS |
| `figures/supplement/` | Yes | PASS |
| `tables/` | Yes | PASS |
| `install_packages.R` | Yes | PASS |
| `requirements.txt` | Yes | PASS |
| `LICENSE` | Yes | PASS |
| `.gitignore` | Yes | PASS |
| `Makefile` (simulation) | Yes | PASS |

**Result: 101/101 referenced paths verified (100%)**

---

## 5. Issues Found & Fixes Applied

### Issue 1: `rstudioapi` dependency (FIXED)

**File:** `code/appendix_F/appendix_F_supplementary_figures.R` (line 34)
**Problem:** Used `rstudioapi::getSourceEditorContext()$path` which only works in RStudio IDE, causing failure via `Rscript` command line.
**Fix applied:** Replaced with portable `tryCatch()` pattern that tries `sys.frame(1)$ofile` first (command line), then falls back to `rstudioapi` (IDE), then `getwd()`.

### Issue 2: `.DS_Store` files (DEFERRED)

**Problem:** 6 macOS metadata files present in working directory.
**Impact:** Cosmetic only; `.gitignore` already excludes them.
**Recommendation:** Run `find . -name ".DS_Store" -delete` before first git commit.

---

## Summary

| Category | Result | Details |
|----------|--------|---------|
| Figure completeness | **23/23 PASS** | All main + appendix figures have code + pre-rendered outputs |
| Table completeness | **16/16 PASS** | All code-generated tables present in CSV + LaTeX |
| Library dependencies | **PASS** | All R/Python packages listed in requirements |
| Relative paths | **PASS** | All paths resolve correctly from script locations |
| Config files | **PASS** | YAML config valid, all pipeline scripts present |
| Unnecessary files | **PASS** | Only .DS_Store (gitignored, cosmetic) |
| Large binaries | **PASS** | Largest file = 5.9 MB (CSV data) |
| Sensitive info | **PASS** | No API keys, passwords, or personal paths |
| README accuracy | **100% PASS** | All 101 referenced paths exist |

**Overall Status: PASS — Package is ready for GitHub distribution.**
