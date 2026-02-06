#!/usr/bin/env Rscript
# ==============================================================================
# install_packages.R — Install all R package dependencies
# ==============================================================================
#
# Part of:  DPprior Replication Package
# Paper:    "Design-Conditional Prior Elicitation for Dirichlet Process Mixtures"
# Author:   JoonHo Lee
#
# This script installs all R packages needed to reproduce every figure and
# table in the paper. Run once before starting any analysis.
#
# Usage:
#   Rscript install_packages.R
#   Rscript install_packages.R --check-only   # Verify without installing
#
# Estimated time: 5-15 minutes (nimble compilation can be slow)
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check-only" %in% args

cat(strrep("=", 70), "\n")
cat("DPprior Replication Package — R Dependency Installer\n")
cat(strrep("=", 70), "\n\n")

# --- R version check ---
cat("R version:", R.version.string, "\n")
r_ver <- as.numeric(R.version$major) + as.numeric(R.version$minor) / 10
if (r_ver < 4.1) {
  warning("R >= 4.1 is recommended. Current version may cause issues.")
}
cat("\n")

# =============================================================================
# Package definitions
# =============================================================================

# Core CRAN packages (required for simulation + figures + tables)
cran_required <- c(
  # Simulation core
  "nimble",           # MCMC engine for DP mixture fitting
  "yaml",            # Config file parsing
  "future",          # Parallel execution framework
  "future.apply",    # Parallel apply functions
  "data.table",      # Fast CSV I/O for results
  "coda",            # MCMC diagnostics (ESS, Rhat)

  # Visualization
  "ggplot2",         # Core plotting
  "dplyr",           # Data manipulation
  "tidyr",           # Data reshaping
  "patchwork",       # Multi-panel figure composition
  "scales"           # Axis formatting
)

# Optional CRAN packages (enhance output quality but not strictly required)
cran_optional <- c(
  "remotes",         # GitHub package installation (needed for DPprior)
  "ragg",            # High-quality raster graphics (PNG/JPEG)
  "RColorBrewer",    # Additional color palettes
  "knitr",           # Table formatting
  "kableExtra"       # Enhanced LaTeX/HTML tables
)

# GitHub packages
github_required <- list(
  list(
    name = "DPprior",
    repo = "joonho112/DPprior",
    description = "Core package: DP prior elicitation framework"
  )
)

# =============================================================================
# Status check function
# =============================================================================

check_package <- function(pkg) {
  installed <- requireNamespace(pkg, quietly = TRUE)
  if (installed) {
    ver <- tryCatch(
      as.character(utils::packageVersion(pkg)),
      error = function(e) "?"
    )
  } else {
    ver <- NA
  }
  list(installed = installed, version = ver)
}

# =============================================================================
# Check & Install
# =============================================================================

# --- CRAN required ---
cat("CRAN packages (required):\n")
cat(strrep("-", 50), "\n")
cran_missing <- character(0)

for (pkg in cran_required) {
  status <- check_package(pkg)
  if (status$installed) {
    cat(sprintf("  [OK]      %-16s  v%s\n", pkg, status$version))
  } else {
    cat(sprintf("  [MISSING] %-16s\n", pkg))
    cran_missing <- c(cran_missing, pkg)
  }
}
cat("\n")

# --- CRAN optional ---
cat("CRAN packages (optional):\n")
cat(strrep("-", 50), "\n")
cran_opt_missing <- character(0)

for (pkg in cran_optional) {
  status <- check_package(pkg)
  if (status$installed) {
    cat(sprintf("  [OK]      %-16s  v%s\n", pkg, status$version))
  } else {
    cat(sprintf("  [MISSING] %-16s  (optional)\n", pkg))
    cran_opt_missing <- c(cran_opt_missing, pkg)
  }
}
cat("\n")

# --- GitHub required ---
cat("GitHub packages (required):\n")
cat(strrep("-", 50), "\n")
github_missing <- list()

for (gh in github_required) {
  status <- check_package(gh$name)
  if (status$installed) {
    cat(sprintf("  [OK]      %-16s  v%s  (%s)\n", gh$name, status$version, gh$repo))
  } else {
    cat(sprintf("  [MISSING] %-16s  (%s)\n", gh$name, gh$repo))
    github_missing <- c(github_missing, list(gh))
  }
}
cat("\n")

# =============================================================================
# Install if not check-only
# =============================================================================

if (check_only) {
  cat(strrep("=", 70), "\n")
  cat("CHECK-ONLY MODE: No packages will be installed.\n")
  n_missing <- length(cran_missing) + length(github_missing)
  if (n_missing == 0) {
    cat("All required packages are installed.\n")
  } else {
    cat(sprintf("%d required package(s) missing. Run without --check-only to install.\n", n_missing))
  }
  cat(strrep("=", 70), "\n")
  quit(status = if (n_missing > 0) 1 else 0)
}

# Install missing CRAN required
if (length(cran_missing) > 0) {
  cat(sprintf("Installing %d missing CRAN required package(s)...\n", length(cran_missing)))
  install.packages(cran_missing, repos = "https://cloud.r-project.org")
  cat("\n")
}

# Install missing CRAN optional
if (length(cran_opt_missing) > 0) {
  cat(sprintf("Installing %d missing CRAN optional package(s)...\n", length(cran_opt_missing)))
  for (pkg in cran_opt_missing) {
    tryCatch(
      install.packages(pkg, repos = "https://cloud.r-project.org"),
      error = function(e) {
        message(sprintf("  Note: optional package '%s' failed to install: %s", pkg, e$message))
      }
    )
  }
  cat("\n")
}

# Install missing GitHub packages (remotes is needed first)
if (length(github_missing) > 0) {
  if (!requireNamespace("remotes", quietly = TRUE)) {
    install.packages("remotes", repos = "https://cloud.r-project.org")
  }
  for (gh in github_missing) {
    cat(sprintf("Installing %s from GitHub (%s)...\n", gh$name, gh$repo))
    remotes::install_github(gh$repo, upgrade = "never")
  }
  cat("\n")
}

# =============================================================================
# Final verification
# =============================================================================

cat(strrep("=", 70), "\n")
cat("VERIFICATION\n")
cat(strrep("=", 70), "\n\n")

all_ok <- TRUE

for (pkg in cran_required) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  FAIL: %s\n", pkg))
    all_ok <- FALSE
  }
}

for (gh in github_required) {
  if (!requireNamespace(gh$name, quietly = TRUE)) {
    cat(sprintf("  FAIL: %s (%s)\n", gh$name, gh$repo))
    all_ok <- FALSE
  }
}

if (all_ok) {
  cat("All required packages installed successfully.\n\n")

  # Print DPprior version for reference
  if (requireNamespace("DPprior", quietly = TRUE)) {
    cat(sprintf("DPprior version: %s\n", as.character(utils::packageVersion("DPprior"))))
  }
} else {
  cat("\nSome packages failed to install. Please check error messages above.\n")
}

cat(strrep("=", 70), "\n")
