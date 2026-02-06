#!/usr/bin/env Rscript
# ==============================================================================
# step0_check_env.R — Verify R environment, packages, and simulation config
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Paper:    "Design-Conditional Prior Elicitation for Dirichlet Process Mixtures"
# Author:   JoonHo Lee
#
# Pipeline: Step 0 of 7 — Environment verification
# Depends:  ../../install_packages.R (run once beforehand)
# Produces: Project directories (results/, figures/, logs/)
# Runtime:  ~10 seconds
#
# Usage:
#   Rscript scripts/step0_check_env.R
#   Rscript scripts/step0_check_env.R --install          # Auto-install missing
#   Rscript scripts/step0_check_env.R --config path.yaml # Custom config
#   Rscript scripts/step0_check_env.R --install --config config/fig3_design.yaml
#
# ==============================================================================

# --- Parse command-line arguments ---
args <- commandArgs(trailingOnly = TRUE)
auto_install <- "--install" %in% args
config_path  <- "config/fig3_design.yaml"

# Support --config <path> anywhere in args
if ("--config" %in% args) {
  idx <- which(args == "--config")
  if (idx < length(args)) config_path <- args[idx + 1]
}

cat(strrep("=", 70), "\n")
cat("DPprior Replication — Step 0: Environment Check\n")
cat(strrep("=", 70), "\n\n")

# =============================================================================
# 1. R version
# =============================================================================

cat("R version:", R.version.string, "\n")
r_major <- as.numeric(R.version$major)
r_minor <- as.numeric(R.version$minor)
r_ver   <- r_major + r_minor / 10

if (r_ver < 4.1) {
  warning("R >= 4.1 is recommended.  Some packages may not install correctly.")
} else {
  cat("  (>= 4.1 requirement satisfied)\n")
}
cat("\n")

# =============================================================================
# 2. Package checks — three tiers
# =============================================================================

# Simulation core (required for running the MCMC pipeline)
sim_pkgs <- c(
  "nimble",           # MCMC engine for DP mixture fitting
  "DPprior",          # Core package: DP prior elicitation (GitHub: joonho112/DPprior)
  "yaml",             # Config file parsing
  "future",           # Parallel execution framework
  "future.apply",     # Parallel apply functions
  "data.table",       # Fast CSV I/O for results
  "coda"              # MCMC diagnostics (ESS, Rhat)
)

# Visualization (required for reproducing figures)
viz_pkgs <- c(
  "ggplot2",          # Core plotting
  "dplyr",            # Data manipulation
  "tidyr",            # Data reshaping
  "patchwork",        # Multi-panel figure composition
  "scales"            # Axis formatting
)

# Optional (enhance output quality but not strictly required)
opt_pkgs <- c(
  "ragg",             # High-quality raster graphics (PNG/JPEG)
  "RColorBrewer",     # Additional color palettes
  "knitr",            # Table formatting
  "kableExtra",       # Enhanced LaTeX/HTML tables
  "remotes"           # GitHub package installation (needed for DPprior)
)

# --- Helper: check one package ---
check_pkg <- function(pkg) {
  ok <- requireNamespace(pkg, quietly = TRUE)
  ver <- if (ok) {
    tryCatch(as.character(utils::packageVersion(pkg)),
             error = function(e) "?")
  } else {
    NA
  }
  list(installed = ok, version = ver)
}

# --- Helper: report & collect missing ---
report_tier <- function(tier_name, pkgs) {
  cat(sprintf("%s:\n", tier_name))
  cat(strrep("-", 50), "\n")
  missing <- character(0)
  for (pkg in pkgs) {
    status <- check_pkg(pkg)
    if (status$installed) {
      cat(sprintf("  [OK]      %-16s  v%s\n", pkg, status$version))
    } else {
      cat(sprintf("  [MISSING] %-16s\n", pkg))
      missing <- c(missing, pkg)
    }
  }
  cat("\n")
  missing
}

sim_missing <- report_tier("Simulation core (required)", sim_pkgs)
viz_missing <- report_tier("Visualization (required)",   viz_pkgs)
opt_missing <- report_tier("Optional enhancements",      opt_pkgs)

all_required_missing <- c(sim_missing, viz_missing)

# =============================================================================
# 3. Auto-install if requested
# =============================================================================

if (auto_install && length(all_required_missing) > 0) {
  cat(strrep("-", 70), "\n")
  cat(sprintf("Auto-installing %d missing required package(s)...\n\n",
              length(all_required_missing)))

  # Separate CRAN vs GitHub
  cran_missing   <- setdiff(all_required_missing, "DPprior")
  github_missing <- intersect(all_required_missing, "DPprior")

  if (length(cran_missing) > 0) {
    cat("Installing from CRAN:", paste(cran_missing, collapse = ", "), "\n")
    install.packages(cran_missing, repos = "https://cloud.r-project.org")
    cat("\n")
  }

  if (length(github_missing) > 0) {
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes", repos = "https://cloud.r-project.org")
    }
    cat("Installing DPprior from GitHub (joonho112/DPprior)...\n")
    remotes::install_github("joonho112/DPprior", upgrade = "never")
    cat("\n")
  }

  # Also attempt optional packages
  if (length(opt_missing) > 0) {
    cat(sprintf("Attempting %d optional package(s)...\n", length(opt_missing)))
    for (pkg in opt_missing) {
      tryCatch(
        install.packages(pkg, repos = "https://cloud.r-project.org"),
        error = function(e) {
          message(sprintf("  Note: optional '%s' failed: %s", pkg, e$message))
        }
      )
    }
    cat("\n")
  }

  # Re-check
  cat("Re-checking after installation...\n\n")
  sim_missing <- report_tier("Simulation core (required)", sim_pkgs)
  viz_missing <- report_tier("Visualization (required)",   viz_pkgs)
  opt_missing <- report_tier("Optional enhancements",      opt_pkgs)
  all_required_missing <- c(sim_missing, viz_missing)
}

# =============================================================================
# 4. Config file validation
# =============================================================================

cat(strrep("=", 70), "\n")
cat("Configuration\n")
cat(strrep("=", 70), "\n\n")

if (!file.exists(config_path)) {
  cat(sprintf("  Config file not found: %s\n", config_path))
  cat("  (This is OK if you are running outside the simulation directory.)\n\n")
} else {
  cat(sprintf("  Config file: %s\n", config_path))

  tryCatch({
    source("R/utils_config.R", local = TRUE)
    cfg <- read_config(config_path)
    ensure_project_dirs(cfg)

    cat(sprintf("  Project:     %s\n", cfg$project$name))
    cat(sprintf("  Author:      %s\n", cfg$project$author))
    cat("\n")

    # Display simulation design parameters if present
    if (!is.null(cfg$design)) {
      cat("  Simulation design:\n")
      if (!is.null(cfg$design$K_star))
        cat(sprintf("    K* values:        %s\n",
                     paste(cfg$design$K_star, collapse = ", ")))
      if (!is.null(cfg$design$informativeness))
        cat(sprintf("    Informativeness:  %s\n",
                     paste(cfg$design$informativeness, collapse = ", ")))
      if (!is.null(cfg$design$n_rep))
        cat(sprintf("    Replications:     %d\n", cfg$design$n_rep))
      if (!is.null(cfg$design$methods))
        cat(sprintf("    Methods:          %s\n",
                     paste(cfg$design$methods, collapse = ", ")))
      cat("\n")
    }

    # Display paths
    cat("  Paths (created if missing):\n")
    if (is.list(cfg$paths)) {
      for (nm in names(cfg$paths)) {
        cat(sprintf("    %-12s  %s\n", paste0(nm, ":"), cfg$paths[[nm]]))
      }
    }
    cat("\n")
  }, error = function(e) {
    cat(sprintf("  Warning: could not parse config — %s\n\n", e$message))
  })
}

# =============================================================================
# 5. Parallel backend
# =============================================================================

cat(strrep("=", 70), "\n")
cat("Parallel Backend\n")
cat(strrep("=", 70), "\n\n")

if (requireNamespace("future", quietly = TRUE)) {
  n_cores <- future::availableCores()
  cat(sprintf("  future::availableCores() = %d\n", n_cores))
  cat(sprintf("  Recommended workers:       %d  (n_cores - 1)\n",
              max(1, n_cores - 1)))
} else {
  cat("  {future} not installed — parallel execution unavailable.\n")
}
cat("\n")

# =============================================================================
# 6. Summary
# =============================================================================

cat(strrep("=", 70), "\n")
cat("SUMMARY\n")
cat(strrep("=", 70), "\n\n")

n_req_missing <- length(all_required_missing)
n_opt_missing <- length(opt_missing)

if (n_req_missing == 0) {
  cat("  All required packages are installed.\n")
  if (n_opt_missing > 0) {
    cat(sprintf("  %d optional package(s) missing (non-critical).\n", n_opt_missing))
  }
  cat("\n  Ready to proceed!\n")
  cat("  Next step:  Rscript scripts/step1_generate_data.R\n")
} else {
  cat(sprintf("  %d REQUIRED package(s) still missing:\n", n_req_missing))
  for (pkg in all_required_missing) {
    cat(sprintf("    - %s\n", pkg))
  }
  cat("\n  To auto-install, run:\n")
  cat("    Rscript scripts/step0_check_env.R --install\n")
  cat("\n  Or install the full set from the repo root:\n")
  cat("    Rscript install_packages.R\n")
}

cat("\n")
cat(strrep("=", 70), "\n")
