#!/usr/bin/env Rscript
# ==============================================================================
# 00_install_packages.R — Install all required R packages
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Paper:    "Design-Conditional Prior Elicitation for DP Mixtures"
# Author:   JoonHo Lee
#
# Pipeline: Setup (run ONCE before starting)
# Depends:  None (initialization step)
# Produces: Package installation in R library
# Runtime:  ~2-5 minutes (depending on compilation needs for nimble)
#
# Usage:
#   Rscript scripts/00_install_packages.R
#
# ==============================================================================

req_cran <- c(
  "yaml",
  "ggplot2",
  "nimble",
  "remotes",
  "future",
  "future.apply",
  "data.table",
  "coda"
)
for (p in req_cran) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}

if (!requireNamespace("DPprior", quietly = TRUE)) {
  remotes::install_github("joonho112/DPprior")
}

cat("Done installing packages.\n")
