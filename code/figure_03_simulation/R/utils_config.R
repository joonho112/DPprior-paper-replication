# ==============================================================================
# utils_config.R — YAML configuration reader and directory helpers
# ==============================================================================
#
# Part of:  DPprior Replication Package (Figure 3 Simulation Study)
# Paper:    "Design-Conditional Prior Elicitation for DP Mixtures"
# Author:   JoonHo Lee
#
# Functions:
#   read_config()        — Parse YAML config file and validate required fields
#   ensure_dir()         — Create directory if it does not exist
#   ensure_project_dirs() — Create all project directories from config
#   timestamp_string()   — Generate timestamp for log files
#
# Dependencies: yaml
# Called by:    All scripts in scripts/ that require configuration
# ==============================================================================

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

read_config <- function(config_path) {
  if (!file.exists(config_path)) stop(sprintf("Config file not found: %s", config_path))
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Package 'yaml' is required. Install with install.packages('yaml').")
  }
  cfg <- yaml::read_yaml(config_path)

  required_top <- c("paths", "design", "methods", "replications", "dgp", "mcmc")
  missing <- setdiff(required_top, names(cfg))
  if (length(missing) > 0) {
    stop(sprintf("Missing required config fields: %s", paste(missing, collapse = ", ")))
  }

  # Normalize paths
  cfg$paths$results_dir <- cfg$paths$results_dir %||% "results"
  cfg$paths$priors_dir  <- cfg$paths$priors_dir  %||% file.path(cfg$paths$results_dir, "priors")
  cfg$paths$sim_dir     <- cfg$paths$sim_dir     %||% file.path(cfg$paths$results_dir, "sim")
  cfg$paths$figures_dir <- cfg$paths$figures_dir  %||% "figures"
  cfg$paths$logs_dir    <- cfg$paths$logs_dir     %||% "logs"

  cfg
}

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

ensure_project_dirs <- function(cfg) {
  ensure_dir(cfg$paths$results_dir)
  ensure_dir(cfg$paths$priors_dir)
  ensure_dir(cfg$paths$sim_dir)
  ensure_dir(cfg$paths$figures_dir)
  ensure_dir(file.path(cfg$paths$figures_dir, "main"))
  ensure_dir(file.path(cfg$paths$figures_dir, "supplement"))
  ensure_dir(cfg$paths$logs_dir)
}

timestamp_string <- function() format(Sys.time(), "%Y%m%d_%H%M%S")
