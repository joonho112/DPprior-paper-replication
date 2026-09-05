# Author: JoonHo Lee (jlee296@ua.edu)
# Aggregate retained simulation fits
#
# Summarize each planned condition while keeping calibration exclusions and
# diagnostic exclusions visible. Pairwise comparisons use only common retained
# replicates with the same generated data. No original model-result files are
# opened here.
#
# Sourced by simulation/01_reaggregate.R through 06_fixed_lambda.R.
# Inputs are checked fit, manifest, denominator and calibration data frames.
# These scientific aggregation functions are retained from the manuscript
# source.

simv2_synthesis_stop <- function(message) stop(message, call. = FALSE)
simv2_synthesis_required_metrics <- c(
  "E_K", "sd_K", "q025_K", "q975_K", "p_K1", "p_K_le2",
  "alpha_mean", "K_bias", "tau_rmse", "cover95_K"
)
simv2_synthesis_metadata_fields <- c(
  "suite", "legacy_cell_id", "conceptual_cell_key", "design", "J",
  "K_true", "weights", "weight_scheme", "dominant_share",
  "dominant_location_mode", "I_target", "n_mean", "prior", "r",
  "mu_K", "VIF", "dual_lambda", "in_convergence_scope", "is_headline",
  "is_active_dual", "prior_calibration_id", "prior_role",
  "grand_mean_label", "grand_mean_mean", "grand_mean_variance"
)

# Require all fields needed by the next aggregation; an absent metric must not
# be silently dropped.
simv2_synthesis_assert_columns <- function(data, required, label) {
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    simv2_synthesis_stop(sprintf(
      "%s lacks required columns: %s", label, paste(missing, collapse = ", ")
    ))
  }
  invisible(TRUE)
}
simv2_synthesis_read_csv <- function(path) {
  utils::read.csv(path,
    stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("NA")
  )
}

# Create a stable representation used when checking that metadata are constant
# within a scientific condition.
simv2_synthesis_signature <- function(value) {
  if (is.factor(value)) value <- as.character(value)
  if (is.numeric(value)) {
    return(ifelse(is.na(value), "<NA>", sprintf("%.17g", value)))
  }
  if (is.logical(value)) {
    return(ifelse(is.na(value), "<NA>", ifelse(value, "TRUE", "FALSE")))
  }
  value <- as.character(value)
  ifelse(is.na(value), "<NA>", paste0("<CHR>", enc2utf8(value)))
}

# Extract one metadata value only when every row in the group agrees.
simv2_synthesis_constant <- function(value, field, group) {
  if (length(unique(simv2_synthesis_signature(value))) != 1L) {
    simv2_synthesis_stop(sprintf(
      "Metadata field %s is not constant in group %s", field, group
    ))
  }
  value[[1L]]
}
simv2_synthesis_group_key <- function(suite, conceptual_cell_key) {
  paste(as.character(suite), as.character(conceptual_cell_key), sep = "\r")
}
simv2_synthesis_safe_sd <- function(value) {
  if (length(value) > 1L) stats::sd(value) else 0
}
simv2_synthesis_safe_se <- function(value) {
  simv2_synthesis_safe_sd(value) / sqrt(length(value))
}
simv2_synthesis_quantile <- function(value, probability) {
  as.numeric(stats::quantile(value, probability, names = FALSE, type = 8))
}

# Check retained-fit uniqueness, finite required metrics and their supported
# ranges before computing summaries.
simv2_synthesis_validate_fit_metrics <- function(fit_metrics) {
  numeric_metrics <- fit_metrics[simv2_synthesis_required_metrics]
  if (any(!vapply(numeric_metrics, is.numeric, logical(1))) ||
    any(!is.finite(as.matrix(numeric_metrics)))) {
    simv2_synthesis_stop("Verified fit metrics are nonnumeric or non-finite")
  }
  domain_ok <- fit_metrics$E_K >= 1 & fit_metrics$E_K <= fit_metrics$J &
    fit_metrics$sd_K >= 0 & fit_metrics$q025_K >= 1 &
    fit_metrics$q025_K <= fit_metrics$q975_K &
    fit_metrics$q975_K <= fit_metrics$J & fit_metrics$p_K1 >= 0 &
    fit_metrics$p_K1 <= 1 & fit_metrics$p_K_le2 >= 0 &
    fit_metrics$p_K_le2 <= 1 &
    fit_metrics$p_K1 <= fit_metrics$p_K_le2 + 1e-12 &
    fit_metrics$alpha_mean >= 0 & fit_metrics$tau_rmse >= 0 &
    fit_metrics$cover95_K %in% c(0, 1) &
    abs(fit_metrics$K_bias - (fit_metrics$E_K - fit_metrics$K_true)) <= 1e-10
  if (!all(domain_ok)) {
    simv2_synthesis_stop("Verified fit metric domain/identity validation failed")
  }
  invisible(TRUE)
}

# Recover the planned condition grid from the manifest, including conditions
# with no retained fits.
simv2_synthesis_manifest_cells <- function(production_fits) {
  simv2_synthesis_assert_columns(
    production_fits, c(simv2_synthesis_metadata_fields, "runnable"),
    "bound production fits"
  )
  group_key <- simv2_synthesis_group_key(
    production_fits$suite, production_fits$conceptual_cell_key
  )
  groups <- split(seq_len(nrow(production_fits)), group_key, drop = TRUE)
  rows <- lapply(names(groups), function(key) {
    indices <- groups[[key]]
    metadata <- lapply(simv2_synthesis_metadata_fields, function(field) {
      simv2_synthesis_constant(production_fits[[field]][indices], field, key)
    })
    names(metadata) <- simv2_synthesis_metadata_fields
    base <- as.data.frame(metadata, stringsAsFactors = FALSE)
    base$n_manifest_from_bound_fits <- length(indices)
    base$n_runnable_from_bound_fits <- sum(as.logical(production_fits$runnable[indices]))
    base
  })
  output <- do.call(rbind, rows)
  row.names(output) <- NULL
  output
}

# Match the planned cells to their denominators and aggregate only the
# eligible fits. Conditions without estimates retain missing numerical results
# and explicit exclusion counts.
simv2_synthesis_build_cell_summary <- function(fit_metrics, production_fits,
                                               cell_denominators,
                                               calibration = NULL) {
  simv2_synthesis_validate_fit_metrics(fit_metrics)
  cells <- simv2_synthesis_manifest_cells(production_fits)
  cell_key <- simv2_synthesis_group_key(cells$suite, cells$conceptual_cell_key)
  denom_key <- simv2_synthesis_group_key(
    cell_denominators$suite, cell_denominators$conceptual_cell_key
  )
  if (anyDuplicated(cell_key) || anyDuplicated(denom_key) ||
    !setequal(cell_key, denom_key)) {
    simv2_synthesis_stop("Bound manifest and collector cell identities differ")
  }
  cell_denominators <- cell_denominators[match(cell_key, denom_key), , drop = FALSE]
  if (!identical(
    as.integer(cells$n_manifest_from_bound_fits),
    as.integer(cell_denominators$n_manifest)
  ) ||
    !identical(
      as.integer(cells$n_runnable_from_bound_fits),
      as.integer(cell_denominators$n_runnable)
    )) {
    simv2_synthesis_stop("Bound manifest and collector cell denominators differ")
  }
  denominator_fields <- c(
    "n_manifest", "n_runnable", "n_scientific_eligible",
    "n_dependency_excluded", "n_diagnostic_excluded", "n_other_excluded"
  )
  cells[denominator_fields] <- cell_denominators[denominator_fields]
  cells$n_manifest_from_bound_fits <- NULL
  cells$n_runnable_from_bound_fits <- NULL
  fit_key <- simv2_synthesis_group_key(
    fit_metrics$suite, fit_metrics$conceptual_cell_key
  )
  groups <- split(seq_len(nrow(fit_metrics)), fit_key, drop = TRUE)
  result_fields <- c(
    "n_retry", "eligible_fraction_manifest", "eligible_fraction_runnable",
    "diagnostic_exclusion_fraction_runnable", "dependency_exclusion_fraction_manifest",
    "mean_E_K", "sd_between_E_K", "se_mean_E_K", "q10_E_K",
    "median_E_K", "q90_E_K", "rmse_E_K", "mean_sd_K",
    "mean_q025_K", "mean_q975_K", "mean_interval_width_K",
    "mean_p_K1", "se_mean_p_K1", "mean_p_K_le2", "mean_alpha_mean",
    "mean_K_bias", "se_mean_K_bias", "mean_tau_rmse", "coverage95_K",
    "se_coverage95_K", "E_K_over_mu_K"
  )
  for (field in result_fields) cells[[field]] <- NA_real_
  for (key in names(groups)) {
    cell_index <- match(key, cell_key)
    if (is.na(cell_index)) {
      simv2_synthesis_stop(sprintf("Verified fit group lacks manifest cell: %s", key))
    }
    indices <- groups[[key]]
    if (length(indices) != as.integer(cells$n_scientific_eligible[[cell_index]])) {
      simv2_synthesis_stop(sprintf(
        "Eligible fit count differs from collector denominator for %s", key
      ))
    }
    for (field in simv2_synthesis_metadata_fields) {
      simv2_synthesis_constant(fit_metrics[[field]][indices], field, key)
    }
    E_K <- fit_metrics$E_K[indices]
    K_bias <- fit_metrics$K_bias[indices]
    p_K1 <- fit_metrics$p_K1[indices]
    cover <- fit_metrics$cover95_K[indices]
    mu_K <- as.numeric(cells$mu_K[[cell_index]])
    values <- c(
      n_retry = sum(as.logical(fit_metrics$retry[indices])),
      eligible_fraction_manifest = length(indices) / cells$n_manifest[[cell_index]],
      eligible_fraction_runnable = length(indices) / cells$n_runnable[[cell_index]],
      diagnostic_exclusion_fraction_runnable =
        cells$n_diagnostic_excluded[[cell_index]] / cells$n_runnable[[cell_index]],
      dependency_exclusion_fraction_manifest =
        cells$n_dependency_excluded[[cell_index]] / cells$n_manifest[[cell_index]],
      mean_E_K = mean(E_K), sd_between_E_K = simv2_synthesis_safe_sd(E_K),
      se_mean_E_K = simv2_synthesis_safe_se(E_K),
      q10_E_K = simv2_synthesis_quantile(E_K, .10),
      median_E_K = simv2_synthesis_quantile(E_K, .50),
      q90_E_K = simv2_synthesis_quantile(E_K, .90),
      rmse_E_K = sqrt(mean(K_bias^2)),
      mean_sd_K = mean(fit_metrics$sd_K[indices]),
      mean_q025_K = mean(fit_metrics$q025_K[indices]),
      mean_q975_K = mean(fit_metrics$q975_K[indices]),
      mean_interval_width_K = mean(
        fit_metrics$q975_K[indices] - fit_metrics$q025_K[indices]
      ),
      mean_p_K1 = mean(p_K1), se_mean_p_K1 = simv2_synthesis_safe_se(p_K1),
      mean_p_K_le2 = mean(fit_metrics$p_K_le2[indices]),
      mean_alpha_mean = mean(fit_metrics$alpha_mean[indices]),
      mean_K_bias = mean(K_bias), se_mean_K_bias = simv2_synthesis_safe_se(K_bias),
      mean_tau_rmse = mean(fit_metrics$tau_rmse[indices]),
      coverage95_K = mean(cover), se_coverage95_K = simv2_synthesis_safe_se(cover),
      E_K_over_mu_K = if (is.finite(mu_K) && mu_K > 0) mean(E_K) / mu_K else NA_real_
    )
    cells[cell_index, names(values)] <- as.list(values)
  }
  available <- cells$n_scientific_eligible > 0L
  cells$scientific_results_available <- available
  cells$retention_below_90pct <- available & cells$eligible_fraction_runnable < .90
  cells$retention_below_75pct <- available & cells$eligible_fraction_runnable < .75
  cells$analysis_population <- ifelse(
    available, "checksum-admitted diagnostic-pass fits",
    ifelse(cells$n_runnable == 0L, "dependency-blocked; no scientific estimate",
      "no checksum-admitted diagnostic-pass fit"
    )
  )
  if (!is.null(calibration)) {
    simv2_synthesis_assert_columns(
      calibration, c("calibration_id", "selected_lambda"),
      "bound prior calibration manifest"
    )
    if (anyDuplicated(calibration$calibration_id)) {
      simv2_synthesis_stop("Calibration IDs are not unique")
    }
    position <- match(cells$prior_calibration_id, calibration$calibration_id)
    cells$selected_lambda <- calibration$selected_lambda[position]
  } else {
    cells$selected_lambda <- NA_real_
  }
  cells$effective_lambda <- ifelse(
    is.finite(cells$dual_lambda), cells$dual_lambda, cells$selected_lambda
  )
  suite_order <- match(cells$suite, c(
    "fallback", "headline_n001", "headline_n01", "central_dominant"
  ))
  cells <- cells[order(
    suite_order, cells$legacy_cell_id,
    cells$conceptual_cell_key
  ), , drop = FALSE]
  row.names(cells) <- NULL
  cells
}

# Recover the collector display from the cell summary while preserving its
# original field and row conventions.
simv2_synthesis_reproduce_collector_cells <- function(cell_summary,
                                                      collector_cells,
                                                      tolerance = 1e-12) {
  key_left <- simv2_synthesis_group_key(
    cell_summary$suite, cell_summary$conceptual_cell_key
  )
  key_right <- simv2_synthesis_group_key(
    collector_cells$suite, collector_cells$conceptual_cell_key
  )
  if (!setequal(key_left, key_right)) return(FALSE)
  collector_cells <- collector_cells[match(key_left, key_right), , drop = FALSE]
  mapping <- c(
    mean_E_K = "mean_E_K", mean_sd_K = "mean_sd_K",
    mean_q025_K = "mean_q025_K", mean_q975_K = "mean_q975_K",
    mean_p_K1 = "mean_p_K1", mean_p_K_le2 = "mean_p_K_le2",
    mean_alpha_mean = "mean_alpha_mean", mean_K_bias = "mean_K_bias",
    mean_tau_rmse = "mean_tau_rmse", coverage95_K = "mean_cover95_K"
  )
  all(vapply(names(mapping), function(left) {
    right <- mapping[[left]]
    a <- cell_summary[[left]]
    b <- collector_cells[[right]]
    same_na <- is.na(a) & is.na(b)
    all(same_na | (is.finite(a) & is.finite(b) & abs(a - b) <= tolerance))
  }, logical(1)))
}

# Select the three grand-mean branches for the headline simulation comparisons
# and keep their publication order.
simv2_synthesis_headline_table <- function(cell_summary) {
  output <- cell_summary[
    cell_summary$is_headline & cell_summary$n_runnable > 0L, ,
    drop = FALSE
  ]
  expected_labels <- c("N(0,0.01)", "N(0,1)", "N(0,100 variance)")
  counts <- table(output$grand_mean_label)
  if (!setequal(names(counts), expected_labels) ||
    any(as.integer(counts[expected_labels]) != 45L)) {
    simv2_synthesis_stop("Headline grand-mean grid is not exactly 45 x 3 cells")
  }
  if (any(!output$scientific_results_available)) {
    simv2_synthesis_stop("A headline cell has no scientific estimate")
  }
  output[order(
    output$K_true, output$I_target, output$prior,
    output$grand_mean_variance
  ), , drop = FALSE]
}

# Pair replicates within each condition, require identical DGP hashes, and
# summarize right-minus-left differences for each metric.
simv2_synthesis_grand_mean_contrasts <- function(fit_metrics) {
  headline <- fit_metrics[fit_metrics$is_headline, , drop = FALSE]
  labels <- c("N(0,0.01)", "N(0,1)", "N(0,100 variance)")
  if (!setequal(unique(headline$grand_mean_label), labels)) {
    simv2_synthesis_stop("Headline fits do not contain all three grand-mean labels")
  }
  comparisons <- list(
    c("N(0,0.01)", "N(0,1)"),
    c("N(0,1)", "N(0,100 variance)"),
    c("N(0,0.01)", "N(0,100 variance)")
  )
  metrics <- c(
    "E_K", "sd_K", "p_K1", "p_K_le2", "alpha_mean", "K_bias",
    "tau_rmse", "cover95_K"
  )
  cell_keys <- sort(unique(headline$conceptual_cell_key), method = "radix")
  if (length(cell_keys) != 45L) {
    simv2_synthesis_stop("Headline fit grid does not contain exactly 45 conditions")
  }
  rows <- list()
  index <- 0L
  for (cell_key in cell_keys) {
    cell <- headline[headline$conceptual_cell_key == cell_key, , drop = FALSE]
    for (comparison in comparisons) {
      left_label <- comparison[[1L]]
      right_label <- comparison[[2L]]
      left <- cell[cell$grand_mean_label == left_label, , drop = FALSE]
      right <- cell[cell$grand_mean_label == right_label, , drop = FALSE]
      merged <- merge(
        left, right,
        by = "replicate", suffixes = c("_left", "_right"),
        all = FALSE, sort = TRUE
      )
      if (!nrow(merged) ||
        any(merged$dgp_sha256_left != merged$dgp_sha256_right)) {
        simv2_synthesis_stop(sprintf(
          "Grand-mean paired DGP identity failed for %s: %s vs %s",
          cell_key, left_label, right_label
        ))
      }
      base <- left[1L, c(
        "conceptual_cell_key", "legacy_cell_id", "design", "J", "K_true",
        "weights", "I_target", "prior", "r", "mu_K", "VIF",
        "prior_role"
      ), drop = FALSE]
      for (metric in metrics) {
        delta <- merged[[paste0(metric, "_right")]] - merged[[paste0(metric, "_left")]]
        index <- index + 1L
        rows[[index]] <- cbind(
          base,
          data.frame(
            left_grand_mean = left_label, right_grand_mean = right_label,
            contrast_direction = "right minus left", metric = metric,
            n_left_eligible = nrow(left), n_right_eligible = nrow(right),
            n_paired = nrow(merged),
            paired_fraction_of_smaller_branch =
              nrow(merged) / min(nrow(left), nrow(right)),
            mean_delta = mean(delta), sd_delta = simv2_synthesis_safe_sd(delta),
            se_mean_delta = simv2_synthesis_safe_se(delta),
            q025_delta = simv2_synthesis_quantile(delta, .025),
            median_delta = simv2_synthesis_quantile(delta, .50),
            q975_delta = simv2_synthesis_quantile(delta, .975),
            stringsAsFactors = FALSE
          )
        )
      }
    }
  }
  output <- do.call(rbind, rows)
  row.names(output) <- NULL
  output
}

# Retain a prior/design trajectory only when estimates exist at all three
# ratios of stated to generating count.
simv2_synthesis_balanced_misspecification <- function(cell_summary) {
  x <- cell_summary[
    cell_summary$suite == "fallback" & cell_summary$design == "A" &
      cell_summary$prior %in% c("TSMM", "Dual-Anchor") &
      cell_summary$r %in% c(.5, 1, 2) &
      cell_summary$scientific_results_available, ,
    drop = FALSE
  ]
  key <- paste(x$prior, x$K_true, x$I_target, x$VIF, sep = "\r")
  groups <- split(seq_len(nrow(x)), key, drop = TRUE)
  keep <- unlist(lapply(groups, function(indices) {
    if (setequal(x$r[indices], c(.5, 1, 2))) indices else integer()
  }), use.names = FALSE)
  output <- x[sort(keep), , drop = FALSE]
  output$balanced_r_trajectory <- TRUE
  output[order(
    output$prior, output$K_true, output$I_target,
    output$VIF, output$r
  ), , drop = FALSE]
}

# Require estimates at J=25, 50, 100 and 200 within each prior and generating
# condition before comparing study sizes.
simv2_synthesis_balanced_design_size <- function(cell_summary) {
  x <- cell_summary[
    cell_summary$suite == "fallback" &
      ((cell_summary$design == "A" & cell_summary$J == 100 &
        ((cell_summary$prior %in% c("TSMM", "Dual-Anchor") &
          cell_summary$r == 1 & cell_summary$VIF == 2.5) |
          cell_summary$prior %in% c("Vague", "DORO-Unif"))) |
        cell_summary$design == "B" | cell_summary$design == "E") &
      cell_summary$scientific_results_available, ,
    drop = FALSE
  ]
  key <- paste(x$prior, x$K_true, x$I_target, sep = "\r")
  groups <- split(seq_len(nrow(x)), key, drop = TRUE)
  keep <- unlist(lapply(groups, function(indices) {
    if (setequal(x$J[indices], c(25, 50, 100, 200))) indices else integer()
  }), use.names = FALSE)
  output <- x[sort(keep), , drop = FALSE]
  output$balanced_J_trajectory <- TRUE
  output[order(output$prior, output$K_true, output$I_target, output$J), , drop = FALSE]
}

# Select the prespecified two-by-three comparison of informativeness and
# fixed/selected lambda.
simv2_synthesis_lambda_table <- function(cell_summary) {
  x <- cell_summary[
    cell_summary$suite == "fallback" & cell_summary$K_true == 5 &
      cell_summary$I_target %in% c(.2, .8) &
      ((cell_summary$design == "D") |
        (cell_summary$design == "A" & cell_summary$prior == "Dual-Anchor" &
          cell_summary$r == 1 & cell_summary$VIF == 2.5)) &
      cell_summary$scientific_results_available, ,
    drop = FALSE
  ]
  if (nrow(x) != 6L || any(!is.finite(x$effective_lambda)) ||
    any(table(x$I_target) != 3L)) {
    simv2_synthesis_stop("Lambda-sensitivity table is not a complete 2 x 3 grid")
  }
  x[order(x$I_target, x$effective_lambda), , drop = FALSE]
}

# Keep central and balanced-random dominant locations together and record
# whether both are available for each comparison.
simv2_synthesis_dominant_location <- function(cell_summary) {
  x <- cell_summary[
    cell_summary$design == "C" &
      cell_summary$dominant_location_mode %in% c("balanced_random", "central"), ,
    drop = FALSE
  ]
  x$location_comparison_available <- ave(
    x$dominant_location_mode,
    paste(x$legacy_cell_id, x$conceptual_cell_key, sep = "\r"),
    FUN = function(value) setequal(unique(value), c("balanced_random", "central"))
  )
  x[order(x$K_true, x$I_target, x$prior, x$dominant_location_mode), , drop = FALSE]
}
