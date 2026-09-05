# Author: JoonHo Lee (jlee296@ua.edu)
# Assemble application comparisons
#
# Attach study labels and prior settings, then compare posterior unit means
# and interval decisions within the same application. Unit-level changes and
# fit-level diagnostics use different denominators.
#
# Sourced by applications/03_summaries.R, 04_sensitivity.R and
# 06_supplemental.R.
# Inputs are verified summary data frames; functions return publication
# frames.
# The scientific summary functions are retained from the manuscript
# computation.

appv2_pub_stop <- function(message) stop(message, call. = FALSE)
appv2_pub_flag <- function(value) as.logical(value)

# Map recorded prior names to their roles in the publication without altering
# the underlying fit identity.
appv2_pub_publication_role <- function(prior_label) {
  prior_label <- as.character(prior_label)
  ifelse(
    prior_label == "DORO-Unif", "external_reference_comparator_only",
    ifelse(prior_label == "Dual-Hard-Sensitivity",
      "hard_sensitivity_only_never_primary",
      "primary_prior_comparison"
    )
  )
}

# Mark whether an application belongs to the main comparison or the
# supplemental OCRS example.
appv2_pub_application_scope <- function(app) {
  unname(c(
    star = "main_multisite_trial_application",
    bd = "main_meta_analysis_application",
    ocrs = "secondary_descriptive_application"
  )[as.character(app)])
}
appv2_pub_application_short_label <- function(app) {
  unname(c(star = "STAR", bd = "Bangert--Drowns", ocrs = "OCRS")[as.character(app)])
}

# Classify strict equal-tail exclusion of zero; an endpoint equal to zero is
# not counted as exclusion.
appv2_pub_interval_state <- function(q025, q975) {
  ifelse(q025 > 0, "positive", ifelse(q975 < 0, "negative", "includes_zero"))
}

# Join fitted summaries to their declared priors and publication labels,
# rejecting unmatched fit keys.
appv2_pub_enrich_fits <- function(verified) {
  fit <- verified$fit
  base_position <- match(fit$base_fit_key, verified$base$fit_key)
  execution_position <- match(
    fit$execution_fit_key,
    verified$execution$execution_fit_key
  )
  if (anyNA(base_position) || anyNA(execution_position)) {
    appv2_pub_stop("cannot enrich fit summaries from declared manifests")
  }
  base_fields <- c(
    "a", "b", "selected_lambda", "prior_EK", "prior_SDK", "prior_K05",
    "prior_K50", "prior_K95", "prior_pK1", "boundary_class",
    "requires_scientific_review"
  )
  for (field in base_fields) {
    fit[[paste0("manifest_", field)]] <- verified$base[[field]][base_position]
  }
  execution_fields <- c(
    "data_source_N", "data_analysis_N", "data_interpretation",
    "grand_mean_mean", "grand_mean_variance", "sigma2_tau_upper",
    "primary_analysis"
  )
  for (field in execution_fields) {
    fit[[field]] <- verified$execution[[field]][execution_position]
  }
  fit$publication_role <- appv2_pub_publication_role(fit$prior_label)
  fit$application_scope <- appv2_pub_application_scope(fit$app)
  fit$application_short_label <- appv2_pub_application_short_label(fit$app)
  fit$fit_denominator <- 35L
  fit$admitted_fit_count <- 35L
  fit$excluded_fit_count <- 0L
  fit
}

# Compare each unit with its unpooled estimate and the same unit under Vague.
# Summarize shifts and interval decisions within each fitted application.
appv2_pub_build_unit_stability <- function(unit, enriched_fit) {
  unit$interval_state <- appv2_pub_interval_state(unit$q025, unit$q975)
  detail_rows <- list()
  summary_rows <- list()
  counter <- 0L
  for (spec in unique(as.character(unit$analysis_spec))) {
    for (app in unique(as.character(unit$app[unit$analysis_spec == spec]))) {
      scope <- unit[unit$analysis_spec == spec & unit$app == app, , drop = FALSE]
      baseline <- scope[scope$prior_label == "Vague",
        c("unit", "posterior_mean", "interval_state"),
        drop = FALSE
      ]
      names(baseline)[2:3] <- c("vague_posterior_mean", "vague_interval_state")
      if (nrow(baseline) != unique(scope$J)) {
        appv2_pub_stop(sprintf(
          "Vague unit baseline is incomplete for %s/%s",
          app, spec
        ))
      }
      for (key in unique(as.character(scope$execution_fit_key))) {
        rows <- scope[scope$execution_fit_key == key, , drop = FALSE]
        rows <- merge(rows, baseline, by = "unit", all.x = TRUE, sort = FALSE)
        rows <- rows[order(rows$unit), , drop = FALSE]
        if (nrow(rows) != rows$J[[1L]] || anyNA(rows$vague_posterior_mean) ||
          anyNA(rows$vague_interval_state)) {
          appv2_pub_stop(sprintf("unit baseline join failed for %s", key))
        }
        rows$posterior_mean_shift_from_vague <-
          rows$posterior_mean - rows$vague_posterior_mean
        rows$interval_decision_changed_from_vague <-
          rows$interval_state != rows$vague_interval_state
        rows$interval_exclusion_gained_from_vague <-
          rows$interval_state != "includes_zero" &
            rows$vague_interval_state == "includes_zero"
        rows$interval_exclusion_lost_from_vague <-
          rows$interval_state == "includes_zero" &
            rows$vague_interval_state != "includes_zero"
        counter <- counter + 1L
        detail_rows[[counter]] <- rows
        metadata <- enriched_fit[enriched_fit$execution_fit_key == key,
          c(
            "execution_fit_key", "execution_fit_sha256",
            "base_fit_key", "app", "context",
            "application_label", "application_short_label",
            "application_scope", "analysis_spec",
            "data_variant", "prior_order", "prior_label",
            "display_label", "prior_role",
            "scientific_role", "publication_role", "J",
            "retained_partition_draws", "fit_denominator",
            "admitted_fit_count", "excluded_fit_count"
          ),
          drop = FALSE
        ]
        difference <- rows$posterior_mean_shift_from_vague
        posterior_vs_unpooled <- rows$posterior_mean - rows$tau_hat
        correlation <- if (stats::sd(rows$posterior_mean) > 0 &&
          stats::sd(rows$vague_posterior_mean) > 0) {
          stats::cor(rows$posterior_mean, rows$vague_posterior_mean)
        } else if (all(difference == 0)) 1 else NA_real_
        summary_rows[[counter]] <- cbind(metadata, data.frame(
          unit_denominator = nrow(rows),
          posterior_mean_across_units = mean(rows$posterior_mean),
          unpooled_mean_across_units = mean(rows$tau_hat),
          rmse_posterior_vs_unpooled = sqrt(mean(posterior_vs_unpooled^2)),
          mean_abs_posterior_vs_unpooled = mean(abs(posterior_vs_unpooled)),
          rmse_unit_shift_from_vague = sqrt(mean(difference^2)),
          mean_abs_unit_shift_from_vague = mean(abs(difference)),
          max_abs_unit_shift_from_vague = max(abs(difference)),
          correlation_with_vague_unit_means = correlation,
          interval_positive_count = sum(rows$interval_state == "positive"),
          interval_negative_count = sum(rows$interval_state == "negative"),
          interval_excludes_zero_count = sum(rows$interval_state != "includes_zero"),
          interval_includes_zero_count = sum(rows$interval_state == "includes_zero"),
          interval_decision_changed_from_vague_count = sum(
            rows$interval_decision_changed_from_vague
          ),
          interval_exclusion_gained_from_vague_count = sum(
            rows$interval_exclusion_gained_from_vague
          ),
          interval_exclusion_lost_from_vague_count = sum(
            rows$interval_exclusion_lost_from_vague
          ),
          interval_rule = "equal_tail_q025_q975_strict_exclusion",
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  detail <- do.call(rbind, detail_rows)
  summary <- do.call(rbind, summary_rows)
  row.names(detail) <- row.names(summary) <- NULL
  list(detail = detail, summary = summary)
}

# Match the same units and priors across primary and sensitivity
# specifications before summarizing their changes.
appv2_pub_build_sensitivity_unit_shifts <- function(unit_detail,
                                                    enriched_fit) {
  pairs <- data.frame(
    comparison = c(rep("primary_vs_wide_scale", 3L), "star_primary_vs_hc3"),
    app = c("star", "bd", "ocrs", "star"),
    reference_spec = "primary_scale_aligned",
    sensitivity_spec = c(rep("wide_scale_sensitivity", 3L), "star_hc3_uncertainty"),
    stringsAsFactors = FALSE
  )
  rows <- list()
  counter <- 0L
  for (pair_index in seq_len(nrow(pairs))) {
    pair <- pairs[pair_index, , drop = FALSE]
    for (prior in unique(as.character(enriched_fit$prior_label[
      enriched_fit$app == pair$app & enriched_fit$analysis_spec == pair$reference_spec
    ]))) {
      reference <- unit_detail[
        unit_detail$app == pair$app & unit_detail$analysis_spec == pair$reference_spec &
          unit_detail$prior_label == prior,
        c("unit", "posterior_mean", "q025", "q975", "interval_state"),
        drop = FALSE
      ]
      sensitivity <- unit_detail[
        unit_detail$app == pair$app &
          unit_detail$analysis_spec == pair$sensitivity_spec &
          unit_detail$prior_label == prior,
        c("unit", "posterior_mean", "q025", "q975", "interval_state"),
        drop = FALSE
      ]
      names(reference)[-1L] <- paste0(names(reference)[-1L], "_reference")
      names(sensitivity)[-1L] <- paste0(names(sensitivity)[-1L], "_sensitivity")
      joined <- merge(reference, sensitivity, by = "unit", all = TRUE, sort = TRUE)
      fit_row <- enriched_fit[
        enriched_fit$app == pair$app &
          enriched_fit$analysis_spec == pair$reference_spec &
          enriched_fit$prior_label == prior, ,
        drop = FALSE
      ]
      if (nrow(fit_row) != 1L || nrow(joined) != fit_row$J ||
        anyNA(joined)) {
        appv2_pub_stop(sprintf(
          "sensitivity unit pairing failed for %s/%s/%s",
          pair$app, pair$sensitivity_spec, prior
        ))
      }
      difference <- joined$posterior_mean_sensitivity - joined$posterior_mean_reference
      counter <- counter + 1L
      rows[[counter]] <- data.frame(
        comparison = pair$comparison, app = pair$app,
        application_short_label = appv2_pub_application_short_label(pair$app),
        application_scope = appv2_pub_application_scope(pair$app),
        prior_order = fit_row$prior_order, prior_label = prior,
        display_label = fit_row$display_label,
        publication_role = fit_row$publication_role,
        reference_spec = pair$reference_spec,
        sensitivity_spec = pair$sensitivity_spec,
        unit_denominator = nrow(joined),
        rmse_unit_mean_shift = sqrt(mean(difference^2)),
        mean_abs_unit_mean_shift = mean(abs(difference)),
        max_abs_unit_mean_shift = max(abs(difference)),
        reference_interval_excludes_zero_count = sum(
          joined$interval_state_reference != "includes_zero"
        ),
        sensitivity_interval_excludes_zero_count = sum(
          joined$interval_state_sensitivity != "includes_zero"
        ),
        interval_decision_changed_count = sum(
          joined$interval_state_reference != joined$interval_state_sensitivity
        ),
        interpretation = if (pair$sensitivity_spec ==
          "star_hc3_uncertainty") {
          "uncertainty_estimator_and_application_spec_sensitivity_not_prior_only"
        } else {
          "grand_mean_and_variance_scale_sensitivity"
        },
        stringsAsFactors = FALSE
      )
    }
  }
  result <- do.call(rbind, rows)
  row.names(result) <- NULL
  result[order(
    match(result$app, c("star", "bd", "ocrs")),
    match(result$comparison, c("primary_vs_wide_scale", "star_primary_vs_hc3")),
    result$prior_order
  ), , drop = FALSE]
}

# Turn one row per metric into one row per matched fit comparison for the
# supplemental table.
appv2_pub_widen_comparison <- function(comparison) {
  id_fields <- c(
    "comparison", "app", "prior_label", "display_label",
    "prior_role", "scientific_role", "reference_spec",
    "sensitivity_spec"
  )
  reference <- stats::reshape(
    comparison[c(id_fields, "metric", "reference_value")],
    idvar = id_fields, timevar = "metric", direction = "wide"
  )
  sensitivity <- stats::reshape(
    comparison[c(id_fields, "metric", "sensitivity_value")],
    idvar = id_fields, timevar = "metric", direction = "wide"
  )
  delta <- stats::reshape(
    comparison[c(id_fields, "metric", "delta_sensitivity_minus_reference")],
    idvar = id_fields, timevar = "metric", direction = "wide"
  )
  result <- Reduce(
    function(left, right) merge(
      left, right,
      by = id_fields, all = TRUE, sort = FALSE
    ),
    list(reference, sensitivity, delta)
  )
  names(result) <- sub("reference_value\\.", "reference_", names(result))
  names(result) <- sub("sensitivity_value\\.", "sensitivity_", names(result))
  names(result) <- sub(
    "delta_sensitivity_minus_reference\\.", "delta_",
    names(result)
  )
  result
}
