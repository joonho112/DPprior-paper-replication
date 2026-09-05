# Author: JoonHo Lee (jlee296@ua.edu)
# Apply the study calibration rules
#
# DPprior supplies numerical fits; these helpers decide whether a result is
# eligible for the paper. Numerical verification, boundary classification and
# the .40 trigger are separate decisions. The .25 target is not the trigger.
#
# Sourced by the calibration entrypoints. Returns fit and selection records.
# Scientific function bodies are retained from the manuscript computation.
# See provenance/calibration-port.json for the source fingerprint and scope.

`%||%` <- function(x, y) if (is.null(x)) y else x
pcv_abort <- function(...) stop(sprintf(...), call. = FALSE)

# Read the study policy and check its supported schema, quadrature orders and
# lambda-selection rule before numerical work begins.
pcv_load_policy <- function(path) {
  if (!requireNamespace("yaml", quietly = TRUE)) pcv_abort("Package 'yaml' is required")
  policy <- yaml::read_yaml(path)
  required <- c(
    "schema_version", "policy_id", "package", "quadrature",
    "dual", "boundaries", "verification", "ssi"
  )
  missing <- setdiff(required, names(policy))
  if (length(missing)) pcv_abort("Policy is missing: %s", paste(missing, collapse = ", "))
  grid <- as.numeric(unlist(policy$dual$lambda_grid, use.names = FALSE))
  if (!length(grid) || any(!is.finite(grid)) || any(grid <= 0) ||
    any(grid > 1) || anyDuplicated(grid) || is.unsorted(grid)) {
    pcv_abort("dual.lambda_grid must be unique, increasing, and in (0,1]")
  }
  policy$dual$lambda_grid <- grid
  policy$quadrature$M_fit <- as.integer(policy$quadrature$M_fit)
  policy$quadrature$M_verify <- as.integer(policy$quadrature$M_verify)
  policy$quadrature$M_summary <- as.integer(policy$quadrature$M_summary)
  policy$quadrature$M_summary_verify <- as.integer(policy$quadrature$M_summary_verify)
  if (policy$quadrature$M_verify < 2L * policy$quadrature$M_fit) {
    pcv_abort("M_verify must be at least twice M_fit")
  }
  if (policy$quadrature$M_summary_verify < 2L * policy$quadrature$M_summary) {
    pcv_abort("M_summary_verify must be at least twice M_summary")
  }
  if (!identical(
    policy$dual$selection_rule$name,
    "largest_verified_lambda_exiting_high_risk_band_v1"
  )) {
    pcv_abort(
      "Unsupported soft-lambda selection rule: %s",
      policy$dual$selection_rule$name
    )
  }
  policy
}

# Preserve both a returned fit and any error condition carrying that fit. A
# numerical solver may provide useful diagnostics even when it signals
# failure.
pcv_capture_result <- function(expr) {
  condition <- NULL
  value <- tryCatch(
    expr,
    error = function(e) {
      condition <<- e
      if (is.list(e) && !is.null(e$result)) e$result else NULL
    }
  )
  list(value = value, condition = condition)
}

# Reduce a caught condition to a serializable class and message for the
# rejection record.
pcv_condition_record <- function(condition) {
  if (is.null(condition)) return(list(class = NA_character_, message = NA_character_))
  list(class = class(condition)[1], message = conditionMessage(condition))
}

# Distinguish an active scientific constraint from an optimizer limit or a
# near-point-mass Gamma prior. The classification is used differently for soft
# selection and hard sensitivity.
pcv_boundary_class <- function(fit, policy) {
  if (is.null(fit) || is.null(fit$parameters)) {
    return(list(
      class = "unclassified_boundary", detail = "missing fit or parameters",
      requires_scientific_review = TRUE
    ))
  }
  a <- suppressWarnings(as.numeric(fit$parameters$a))
  gamma_cv <- if (length(a) == 1L && is.finite(a) && a > 0) 1 / sqrt(a) else NA_real_
  point_mass <- is.finite(gamma_cv) &&
    gamma_cv < as.numeric(policy$boundaries$gamma_cv_point_mass_threshold)
  hard_constraint_active <- identical(fit$mode, "dual_hard") &&
    identical(fit$status, "boundary") &&
    isTRUE(fit$constraint$active) && isTRUE(fit$constraint$satisfied)
  hard_active <- hard_constraint_active &&
    identical(fit$computation$termination$boundary_reason, "active_hard_constraint")
  if (hard_active || (hard_constraint_active && point_mass)) {
    cls <- if (point_mass) "hard_active_constraint_point_mass_limit" else
      "hard_active_constraint"
    return(list(
      class = cls,
      detail = sprintf(
        "verified active hard constraint (%s); Gamma-alpha CV=%.6g",
        fit$computation$termination$boundary_reason %||% "unspecified",
        gamma_cv
      ),
      requires_scientific_review = point_mass
    ))
  }
  bound_state <- fit$tradeoff$optimality$bound_state %||% character()
  if (length(bound_state) && any(is.na(bound_state) | bound_state != "interior")) {
    return(list(
      class = "solver_parameter_boundary",
      detail = paste(names(bound_state), bound_state, sep = "=", collapse = ";"),
      requires_scientific_review = TRUE
    ))
  }
  if (point_mass) {
    return(list(
      class = "scientific_point_mass_limit",
      detail = sprintf(
        "Gamma-alpha CV %.6g is below policy threshold %.6g",
        gamma_cv,
        as.numeric(policy$boundaries$gamma_cv_point_mass_threshold)
      ),
      requires_scientific_review = TRUE
    ))
  }
  if (identical(fit$status, "boundary")) {
    return(list(
      class = "unclassified_boundary",
      detail = as.character(fit$computation$termination$boundary_reason %||%
        "boundary status without an approved classification"),
      requires_scientific_review = TRUE
    ))
  }
  list(
    class = "interior", detail = "finite interior solution",
    requires_scientific_review = FALSE
  )
}

# Collect every acceptance failure rather than stopping at the first. Return
# the reasons and boundary class alongside the final decision.
pcv_gate_fit <- function(fit, policy) {
  reasons <- character()
  if (is.null(fit) || !is.list(fit)) {
    return(list(
      ok = FALSE, reasons = "missing_result",
      boundary = list(
        class = "unclassified_boundary",
        detail = "missing result",
        requires_scientific_review = TRUE
      )
    ))
  }
  if (!fit$status %in% unlist(policy$verification$require_status)) {
    reasons <- c(reasons, paste0("status_", fit$status %||% "missing"))
  }
  if (isTRUE(policy$verification$require_usable) && !isTRUE(fit$usable)) {
    reasons <- c(reasons, "not_usable")
  }
  if (isTRUE(policy$verification$require_verified) && !isTRUE(fit$verified)) {
    reasons <- c(reasons, "not_verified")
  }
  if (!isTRUE(fit$verification$performed) || !isTRUE(fit$verification$passed)) {
    reasons <- c(reasons, "independent_verification_not_passed")
  }
  ab <- suppressWarnings(as.numeric(c(fit$parameters$a, fit$parameters$b)))
  if (length(ab) != 2L || any(!is.finite(ab)) || any(ab <= 0)) {
    reasons <- c(reasons, "nonfinite_or_nonpositive_parameters")
  }
  if (isTRUE(policy$verification$reject_approximation) &&
    isTRUE(fit$provenance$approximation$active)) {
    reasons <- c(reasons, "approximation_active")
  }
  boundary <- pcv_boundary_class(fit, policy)
  if (identical(boundary$class, "unclassified_boundary")) {
    reasons <- c(reasons, "unclassified_boundary")
  }
  list(ok = !length(unique(reasons)), reasons = unique(reasons), boundary = boundary)
}

# Require an accepted fit when the next calculation cannot proceed without
# one.
pcv_assert_fit <- function(fit, policy, label = "fit") {
  gate <- pcv_gate_fit(fit, policy)
  if (!gate$ok) pcv_abort(
    "%s failed closed: %s",
    label,
    paste(gate$reasons, collapse = ";")
  )
  invisible(gate)
}

# Request the exact count calibration and retain its condition and acceptance
# record. This lower-level helper may return an unusable candidate for
# inspection.
pcv_fit_k_candidate <- function(spec, policy) {
  captured <- pcv_capture_result(DPprior::DPprior_fit(
    J = as.integer(spec$J), mu_K = as.numeric(spec$mu_K),
    var_K = as.numeric(spec$var_K), method = "A2-MN",
    M = policy$quadrature$M_fit,
    check_diagnostics = FALSE
  ))
  list(
    fit = captured$value, condition = captured$condition,
    gate = pcv_gate_fit(captured$value, policy)
  )
}

# Return an accepted count-only fit, or stop with the recorded reason. The
# caller never receives silent fallback parameters.
pcv_fit_k_target <- function(spec, policy) {
  candidate <- pcv_fit_k_candidate(spec, policy)
  if (is.null(candidate$fit)) {
    rec <- pcv_condition_record(candidate$condition)
    pcv_abort("K-only calibration %s returned no result: %s", spec$spec_id, rec$message)
  }
  pcv_assert_fit(candidate$fit, policy, paste0(spec$spec_id, " K-only"))
  candidate$fit
}

# Keep TSMM if its size-biased tail is within the trigger. Otherwise evaluate
# the fixed grid, apply numerical and boundary checks, and select the largest
# eligible lambda.
pcv_soft_curve_select <- function(k_fit, spec, policy) {
  gate <- pcv_assert_fit(k_fit, policy, paste0(spec$spec_id, " K-only input"))
  a0 <- k_fit$parameters$a
  b0 <- k_fit$parameters$b
  p0 <- DPprior::prob_wsb_exceeds(policy$dual$threshold, a0, b0)
  trigger <- as.numeric(policy$dual$trigger_above)
  tol <- as.numeric(policy$dual$selection_rule$probability_tolerance)
  if (p0 <= trigger + tol) {
    curve <- data.frame(
      spec_id = spec$spec_id, lambda = 1, status = k_fit$status,
      usable = k_fit$usable, verified = k_fit$verified,
      a = a0, b = b0, achieved_mu_K = k_fit$achieved$K$mean,
      achieved_var_K = k_fit$achieved$K$variance,
      achieved_tail = p0, K_loss = 0, weight_loss = NA_real_,
      total_loss = 0, boundary_class = gate$boundary$class,
      gate_ok = TRUE, exits_high_risk_band = TRUE,
      soft_policy_eligible = TRUE, selected = TRUE,
      rejection_codes = "", stringsAsFactors = FALSE
    )
    return(list(
      fit = k_fit, curve = curve, applied = FALSE,
      selected_lambda = 1,
      selection_reason = "below_trigger_retain_verified_K_only",
      rejection_code = NA_character_, objects = list(k_only = k_fit)
    ))
  }
  curve_obj <- DPprior::compute_tradeoff_curve(
    J = as.integer(spec$J),
    K_target = list(mu_K = as.numeric(spec$mu_K), var_K = as.numeric(spec$var_K)),
    target = list(
      metric = policy$dual$metric,
      threshold = as.numeric(policy$dual$threshold),
      relation = "target", value = as.numeric(policy$dual$soft_target)
    ),
    lambda_seq = policy$dual$lambda_grid,
    M = policy$quadrature$M_fit,
    M_verify = policy$quadrature$M_verify,
    allow_approximate = TRUE,
    verbose = FALSE
  )
  curve <- as.data.frame(curve_obj, stringsAsFactors = FALSE)
  fits <- attr(curve_obj, "fits")
  conditions <- attr(curve_obj, "conditions")
  boundary_class <- character(nrow(curve))
  boundary_detail <- character(nrow(curve))
  scientific_review <- logical(nrow(curve))
  gate_ok <- logical(nrow(curve))
  rejection_codes <- character(nrow(curve))
  for (i in seq_len(nrow(curve))) {
    f <- fits[[curve$point_id[i]]]
    g <- pcv_gate_fit(f, policy)
    boundary_class[i] <- g$boundary$class
    boundary_detail[i] <- g$boundary$detail
    scientific_review[i] <- g$boundary$requires_scientific_review
    gate_ok[i] <- g$ok
    rejection_codes[i] <- paste(g$reasons, collapse = ";")
  }
  curve$spec_id <- spec$spec_id
  curve$baseline_tail <- p0
  curve$boundary_class <- boundary_class
  curve$boundary_detail <- boundary_detail
  curve$requires_scientific_review <- scientific_review
  curve$gate_ok <- gate_ok
  curve$exits_high_risk_band <- is.finite(curve$achieved_weight) &
    curve$achieved_weight <= trigger + tol
  curve$soft_boundary_allowed <- !curve$boundary_class %in%
    unlist(policy$boundaries$reject_soft_classes)
  curve$soft_policy_eligible <- curve$gate_ok & curve$exits_high_risk_band &
    curve$soft_boundary_allowed
  curve$selected <- FALSE
  curve$rejection_codes <- rejection_codes
  curve$rejection_codes[!curve$exits_high_risk_band] <- trimws(paste(
    curve$rejection_codes[!curve$exits_high_risk_band], "did_not_exit_high_risk_band",
    sep = ";"
  ), which = "left", whitespace = ";")
  curve$rejection_codes[!curve$soft_boundary_allowed] <- trimws(paste(
    curve$rejection_codes[!curve$soft_boundary_allowed], "soft_boundary_rejected",
    sep = ";"
  ), which = "left", whitespace = ";")
  eligible <- which(curve$soft_policy_eligible)
  if (!length(eligible)) {
    classes <- paste(sort(unique(curve$boundary_class[curve$exits_high_risk_band])),
      collapse = ","
    )
    return(list(
      fit = NULL, curve = curve, applied = NA,
      selected_lambda = NA_real_,
      selection_reason = "rejected_no_eligible_soft_candidate",
      rejection_code = paste0("no_eligible_soft_candidate;exit_band_boundary_classes=", classes),
      objects = list(curve_fits = fits, curve_conditions = conditions)
    ))
  }
  # Losses at different lambda values weight different objectives. We therefore
  # choose by the prespecified largest-eligible-lambda rule, not the smallest
  # total loss across the grid.
  chosen <- eligible[which.max(curve$lambda[eligible])]
  curve$selected[chosen] <- TRUE
  selected_fit <- fits[[curve$point_id[chosen]]]
  pcv_assert_fit(selected_fit, policy, paste0(spec$spec_id, " selected soft fit"))
  list(
    fit = selected_fit, curve = curve, applied = TRUE,
    selected_lambda = curve$lambda[chosen],
    selection_reason = policy$dual$selection_rule$name,
    rejection_code = NA_character_,
    objects = list(curve_fits = fits, curve_conditions = conditions)
  )
}

# Fit the separate upper-bound sensitivity. Numerical success is insufficient:
# the hard constraint must be satisfied and its boundary class must be
# allowed.
pcv_fit_hard_sensitivity <- function(k_fit, spec, policy) {
  pcv_assert_fit(k_fit, policy, paste0(spec$spec_id, " hard input"))
  captured <- pcv_capture_result(DPprior::DPprior_dual_hard(
    k_fit,
    constraint = list(
      metric = policy$dual$metric,
      threshold = as.numeric(policy$dual$threshold),
      relation = "<=", bound = as.numeric(policy$dual$hard_bound)
    ),
    M = policy$quadrature$M_fit,
    M_verify = policy$quadrature$M_verify,
    allow_approximate = FALSE
  ))
  fit <- captured$value
  if (is.null(fit)) return(list(
    fit = NULL, gate = list(
      ok = FALSE,
      reasons = "hard_no_result", boundary = list(
        class = "unclassified_boundary",
        detail = "hard calibration returned no result",
        requires_scientific_review = TRUE
      )
    ),
    condition = captured$condition
  ))
  gate <- pcv_gate_fit(fit, policy)
  allowed <- gate$boundary$class %in%
    unlist(policy$boundaries$allow_hard_sensitivity_classes)
  satisfied <- isTRUE(fit$constraint$satisfied)
  gate$ok <- gate$ok && allowed && satisfied
  if (!allowed) gate$reasons <- c(gate$reasons, "hard_boundary_class_not_allowed")
  if (!satisfied) gate$reasons <- c(gate$reasons, "hard_constraint_not_satisfied")
  list(fit = fit, gate = gate, condition = captured$condition)
}

# Solve the two size-biased tail equations on the log-rate scale, then check
# both attained probabilities directly.
pcv_solve_ssi <- function(policy) {
  t1 <- as.numeric(policy$ssi$target_1$threshold)
  p1 <- as.numeric(policy$ssi$target_1$probability)
  t2 <- as.numeric(policy$ssi$target_2$threshold)
  p2 <- as.numeric(policy$ssi$target_2$probability)
  d1 <- -log1p(-t1)
  d2 <- -log1p(-t2)
  target_ratio <- log(p1) / log(p2)
  objective <- function(log_b) {
    b <- exp(log_b)
    log(b / (b + d1)) / log(b / (b + d2)) - target_ratio
  }
  bracket <- c(-20, 20)
  root <- uniroot(objective, bracket, tol = .Machine$double.eps^0.75)
  b <- exp(root$root)
  a <- log(p1) / log(b / (b + d1))
  achieved <- c(
    DPprior::prob_wsb_exceeds(t1, a, b),
    DPprior::prob_wsb_exceeds(t2, a, b)
  )
  residual <- achieved - c(p1, p2)
  tol <- as.numeric(policy$verification$ssi_root_tolerance)
  verified <- all(is.finite(c(a, b, achieved, residual))) && a > 0 && b > 0 &&
    max(abs(residual)) <= tol
  if (!verified) pcv_abort("SSI root solution failed independent tail verification")
  list(
    a = a, b = b, status = "root_verified", usable = TRUE, verified = TRUE,
    mode = "ssi_two_tail_root", method = "deterministic_uniroot",
    achieved = achieved, residual = residual, target = c(p1, p2),
    thresholds = c(t1, t2), root = root,
    boundary_class = if (1 / sqrt(a) <
      as.numeric(policy$boundaries$gamma_cv_point_mass_threshold))
      "scientific_point_mass_limit" else "interior"
  )
}

# Return count moments, verified discrete count quantiles and analytic weight
# summaries. If PMF verification fails, leave its quantiles missing while
# retaining independently available quantities.
pcv_prior_summary <- function(J, a, b, M_fit = 160L, M_verify = 320L,
                              strict = TRUE) {
  moments <- DPprior::exact_K_moments(as.integer(J), a, b)
  logS <- DPprior::compute_log_stirling(as.integer(J))
  # Always request a returned audit object. If the independent PMF contract
  # does not pass, PMF-derived quantiles/p(K=1) below are failed closed to NA;
  # exact K moments and analytic W_SB tails remain independently available.
  pm_raw <- DPprior::pmf_K_marginal(
    as.integer(J), a, b,
    logS = logS,
    M = as.integer(M_fit), M_verify = as.integer(M_verify), strict = FALSE
  )
  pm_metadata <- attr(pm_raw, "marginal_metadata")
  pmf_passed <- isTRUE(pm_metadata$verification$passed)
  pm <- as.numeric(pm_raw)
  if (length(pm) != J + 1L) pcv_abort("Unexpected K PMF support length for J=%d", J)
  if (any(!is.finite(pm)) || any(pm < -1e-12)) pcv_abort("Invalid K PMF for J=%d", J)
  pm <- pmax(pm, 0)
  pm <- pm / sum(pm)
  support <- 0:J
  cdf <- cumsum(pm)
  qfun <- function(p) if (pmf_passed) support[which(cdf >= p)[1]] else NA_integer_
  list(
    E_alpha = a / b, CV_alpha = 1 / sqrt(a),
    prior_EK = moments$mean, prior_varK = moments$var,
    prior_SDK = sqrt(moments$var),
    prior_K05 = qfun(.05), prior_K50 = qfun(.5), prior_K95 = qfun(.95),
    prior_pK1 = if (pmf_passed) pm[support == 1] else NA_real_,
    p_wsb_gt_05 = DPprior::prob_wsb_exceeds(.5, a, b),
    p_wsb_gt_09 = DPprior::prob_wsb_exceeds(.9, a, b),
    E_rho = DPprior::mean_rho(a, b),
    summary_M = as.integer(M_fit), summary_M_verify = as.integer(M_verify),
    summary_pmf_strict_requested = isTRUE(strict),
    summary_pmf_status = as.character(pm_metadata$status %||% NA_character_),
    summary_pmf_verification_passed = pmf_passed,
    summary_pmf_l1_difference = as.numeric(
      pm_metadata$verification$l1_difference %||% NA_real_
    )
  )
}
