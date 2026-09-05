# Author: JoonHo Lee (jlee296@ua.edu)
# Estimate the STAR school contrasts
#
# The primary analysis accounts for classroom dependence within each school. A
# separate schoolwise OLS analysis uses HC3 standard errors. Keeping both
# inputs allows the sensitivity analysis to isolate this first-stage choice.
#
# Called by applications/02_first_stage.R after data_loaders.R.
# Returns primary/HC3 school inputs and an uncertainty comparison.

# Form the kindergarten complete-case sample and estimate school contrasts
# under the classroom and HC3 specifications.
app_star_first_stage <- function() {
  informativeness <- function(tau2, se2) tau2 / (tau2 + exp(mean(log(se2))))
  star_raw <- app_load_star()
  star_k <- star_raw[star_raw$gr == "K", , drop = FALSE]
  star_source_n <- nrow(star_k)
  complete_fields <- c("cltype", "sch", "read", "math", "tch")
  star_cc <- star_k[stats::complete.cases(star_k[, complete_fields]), , drop = FALSE]
  star_analysis_n <- nrow(star_cc)
  if (star_source_n != 6325L || star_analysis_n != 5786L) {
    stop(sprintf(
      "Unexpected STAR source/complete-case N: %d/%d",
      star_source_n, star_analysis_n
    ), call. = FALSE)
  }

  # Standardize reading and mathematics separately, average them, then
  # standardize the composite over the retained sample.
  star_cc$zr <- as.numeric(scale(star_cc$read))
  star_cc$zm <- as.numeric(scale(star_cc$math))
  star_cc$y <- as.numeric(scale((star_cc$zr + star_cc$zm) / 2))
  star_cc$small <- as.integer(star_cc$cltype == "small")
  star_cc$school <- factor(star_cc$sch)
  star_cc$classroom <- interaction(star_cc$sch, star_cc$tch, drop = TRUE)
  arm_count <- stats::aggregate(
    star_cc$small, list(school = star_cc$school),
    function(x) c(n = length(x), small = sum(x), regular = sum(1L - x))
  )
  arm_matrix <- if (is.list(arm_count$x)) do.call(rbind, arm_count$x) else arm_count$x
  valid_school <- arm_matrix[, "small"] > 0L & arm_matrix[, "regular"] > 0L
  if (!all(valid_school) || nrow(arm_count) != 79L) {
    stop("STAR must contain 79 complete-case schools with both arms", call. = FALSE)
  }

  # Fit school-specific treatment effects with a shared classroom
  # random-intercept variance.
  mixed <- lme4::lmer(
    y ~ 0 + school + school:small + (1 | classroom),
    data = star_cc,
    REML = TRUE,
    control = lme4::lmerControl(check.rankX = "stop.deficient")
  )
  if (lme4::isSingular(mixed) || length(mixed@optinfo$conv$lme4$messages)) {
    stop("STAR classroom random-intercept model did not pass fit checks", call. = FALSE)
  }
  mixed_names <- names(lme4::fixef(mixed))
  mixed_index <- grep(":small$", mixed_names)
  mixed_schools <- sub("^school", "", sub(":small$", "", mixed_names[mixed_index]))
  mixed_effect <- unname(lme4::fixef(mixed)[mixed_index])
  mixed_se <- sqrt(diag(as.matrix(stats::vcov(mixed))))[mixed_index]
  school_levels <- levels(star_cc$school)

  # Estimate schoolwise OLS contrasts and the four HC standard-error variants
  # for the uncertainty comparison.
  school_rows <- lapply(school_levels, function(school) {
    d <- star_cc[star_cc$school == school, , drop = FALSE]
    fit <- stats::lm(y ~ small, data = d)
    ordinary <- sqrt(diag(stats::vcov(fit)))[["small"]]
    robust <- vapply(c("HC0", "HC1", "HC2", "HC3"), function(type) {
      sqrt(diag(sandwich::vcovHC(fit, type = type)))[["small"]]
    }, numeric(1))
    school_type <- names(sort(table(as.character(d$schtype)), decreasing = TRUE))[[1L]]
    data.frame(
      school = school, n_j = nrow(d), n_small = sum(d$small),
      n_regular = sum(1L - d$small), classrooms = length(unique(d$classroom)),
      location = school_type, ols_tau_hat = unname(stats::coef(fit)[["small"]]),
      se_ols = ordinary, se_HC0 = robust[["HC0"]], se_HC1 = robust[["HC1"]],
      se_HC2 = robust[["HC2"]], se_HC3 = robust[["HC3"]],
      stringsAsFactors = FALSE
    )
  })
  star_school <- do.call(rbind, school_rows)
  mixed_match <- match(star_school$school, mixed_schools)
  if (anyNA(mixed_match)) stop("Failed to align STAR mixed-model effects", call. = FALSE)
  star_school$mixed_tau_hat <- mixed_effect[mixed_match]
  star_school$se_mixed <- mixed_se[mixed_match]
  star_school$unit <- seq_len(nrow(star_school))

  # Give each first-stage choice the same unit/input schema so downstream
  # models receive comparable fields.
  make_star_variant <- function(name, effect, se) {
    data.frame(
      unit = star_school$unit, school = star_school$school,
      tau_hat = effect, se = se, se2 = se^2, n_j = star_school$n_j,
      n_small = star_school$n_small, n_regular = star_school$n_regular,
      classrooms = star_school$classrooms, location = star_school$location,
      data_variant = name, stringsAsFactors = FALSE
    )
  }
  star_variants <- list(
    star_multilevel = make_star_variant(
      "star_multilevel", star_school$mixed_tau_hat, star_school$se_mixed
    ),
    star_ols = make_star_variant("star_ols", star_school$ols_tau_hat, star_school$se_ols),
    star_hc0 = make_star_variant("star_hc0", star_school$ols_tau_hat, star_school$se_HC0),
    star_hc1 = make_star_variant("star_hc1", star_school$ols_tau_hat, star_school$se_HC1),
    star_hc2 = make_star_variant("star_hc2", star_school$ols_tau_hat, star_school$se_HC2),
    star_hc3 = make_star_variant("star_hc3", star_school$ols_tau_hat, star_school$se_HC3)
  )

  # Summarize how the first-stage variance choice changes the normal-model
  # benchmark and informativeness.
  star_uncertainty <- do.call(rbind, lapply(star_variants, function(d) {
    yi_values <- d$tau_hat
    vi_values <- d$se2
    re <- metafor::rma.uni(yi = yi_values, vi = vi_values, method = "REML")
    data.frame(
      data_variant = unique(d$data_variant), J = nrow(d), source_N = star_source_n,
      complete_case_N = star_analysis_n, classrooms = length(unique(star_cc$classroom)),
      mean_effect = mean(d$tau_hat), mean_se = mean(d$se),
      geometric_mean_se = sqrt(exp(mean(log(d$se2)))), tau2_REML = unname(re$tau2),
      I2_percent = unname(re$I2), informativeness = informativeness(re$tau2, d$se2),
      stringsAsFactors = FALSE
    )
  }))
  list(variants = star_variants, uncertainty = star_uncertainty)
}
