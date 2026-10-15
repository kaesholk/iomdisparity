#' Estimate Racial/Ethnic Healthcare Disparities via IOM Rank-and-Replace
#'
#' @description Implements the Institute of Medicine (IOM) definition of racial and
#'   ethnic healthcare disparities using the rank-and-replace algorithm (McGuire et al., 2006;
#'   Cook et al., 2010, 2012). Provides comparative reporting across the IOM Rank-and-Replace
#'   estimator, the Residual Direct Effect (RDE), and Unadjusted Differences.
#'
#' @details
#' According to the IOM definition (*Unequal Treatment*, 2002), healthcare disparities
#' are racial/ethnic differences not justified by clinical need or preferences.
#' Differences mediated through socioeconomic status (SES), health insurance, geography,
#' or discrimination are considered part of the disparity.
#'
#' This function carries out:
#' \enumerate{
#'   \item Estimation of a baseline regression model (\code{glm} or \code{two_part_model})
#'     predicting utilization/expenditure from need, SES, and race.
#'   \item Computation of a scalar "health index" summarizing clinical need.
#'   \item Quantile mapping of the comparison group's health index onto the reference
#'     distribution (creating a counterfactual minority population with reference need).
#'   \item Estimation of IOM-concordant counterfactual predictions and disparity metrics.
#'   \item Stratified bootstrap inference for robust standard errors and confidence intervals.
#' }
#'
#' @param formula An object of class \code{\link[stats]{formula}} specifying the regression
#'   model (e.g. \code{outcome ~ need_var1 + ses_var1 + race}).
#' @param data A data frame containing all variables in \code{formula}.
#' @param race_col Character name of the column identifying racial/ethnic groups.
#' @param ref_race Category value for the reference group (e.g., \code{"White"}).
#' @param comp_race Category value for the comparison minority group (e.g., \code{"Black"}).
#' @param need_vars Character vector of variable names in \code{formula} representing
#'   clinical need and health status (e.g., age, physical/mental functioning, chronic conditions).
#' @param model_type Model type to fit: \code{"glm"} for standard single-part GLMs or
#'   \code{"two_part"} for semi-continuous expenditure/utilization models.
#' @param family Error distribution and link function for \code{"glm"} or Part 1 of
#'   \code{"two_part"}. Defaults to \code{\link[stats]{gaussian}()}.
#' @param part2_family Error distribution and link function for Part 2 of \code{"two_part"}.
#'   Defaults to \code{\link[stats]{Gamma}(link = "log")}.
#' @param weights Optional character column name or numeric vector of survey/sampling weights.
#' @param n_boot Number of stratified bootstrap iterations for standard error and CI
#'   estimation. Set to \code{0} to skip bootstrapping. Defaults to \code{200}.
#' @param conf_level Confidence level for bootstrap intervals (defaults to \code{0.95}).
#' @param refit_with_index Logical; whether to refit a secondary model with the health
#'   index substituted for need variables. Defaults to \code{FALSE} (canonical Cook et al. method).
#' @param seed Optional random seed for reproducible bootstrap sampling.
#'
#' @return An object of class \code{iom_disparity} containing:
#'   \item{estimates}{Data frame containing point estimates, standard errors, and 95% CIs
#'     for IOM Rank-and-Replace, Residual Direct Effect (RDE), and Unadjusted differences.}
#'   \item{means}{Summary of observed, predicted, and counterfactual means for reference
#'     and comparison groups.}
#'   \item{health_indices}{List containing factual reference, factual comparison, and
#'     counterfactual comparison health index values.}
#'   \item{model}{The fitted stage-1 model object.}
#'   \item{bootstrap}{Detailed bootstrap output if \code{n_boot > 0}.}
#'   \item{call}{The matched call.}
#'
#' @references
#'   Institute of Medicine. (2002). \emph{Unequal Treatment: Confronting Racial and
#'   Ethnic Disparities in Health Care}. Washington, DC: The National Academies Press.
#'   \doi{10.17226/10260}
#'
#'   McGuire, T. G., Alegria, M., Cook, B. L., Wells, K. B., & Zaslavsky, A. M. (2006).
#'   Implementing the Institute of Medicine definition of disparities: an application
#'   to mental health care. \emph{Health Services Research}, 41(5), 1979-2005.
#'   \doi{10.1111/j.1475-6773.2006.00583.x}
#'
#'   Cook, B. L., McGuire, T. G., Lock, K., & Zaslavsky, A. M. (2010). Comparing methods
#'   of racial and ethnic disparities measurement across different settings of mental
#'   health care. \emph{Health Services Research}, 45(3), 825-847.
#'   \doi{10.1111/j.1475-6773.2010.01100.x}
#'
#'   Cook, B. L., McGuire, T. G., & Zaslavsky, A. M. (2012). Measuring racial/ethnic
#'   disparities in health care: methods and practical issues. \emph{Health Services Research},
#'   47(3 Pt 2), 1232-1254. \doi{10.1111/j.1475-6773.2012.01387.x}
#'
#' @export
#'
#' @examples
#' data(meps_synthetic)
#'
#' # Binary Outcome: Any Mental Health Care Use
#' fit_bin <- iom_disparity(
#'   formula = any_mental_health_use ~ age + female + chronic_conditions +
#'     sf12_physical + sf12_mental + income_poverty_ratio + insurance + race,
#'   data = meps_synthetic,
#'   race_col = "race",
#'   ref_race = "White",
#'   comp_race = "Black",
#'   need_vars = c("age", "female", "chronic_conditions", "sf12_physical", "sf12_mental"),
#'   model_type = "glm",
#'   family = binomial(),
#'   n_boot = 50,
#'   seed = 42
#' )
#' print(fit_bin)
#' summary(fit_bin)
#' plot(fit_bin, type = "distributions")
iom_disparity <- function(formula,
                          data,
                          race_col,
                          ref_race,
                          comp_race,
                          need_vars,
                          model_type = c("glm", "two_part"),
                          family = stats::gaussian(),
                          part2_family = stats::Gamma(link = "log"),
                          weights = NULL,
                          n_boot = 200,
                          conf_level = 0.95,
                          refit_with_index = FALSE,
                          seed = NULL) {
  cl <- match.call()
  model_type <- match.arg(model_type)
  
  # Validate input arguments
  validate_iom_inputs(data, formula, race_col, ref_race, comp_race, need_vars, weights)
  
  # Handle weights column name vs vector
  weights_col <- NULL
  if (!is.null(weights)) {
    if (is.character(weights) && length(weights) == 1) {
      weights_col <- weights
    } else if (is.numeric(weights)) {
      weights_col <- "..iom_w.."
      data[[weights_col]] <- weights
    }
  }
  
  # Filter data to the two target racial groups
  sub_data <- data[data[[race_col]] %in% c(ref_race, comp_race), , drop = FALSE]
  
  # Point estimation
  core_res <- estimate_disparities_core(
    data = sub_data,
    formula = formula,
    race_col = race_col,
    ref_race = ref_race,
    comp_race = comp_race,
    need_vars = need_vars,
    model_type = model_type,
    family = family,
    part2_family = part2_family,
    weights_col = weights_col,
    refit_with_index = refit_with_index
  )
  
  # Bootstrap inference
  boot_res <- NULL
  se_vec <- c(iom = NA_real_, rde = NA_real_, unadjusted = NA_real_)
  ci_mat <- matrix(NA_real_, nrow = 3, ncol = 2,
                   dimnames = list(c("iom", "rde", "unadjusted"),
                                   c(paste0(100 * (1 - conf_level) / 2, "%"),
                                     paste0(100 * (1 - (1 - conf_level) / 2), "%"))))
  
  if (n_boot > 0) {
    boot_res <- bootstrap_iom(
      sub_data = sub_data,
      formula = formula,
      race_col = race_col,
      ref_race = ref_race,
      comp_race = comp_race,
      need_vars = need_vars,
      model_type = model_type,
      family = family,
      part2_family = part2_family,
      weights_col = weights_col,
      n_boot = n_boot,
      conf_level = conf_level,
      refit_with_index = refit_with_index,
      seed = seed
    )
    se_vec <- boot_res$se
    ci_mat["iom", ] <- boot_res$ci$iom
    ci_mat["rde", ] <- boot_res$ci$rde
    ci_mat["unadjusted", ] <- boot_res$ci$unadjusted
  }
  
  estimates_df <- data.frame(
    Method = c("IOM Rank-and-Replace", "Residual Direct Effect (RDE)", "Unadjusted Difference"),
    Disparity = c(core_res$iom_disparity, core_res$rde_disparity, core_res$unadjusted_difference),
    Std_Error = c(se_vec["iom"], se_vec["rde"], se_vec["unadjusted"]),
    CI_Lower = ci_mat[, 1],
    CI_Upper = ci_mat[, 2],
    Relative_Ratio = c(core_res$iom_relative, core_res$rde_relative, core_res$unadjusted_relative),
    stringsAsFactors = FALSE,
    row.names = c("IOM", "RDE", "Unadjusted")
  )
  
  out <- list(
    estimates = estimates_df,
    means = core_res$means,
    health_indices = core_res$health_indices,
    model = core_res$model,
    bootstrap = boot_res,
    settings = list(
      race_col = race_col,
      ref_race = ref_race,
      comp_race = comp_race,
      need_vars = need_vars,
      model_type = model_type,
      n_boot = n_boot,
      conf_level = conf_level
    ),
    call = cl
  )
  class(out) <- "iom_disparity"
  return(out)
}

#' Core Disparity Estimation Engine
#' @keywords internal
#' @noRd
estimate_disparities_core <- function(data, formula, race_col, ref_race, comp_race, need_vars,
                                     model_type = "glm", family = stats::gaussian(),
                                     part2_family = stats::Gamma(link = "log"),
                                     weights_col = NULL, refit_with_index = FALSE) {
  y_var <- all.vars(formula)[1]
  w_vec <- if (!is.null(weights_col)) data[[weights_col]] else NULL
  
  is_ref <- data[[race_col]] == ref_race
  is_comp <- data[[race_col]] == comp_race
  
  if (sum(is_ref) < 5 || sum(is_comp) < 5) {
    stop("Insufficient observations in reference or comparison group.", call. = FALSE)
  }
  
  # Fit stage 1 model
  model <- if (model_type == "glm") {
    if (is.null(w_vec)) {
      stats::glm(formula, data = data, family = family)
    } else {
      stats::glm(formula, data = data, family = family, weights = w_vec)
    }
  } else {
    two_part_model(formula, data = data, weights = w_vec, part1_family = family, part2_family = part2_family)
  }
  
  w_ref <- if (!is.null(w_vec)) w_vec[is_ref] else NULL
  w_comp <- if (!is.null(w_vec)) w_vec[is_comp] else NULL
  
  y_ref <- data[[y_var]][is_ref]
  y_comp <- data[[y_var]][is_comp]
  
  # 1. Unadjusted difference
  mean_y_ref <- weighted_mean_safe(y_ref, w_ref)
  mean_y_comp <- weighted_mean_safe(y_comp, w_comp)
  unadjusted_diff <- mean_y_ref - mean_y_comp
  unadjusted_rel <- mean_y_comp / mean_y_ref
  
  # 2. Residual Direct Effect (RDE / Predictive Margins)
  comp_data <- data[is_comp, , drop = FALSE]
  ref_data <- data[is_ref, , drop = FALSE]
  
  pred_ref <- stats::predict(model, newdata = ref_data, type = "response")
  pred_comp <- stats::predict(model, newdata = comp_data, type = "response")
  
  # Recycled predictions: comparison individuals counterfactually assigned to ref_race
  comp_as_ref <- comp_data
  comp_as_ref[[race_col]] <- ref_race
  pred_comp_as_ref <- stats::predict(model, newdata = comp_as_ref, type = "response")
  
  mean_pred_ref <- weighted_mean_safe(pred_ref, w_ref)
  mean_pred_comp <- weighted_mean_safe(pred_comp, w_comp)
  mean_pred_comp_as_ref <- weighted_mean_safe(pred_comp_as_ref, w_comp)
  
  rde_diff <- mean_pred_comp_as_ref - mean_pred_comp
  rde_rel <- mean_pred_comp / mean_pred_comp_as_ref
  
  # 3. IOM Rank-and-Replace
  if (model_type == "glm") {
    hi_all <- calculate_health_index(model, data, need_vars)
    hi_ref <- hi_all[is_ref]
    hi_comp <- hi_all[is_comp]
    
    hi_comp_cf <- rank_replace(hi_ref, hi_comp, weights_ref = w_ref, weights_comp = w_comp)
    
    eta_comp <- stats::predict(model, newdata = comp_data, type = "link")
    eta_comp_cf <- eta_comp - hi_comp + hi_comp_cf
    pred_comp_cf <- model$family$linkinv(eta_comp_cf)
    
    hi_out <- list(ref = hi_ref, comp = hi_comp, comp_counterfactual = hi_comp_cf)
  } else {
    hi_df <- calculate_health_index(model, data, need_vars)
    hi1_ref <- hi_df$health_index_part1[is_ref]
    hi1_comp <- hi_df$health_index_part1[is_comp]
    hi1_comp_cf <- rank_replace(hi1_ref, hi1_comp, weights_ref = w_ref, weights_comp = w_comp)
    
    hi2_ref <- hi_df$health_index_part2[is_ref]
    hi2_comp <- hi_df$health_index_part2[is_comp]
    hi2_comp_cf <- rank_replace(hi2_ref, hi2_comp, weights_ref = w_ref, weights_comp = w_comp)
    
    eta1_comp <- stats::predict.glm(model$part1, newdata = comp_data, type = "link")
    eta1_comp_cf <- eta1_comp - hi1_comp + hi1_comp_cf
    pred1_comp_cf <- model$part1$family$linkinv(eta1_comp_cf)
    
    eta2_comp <- stats::predict.glm(model$part2, newdata = comp_data, type = "link")
    eta2_comp_cf <- eta2_comp - hi2_comp + hi2_comp_cf
    pred2_comp_cf <- model$part2$family$linkinv(eta2_comp_cf)
    
    pred_comp_cf <- pred1_comp_cf * pred2_comp_cf
    
    hi_out <- list(
      part1 = list(ref = hi1_ref, comp = hi1_comp, comp_counterfactual = hi1_comp_cf),
      part2 = list(ref = hi2_ref, comp = hi2_comp, comp_counterfactual = hi2_comp_cf)
    )
  }
  
  mean_pred_comp_cf <- weighted_mean_safe(pred_comp_cf, w_comp)
  iom_diff <- mean_pred_ref - mean_pred_comp_cf
  iom_rel <- mean_pred_comp_cf / mean_pred_ref
  
  means_summary <- data.frame(
    Group = c(paste("Reference (", ref_race, ")", sep = ""),
              paste("Comparison (", comp_race, ")", sep = ""),
              paste("Counterfactual (", comp_race, " with ", ref_race, " Need)", sep = "")),
    Observed_Mean = c(mean_y_ref, mean_y_comp, NA_real_),
    Model_Predicted_Mean = c(mean_pred_ref, mean_pred_comp, mean_pred_comp_cf),
    stringsAsFactors = FALSE
  )
  
  list(
    iom_disparity = iom_diff,
    iom_relative = iom_rel,
    rde_disparity = rde_diff,
    rde_relative = rde_rel,
    unadjusted_difference = unadjusted_diff,
    unadjusted_relative = unadjusted_rel,
    means = means_summary,
    health_indices = hi_out,
    model = model
  )
}
