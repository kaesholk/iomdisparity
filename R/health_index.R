#' Calculate Need-Based Health Index
#'
#' @description Computes a univariate summary index of health status or clinical
#'   need based on the estimated coefficients of health-need variables from a fitted
#'   regression model (GLM or two-part model), as formulated in the IOM disparity
#'   framework (McGuire et al., 2006; Cook et al., 2010).
#'
#' @details The health index represents the linear prediction component attributable
#'   specifically to clinical need variables:
#'   \deqn{HI_i = \sum_{k \in \text{Need}} X_{ik} \hat{\beta}_k}
#'   Categorical factors, polynomials, and interaction terms belonging to
#'   \code{need_vars} are handled via the model matrix term assignments.
#'
#' @param model A fitted regression model object of class \code{\link[stats]{glm}},
#'   \code{\link[stats]{lm}}, or \code{two_part_model}.
#' @param data A data frame containing the covariates used in the model.
#' @param need_vars Character vector of variable names representing clinical need
#'   or health status (e.g. age, physical functioning, chronic conditions).
#' @param include_intercept Logical; if \code{TRUE}, adds the model intercept
#'   to the index. Defaults to \code{FALSE} as constants do not affect rankings.
#'
#' @return A numeric vector of calculated health index values of length \code{nrow(data)}.
#'   If \code{model} is a \code{two_part_model}, returns a data frame containing
#'   \code{health_index_part1} and \code{health_index_part2}.
#'
#' @references
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
#' @export
#'
#' @examples
#' data(meps_synthetic)
#' fit <- glm(any_mental_health_use ~ age + female + chronic_conditions +
#'              sf12_physical + sf12_mental + income_poverty_ratio + race,
#'            data = meps_synthetic, family = binomial())
#' hi <- calculate_health_index(fit, meps_synthetic,
#'                              need_vars = c("age", "female", "chronic_conditions",
#'                                            "sf12_physical", "sf12_mental"))
#' summary(hi)
calculate_health_index <- function(model, data, need_vars, include_intercept = FALSE) {
  if (inherits(model, "two_part_model")) {
    hi1 <- calculate_health_index_single(model$part1, data, need_vars, include_intercept)
    hi2 <- calculate_health_index_single(model$part2, data, need_vars, include_intercept)
    return(data.frame(
      health_index_part1 = hi1,
      health_index_part2 = hi2
    ))
  } else {
    return(calculate_health_index_single(model, data, need_vars, include_intercept))
  }
}

calculate_health_index_single <- function(model, data, need_vars, include_intercept = FALSE) {
  extracted <- extract_term_columns(model, data, need_vars, include_intercept = include_intercept)
  mm_sub <- extracted$matrix
  col_names <- extracted$col_names
  
  all_coefs <- stats::coef(model)
  missing_coefs <- setdiff(col_names, names(all_coefs))
  if (length(missing_coefs) > 0) {
    stop(sprintf("Model missing coefficients for columns: %s", paste(missing_coefs, collapse = ", ")), call. = FALSE)
  }
  
  coef_sub <- all_coefs[col_names]
  # Replace any NA coefficients (due to collinearity) with 0
  coef_sub[is.na(coef_sub)] <- 0
  
  hi <- as.vector(mm_sub %*% coef_sub)
  return(hi)
}