#' Fit Two-Part Hurdle / Expenditure Model
#'
#' @description Fits a two-part generalized linear model for semi-continuous
#'   outcomes with substantial zero-mass (e.g., healthcare utilization and expenditures),
#'   as described by Manning & Mullahy (2001) and Cook et al. (2010).
#'
#' @details
#' Part 1 models the probability of any positive expenditure / utilization:
#' \deqn{\Pr(Y > 0 \mid X) = g_1^{-1}(X \beta_1)}
#' Part 2 models the expected level of expenditure conditional on positive use:
#' \deqn{E[Y \mid Y > 0, X] = g_2^{-1}(X \beta_2)}
#' The unconditional expectation is calculated as the product of the two parts:
#' \deqn{E[Y \mid X] = \Pr(Y > 0 \mid X) \times E[Y \mid Y > 0, X]}
#'
#' @param formula An object of class \code{\link[stats]{formula}} describing the model.
#' @param data A data frame containing the variables in the model.
#' @param weights Optional weights for model fitting (either vector or column name).
#' @param part1_family Error distribution and link function for Part 1 (probability of any use).
#'   Defaults to \code{\link[stats]{binomial}(link = "logit")}.
#' @param part2_family Error distribution and link function for Part 2 (level of positive use).
#'   Defaults to \code{\link[stats]{Gamma}(link = "log")}.
#'
#' @return An object of class \code{two_part_model} containing:
#'   \item{part1}{The fitted GLM for Part 1.}
#'   \item{part2}{The fitted GLM for Part 2.}
#'   \item{formula}{The model formula.}
#'   \item{call}{The matched call.}
#'
#' @references
#'   Manning, W. G., & Mullahy, J. (2001). Estimating log models: to transform or not to
#'   transform? \emph{Journal of Health Economics}, 20(4), 461-495.
#'   \doi{10.1016/S0167-6296(01)00086-8}
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
#' tpm <- two_part_model(
#'   mental_health_expenditure ~ age + female + income_poverty_ratio + race,
#'   data = meps_synthetic
#' )
#' summary(tpm)
#' preds <- predict(tpm, newdata = meps_synthetic[1:5, ])
two_part_model <- function(formula, data, weights = NULL,
                           part1_family = stats::binomial(link = "logit"),
                           part2_family = stats::Gamma(link = "log")) {
  cl <- match.call()
  y_var <- all.vars(formula)[1]
  
  if (!y_var %in% names(data)) {
    stop(sprintf("Response variable '%s' not found in `data`.", y_var), call. = FALSE)
  }
  
  y <- data[[y_var]]
  if (any(y < 0, na.rm = TRUE)) {
    stop("Response variable in a two-part model cannot contain negative values.", call. = FALSE)
  }
  
  # Resolve weights
  w_vec <- NULL
  if (!is.null(weights)) {
    if (is.character(weights) && length(weights) == 1) {
      w_vec <- data[[weights]]
    } else if (is.numeric(weights)) {
      w_vec <- weights
    }
  }
  
  # Part 1: Binary indicator
  data_p1 <- data
  data_p1[[y_var]] <- as.integer(y > 0)
  
  part1_fit <- if (is.null(w_vec)) {
    stats::glm(formula, data = data_p1, family = part1_family)
  } else {
    stats::glm(formula, data = data_p1, family = part1_family, weights = w_vec)
  }
  
  # Part 2: Positive outcome subset
  pos_idx <- which(y > 0)
  if (length(pos_idx) == 0) {
    stop("No positive observations found for Part 2 of the two-part model.", call. = FALSE)
  }
  
  data_p2 <- data[pos_idx, , drop = FALSE]
  w_vec_p2 <- if (!is.null(w_vec)) w_vec[pos_idx] else NULL
  
  part2_fit <- if (is.null(w_vec_p2)) {
    stats::glm(formula, data = data_p2, family = part2_family)
  } else {
    stats::glm(formula, data = data_p2, family = part2_family, weights = w_vec_p2)
  }
  
  res <- list(
    part1 = part1_fit,
    part2 = part2_fit,
    formula = formula,
    call = cl
  )
  class(res) <- "two_part_model"
  return(res)
}

#' Predict Method for Two-Part Models
#'
#' @param object An object of class \code{two_part_model}.
#' @param newdata Optional new data frame for predictions.
#' @param type Type of prediction: \code{"response"} for unconditional expected values
#'   (\eqn{\Pr(Y > 0) \times E[Y \mid Y > 0]}), \code{"part1"} for probability of positive use,
#'   or \code{"part2"} for conditional expected positive values.
#' @param ... Additional arguments passed to \code{\link[stats]{predict.glm}}.
#'
#' @return A numeric vector of predicted values.
#' @export
predict.two_part_model <- function(object, newdata = NULL,
                                   type = c("response", "part1", "part2"), ...) {
  type <- match.arg(type)
  
  pred1 <- stats::predict.glm(object$part1, newdata = newdata, type = "response", ...)
  pred2 <- stats::predict.glm(object$part2, newdata = newdata, type = "response", ...)
  
  if (type == "part1") {
    return(pred1)
  } else if (type == "part2") {
    return(pred2)
  } else {
    return(pred1 * pred2)
  }
}

#' Extract Coefficients from Two-Part Model
#'
#' @param object An object of class \code{two_part_model}.
#' @param ... Unused.
#' @return A list with elements \code{part1} and \code{part2}.
#' @export
coef.two_part_model <- function(object, ...) {
  list(
    part1 = stats::coef(object$part1),
    part2 = stats::coef(object$part2)
  )
}

#' Summary of Two-Part Model
#'
#' @param object An object of class \code{two_part_model}.
#' @param ... Unused.
#' @return A list with summaries for \code{part1} and \code{part2}.
#' @export
summary.two_part_model <- function(object, ...) {
  res <- list(
    part1 = summary(object$part1),
    part2 = summary(object$part2),
    call = object$call
  )
  class(res) <- "summary.two_part_model"
  return(res)
}

#' @export
print.summary.two_part_model <- function(x, ...) {
  cat("\n=== Two-Part Model Summary ===\n")
  cat("Call:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n\n", sep = "")
  cat("--- Part 1: Probability of Positive Outcome ---\n")
  print(x$part1$coefficients)
  cat("\n--- Part 2: Conditional Positive Level ---\n")
  print(x$part2$coefficients)
  cat("\n")
  invisible(x)
}

#' @export
print.two_part_model <- function(x, ...) {
  cat("Two-Part Model Object\n")
  cat("Formula: ", paste(deparse(x$formula), collapse = " "), "\n")
  cat("Part 1 Family: ", x$part1$family$family, "(", x$part1$family$link, ")\n", sep = "")
  cat("Part 2 Family: ", x$part2$family$family, "(", x$part2$family$link, ")\n", sep = "")
  invisible(x)
}
