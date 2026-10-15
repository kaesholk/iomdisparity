#' Rank-and-Replace Quantile Mapping
#'
#' @description Transforms the distribution of a variable (or health index) for a
#'   comparison group to match the distribution of a reference group while strictly
#'   preserving the relative rank order and sample size of the comparison group.
#'
#' @details The rank-and-replace method maps each observation in the comparison group
#'   to the value at the corresponding percentile in the reference group's distribution:
#'   \deqn{p_i = \frac{\text{rank}(v_i) - 0.5}{N_{\text{comp}}}}
#'   \deqn{v_i^* = Q_{\text{ref}}(p_i)}
#'   where \eqn{Q_{\text{ref}}} is the quantile function of the reference distribution.
#'   This achieves exact distribution matching for continuous variables without
#'   dropping observations or distorting non-parametric correlations with socioeconomic
#'   covariates.
#'
#' @param ref_values Numeric vector of values from the reference population.
#' @param comp_values Numeric vector of values from the comparison population.
#' @param weights_ref Optional numeric vector of sampling weights for the reference group.
#' @param weights_comp Optional numeric vector of sampling weights for the comparison group.
#' @param ties_method Method for resolving ties in ranks; passed to \code{\link[base]{rank}}.
#'   Defaults to \code{"average"}.
#'
#' @return A numeric vector of counterfactual values with length equal to
#'   \code{length(comp_values)}.
#'
#' @references
#'   McGuire, T. G., Alegria, M., Cook, B. L., Wells, K. B., & Zaslavsky, A. M. (2006).
#'   Implementing the Institute of Medicine definition of disparities: an application
#'   to mental health care. \emph{Health Services Research}, 41(5), 1979-2005.
#'   \doi{10.1111/j.1475-6773.2006.00583.x}
#'
#'   Cook, B. L., McGuire, T. G., & Zaslavsky, A. M. (2012). Measuring racial/ethnic
#'   disparities in health care: methods and practical issues. \emph{Health Services Research},
#'   47(3 Pt 2), 1232-1254. \doi{10.1111/j.1475-6773.2012.01387.x}
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' white_hi <- rnorm(1000, mean = 2.0, sd = 1.0)
#' black_hi <- rnorm(300, mean = 2.5, sd = 1.2)
#'
#' black_counterfactual <- rank_replace(white_hi, black_hi)
#'
#' # Comparison group size is preserved
#' length(black_counterfactual) == length(black_hi)
#'
#' # Mean and variance match the reference group
#' c(mean(white_hi), mean(black_counterfactual))
#' c(sd(white_hi), sd(black_counterfactual))
rank_replace <- function(ref_values, comp_values,
                         weights_ref = NULL, weights_comp = NULL,
                         ties_method = c("average", "first", "random", "max", "min")) {
  ties_method <- match.arg(ties_method)
  
  if (!is.numeric(ref_values) || !is.numeric(comp_values)) {
    stop("`ref_values` and `comp_values` must both be numeric vectors.", call. = FALSE)
  }
  if (length(ref_values) == 0 || length(comp_values) == 0) {
    stop("Input vectors cannot be empty.", call. = FALSE)
  }
  
  n_comp <- length(comp_values)
  
  if (!is.null(weights_comp)) {
    if (length(weights_comp) != n_comp) {
      stop("`weights_comp` must have same length as `comp_values`.", call. = FALSE)
    }
    ord <- order(comp_values)
    w_ord <- weights_comp[ord]
    cum_w <- cumsum(w_ord) - 0.5 * w_ord
    probs_ord <- cum_w / sum(w_ord)
    probs <- numeric(n_comp)
    probs[ord] <- probs_ord
  } else {
    probs <- (rank(comp_values, ties.method = ties_method) - 0.5) / n_comp
  }
  
  # Bounds check on probs to avoid extrapolation
  probs <- pmin(pmax(probs, 1e-6), 1 - 1e-6)
  
  mapped <- weighted_quantile_safe(ref_values, probs = probs, w = weights_ref)
  return(mapped)
}