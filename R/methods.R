#' S3 Methods for iom_disparity Objects
#'
#' @description Methods for printing, summarizing, plotting, and extracting
#'   coefficients and confidence intervals from \code{iom_disparity} objects.
#'
#' @name methods
NULL

#' Print IOM Disparity Object
#'
#' @param x An object of class \code{iom_disparity}.
#' @param digits Number of decimal places to print.
#' @param ... Unused.
#' @return Invisibly returns \code{x}.
#' @export
print.iom_disparity <- function(x, digits = 4, ...) {
  cat("\n=======================================================\n")
  cat("  Institute of Medicine (IOM) Healthcare Disparity Analysis\n")
  cat("=======================================================\n")
  cat(sprintf("Reference Group : %s\n", x$settings$ref_race))
  cat(sprintf("Comparison Group: %s\n", x$settings$comp_race))
  cat(sprintf("Model Type      : %s\n", x$settings$model_type))
  cat(sprintf("Bootstrap Reps  : %d\n", x$settings$n_boot))
  cat("-------------------------------------------------------\n")
  cat("Estimated Disparities:\n\n")
  
  df_print <- x$estimates
  df_print$Disparity <- round(df_print$Disparity, digits)
  df_print$Std_Error <- round(df_print$Std_Error, digits)
  df_print$CI_Lower <- round(df_print$CI_Lower, digits)
  df_print$CI_Upper <- round(df_print$CI_Upper, digits)
  df_print$Relative_Ratio <- round(df_print$Relative_Ratio, digits)
  
  print(df_print, row.names = FALSE)
  cat("-------------------------------------------------------\n")
  cat("Note: Disparity = Reference Mean - Comparison (or Counterfactual) Mean.\n")
  cat("Relative_Ratio = Comparison / Reference (>1.0 indicates minority advantage).\n\n")
  invisible(x)
}

#' Summary of IOM Disparity Object
#'
#' @param object An object of class \code{iom_disparity}.
#' @param ... Unused.
#' @return An object of class \code{summary.iom_disparity}.
#' @export
summary.iom_disparity <- function(object, ...) {
  structure(
    list(
      estimates = object$estimates,
      means = object$means,
      settings = object$settings,
      health_indices = object$health_indices,
      call = object$call
    ),
    class = "summary.iom_disparity"
  )
}

#' @export
print.summary.iom_disparity <- function(x, digits = 4, ...) {
  cat("\n=======================================================\n")
  cat("  Detailed IOM Healthcare Disparities Summary Report\n")
  cat("=======================================================\n")
  cat("Call:\n", paste(deparse(x$call), collapse = "\n"), "\n\n", sep = "")
  
  cat("--- Group Means and Counterfactuals ---\n")
  means_print <- x$means
  means_print$Observed_Mean <- round(means_print$Observed_Mean, digits)
  means_print$Model_Predicted_Mean <- round(means_print$Model_Predicted_Mean, digits)
  print(means_print, row.names = FALSE)
  cat("\n")
  
  cat("--- Method Comparison (Absolute & Relative Disparities) ---\n")
  est_print <- x$estimates
  est_print$Disparity <- round(est_print$Disparity, digits)
  est_print$Std_Error <- round(est_print$Std_Error, digits)
  est_print$CI_Lower <- round(est_print$CI_Lower, digits)
  est_print$CI_Upper <- round(est_print$CI_Upper, digits)
  est_print$Relative_Ratio <- round(est_print$Relative_Ratio, digits)
  print(est_print, row.names = FALSE)
  cat("\n")
  
  # Health index summary
  if (!is.null(x$health_indices$ref)) {
    cat("--- Need Index (Health Index) Moments ---\n")
    hi_summary <- data.frame(
      Group = c("Reference Factual", "Comparison Factual", "Comparison Counterfactual"),
      Mean = c(mean(x$health_indices$ref, na.rm = TRUE),
               mean(x$health_indices$comp, na.rm = TRUE),
               mean(x$health_indices$comp_counterfactual, na.rm = TRUE)),
      SD = c(stats::sd(x$health_indices$ref, na.rm = TRUE),
             stats::sd(x$health_indices$comp, na.rm = TRUE),
             stats::sd(x$health_indices$comp_counterfactual, na.rm = TRUE)),
      Median = c(stats::median(x$health_indices$ref, na.rm = TRUE),
                 stats::median(x$health_indices$comp, na.rm = TRUE),
                 stats::median(x$health_indices$comp_counterfactual, na.rm = TRUE))
    )
    hi_summary$Mean <- round(hi_summary$Mean, digits)
    hi_summary$SD <- round(hi_summary$SD, digits)
    hi_summary$Median <- round(hi_summary$Median, digits)
    print(hi_summary, row.names = FALSE)
    cat("\n")
  }
  invisible(x)
}

#' Confidence Intervals for IOM Disparity Object
#'
#' @param object An object of class \code{iom_disparity}.
#' @param parm Character vector of methods to return (\code{"iom"}, \code{"rde"}, \code{"unadjusted"}).
#' @param level Confidence level (ignored if bootstrapping was already conducted at a fixed level).
#' @param ... Unused.
#' @return A matrix of lower and upper confidence limits.
#' @export
confint.iom_disparity <- function(object, parm = NULL, level = 0.95, ...) {
  ci_mat <- as.matrix(object$estimates[, c("CI_Lower", "CI_Upper")])
  rownames(ci_mat) <- rownames(object$estimates)
  colnames(ci_mat) <- c(paste0(100 * (1 - object$settings$conf_level) / 2, "%"),
                        paste0(100 * (1 - (1 - object$settings$conf_level) / 2), "%"))
  if (!is.null(parm)) {
    ci_mat <- ci_mat[parm, , drop = FALSE]
  }
  return(ci_mat)
}

#' Extract Disparity Point Estimates
#'
#' @param object An object of class \code{iom_disparity}.
#' @param ... Unused.
#' @return Named numeric vector of disparity estimates.
#' @export
coef.iom_disparity <- function(object, ...) {
  res <- object$estimates$Disparity
  names(res) <- rownames(object$estimates)
  return(res)
}

#' Diagnostic and Forest Plots for IOM Disparity Analysis
#'
#' @description Generates publication-ready visualizations:
#' \itemize{
#'   \item \code{"distributions"}: Kernel density plot showing the health index distribution
#'     for the reference, comparison, and rank-and-replace counterfactual groups (replicating
#'     Figure 2 of McGuire et al., 2006).
#'   \item \code{"disparity"}: Forest plot comparing absolute disparity estimates and 95% CIs
#'     across IOM Rank-and-Replace, Residual Direct Effect (RDE), and Unadjusted differences.
#' }
#'
#' @param x An object of class \code{iom_disparity}.
#' @param type Type of plot: \code{"distributions"} or \code{"disparity"}.
#' @param ... Additional graphical parameters passed to \code{\link[graphics]{plot}}.
#'
#' @return Invisibly returns \code{NULL}.
#' @export
plot.iom_disparity <- function(x, type = c("distributions", "disparity"), ...) {
  type <- match.arg(type)
  
  if (type == "distributions") {
    hi <- if (!is.null(x$health_indices$ref)) {
      x$health_indices
    } else {
      x$health_indices$part1
    }
    
    d_ref <- stats::density(hi$ref, na.rm = TRUE)
    d_comp <- stats::density(hi$comp, na.rm = TRUE)
    d_cf <- stats::density(hi$comp_counterfactual, na.rm = TRUE)
    
    x_lim <- range(c(d_ref$x, d_comp$x, d_cf$x))
    y_lim <- c(0, max(c(d_ref$y, d_comp$y, d_cf$y)) * 1.15)
    
    graphics::plot(d_ref, col = "black", lwd = 2.5, lty = 1,
                   xlim = x_lim, ylim = y_lim,
                   main = paste("Health Need Index Distributions:\n",
                                x$settings$ref_race, "vs.", x$settings$comp_race),
                   xlab = "Clinical Need Index (Higher = Greater Need)",
                   ylab = "Relative Frequency (Density)", ...)
    graphics::lines(d_comp, col = "firebrick", lwd = 2, lty = 2)
    graphics::lines(d_cf, col = "royalblue", lwd = 2.5, lty = 3)
    
    graphics::legend("topright",
                     legend = c(paste(x$settings$ref_race, "(Reference)"),
                                paste(x$settings$comp_race, "(Factual)"),
                                paste(x$settings$comp_race, "(Counterfactual)")),
                     col = c("black", "firebrick", "royalblue"),
                     lty = c(1, 2, 3), lwd = c(2.5, 2, 2.5), bty = "n")
    
  } else if (type == "disparity") {
    est <- x$estimates
    n_meth <- nrow(est)
    y_pos <- rev(seq_len(n_meth))
    
    has_ci <- !any(is.na(est$CI_Lower)) && !any(is.na(est$CI_Upper))
    x_lim <- if (has_ci) {
      range(c(est$CI_Lower, est$CI_Upper, 0), na.rm = TRUE)
    } else {
      range(c(est$Disparity, 0), na.rm = TRUE)
    }
    # Add 10% margin to limits
    margin <- diff(x_lim) * 0.1
    x_lim <- c(x_lim[1] - margin, x_lim[2] + margin)
    
    graphics::plot(est$Disparity, y_pos, pch = 19, cex = 1.4, col = "navy",
                   xlim = x_lim, ylim = c(0.5, n_meth + 0.5), yaxt = "n",
                   xlab = "Estimated Disparity (Reference - Comparison)",
                   ylab = "", main = "Healthcare Disparity Estimates by Method", ...)
    
    graphics::axis(2, at = y_pos, labels = est$Method, las = 1, cex.axis = 0.85)
    graphics::abline(v = 0, lty = 2, col = "gray50")
    
    if (has_ci) {
      for (i in seq_len(n_meth)) {
        graphics::segments(est$CI_Lower[i], y_pos[i], est$CI_Upper[i], y_pos[i],
                           col = "navy", lwd = 2)
        graphics::segments(est$CI_Lower[i], y_pos[i] - 0.1, est$CI_Lower[i], y_pos[i] + 0.1,
                           col = "navy", lwd = 1.5)
        graphics::segments(est$CI_Upper[i], y_pos[i] - 0.1, est$CI_Upper[i], y_pos[i] + 0.1,
                           col = "navy", lwd = 1.5)
      }
    }
  }
  invisible(NULL)
}
