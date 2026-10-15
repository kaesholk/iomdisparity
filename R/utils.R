#' Internal Utility Functions for iomdisparity
#'
#' @description Helper routines for data validation, term extraction from model
#'   matrices, and weighted statistics.
#'
#' @keywords internal
#' @noRd

validate_iom_inputs <- function(data, formula, race_col, ref_race, comp_race, need_vars, weights = NULL) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(race_col) || length(race_col) != 1 || !race_col %in% names(data)) {
    stop(sprintf("`race_col` must be a single column name present in `data` (got '%s').", as.character(race_col)), call. = FALSE)
  }
  race_vals <- unique(as.character(data[[race_col]]))
  if (!as.character(ref_race) %in% race_vals) {
    stop(sprintf("`ref_race` ('%s') was not found in column '%s'. Available values: %s.",
                 as.character(ref_race), race_col, paste(race_vals, collapse = ", ")), call. = FALSE)
  }
  if (!as.character(comp_race) %in% race_vals) {
    stop(sprintf("`comp_race` ('%s') was not found in column '%s'. Available values: %s.",
                 as.character(comp_race), race_col, paste(race_vals, collapse = ", ")), call. = FALSE)
  }
  if (as.character(ref_race) == as.character(comp_race)) {
    stop("`ref_race` and `comp_race` must be distinct categories.", call. = FALSE)
  }
  if (!is.character(need_vars) || length(need_vars) == 0) {
    stop("`need_vars` must be a non-empty character vector of variable names.", call. = FALSE)
  }
  missing_need <- setdiff(need_vars, names(data))
  if (length(missing_need) > 0) {
    stop(sprintf("The following `need_vars` were not found in `data`: %s.",
                 paste(missing_need, collapse = ", ")), call. = FALSE)
  }
  if (!is.null(weights)) {
    if (is.character(weights) && length(weights) == 1) {
      if (!weights %in% names(data)) {
        stop(sprintf("Weight column '%s' not found in `data`.", weights), call. = FALSE)
      }
    } else if (is.numeric(weights)) {
      if (length(weights) != nrow(data)) {
        stop("Numeric `weights` vector must have length equal to `nrow(data)`.", call. = FALSE)
      }
    } else {
      stop("`weights` must be either a column name or a numeric vector.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

extract_term_columns <- function(model, data, term_names, include_intercept = FALSE) {
  tt <- stats::terms(model)
  all_term_labels <- attr(tt, "term.labels")
  
  matched_terms <- intersect(term_names, all_term_labels)
  if (length(matched_terms) == 0) {
    stop("None of the specified `need_vars` match terms in the fitted model formula.", call. = FALSE)
  }
  
  term_indices <- which(all_term_labels %in% matched_terms)
  
  # Delete response to build design matrix on data
  tt_no_resp <- stats::delete.response(tt)
  mm <- stats::model.matrix(tt_no_resp, data = data, xlev = model$xlevels)
  
  assign_attr <- attr(mm, "assign")
  cols <- which(assign_attr %in% term_indices)
  
  if (include_intercept) {
    int_col <- which(colnames(mm) == "(Intercept)")
    if (length(int_col) > 0) {
      cols <- c(int_col, cols)
    }
  }
  
  if (length(cols) == 0) {
    stop("No design matrix columns found for the specified need variables.", call. = FALSE)
  }
  
  return(list(matrix = mm[, cols, drop = FALSE], col_names = colnames(mm)[cols]))
}

weighted_mean_safe <- function(x, w = NULL) {
  if (is.null(w)) {
    return(mean(x, na.rm = TRUE))
  }
  valid <- !is.na(x) & !is.na(w) & (w > 0)
  if (!any(valid)) return(NA_real_)
  sum(x[valid] * w[valid]) / sum(w[valid])
}

weighted_quantile_safe <- function(x, probs, w = NULL) {
  if (is.null(w)) {
    return(stats::quantile(x, probs = probs, na.rm = TRUE, type = 7, names = FALSE))
  }
  valid <- !is.na(x) & !is.na(w) & (w > 0)
  x_clean <- x[valid]
  w_clean <- w[valid]
  if (length(x_clean) == 0) return(rep(NA_real_, length(probs)))
  
  ord <- order(x_clean)
  x_sort <- x_clean[ord]
  w_sort <- w_clean[ord]
  
  cum_w <- cumsum(w_sort) / sum(w_sort)
  # Linear interpolation of empirical CDF
  q_vals <- stats::approx(x = cum_w, y = x_sort, xout = probs, rule = 2, ties = "ordered")$y
  return(q_vals)
}
