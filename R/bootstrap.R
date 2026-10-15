#' Stratified Bootstrap Inference for IOM Disparity Models
#'
#' @description Performs stratified resampling by racial/ethnic group to estimate
#'   standard errors and confidence intervals for disparity metrics, accounting for
#'   first-stage model estimation uncertainty and the rank-and-replace transformation.
#'
#' @param sub_data Subsampled data frame containing only reference and comparison groups.
#' @param formula Model formula.
#' @param race_col Character name of the race column.
#' @param ref_race Value of the reference race.
#' @param comp_race Value of the comparison race.
#' @param need_vars Character vector of need variable names.
#' @param model_type Type of model ("glm" or "two_part").
#' @param family Error distribution for GLM or Part 1.
#' @param part2_family Error distribution for Part 2.
#' @param weights_col Character name of weights column (or NULL).
#' @param n_boot Number of bootstrap iterations.
#' @param conf_level Confidence level (e.g., 0.95).
#' @param refit_with_index Logical; whether to refit with index.
#' @param seed Optional random seed for reproducibility.
#'
#' @return A list with bootstrap standard errors, confidence intervals, and replicate distributions.
#'
#' @keywords internal
#' @noRd
bootstrap_iom <- function(sub_data, formula, race_col, ref_race, comp_race, need_vars,
                          model_type = "glm", family = stats::gaussian(),
                          part2_family = stats::Gamma(link = "log"),
                          weights_col = NULL, n_boot = 200, conf_level = 0.95,
                          refit_with_index = FALSE, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  ref_indices <- which(sub_data[[race_col]] == ref_race)
  comp_indices <- which(sub_data[[race_col]] == comp_race)
  n_ref <- length(ref_indices)
  n_comp <- length(comp_indices)
  
  boot_reps <- data.frame(
    iom_disp = numeric(n_boot),
    rde_disp = numeric(n_boot),
    unadj_disp = numeric(n_boot)
  )
  
  valid_reps <- 0
  max_attempts <- n_boot * 2
  attempt <- 0
  
  while (valid_reps < n_boot && attempt < max_attempts) {
    attempt <- attempt + 1
    
    # Stratified resample within each group
    boot_ref_idx <- sample(ref_indices, size = n_ref, replace = TRUE)
    boot_comp_idx <- sample(comp_indices, size = n_comp, replace = TRUE)
    boot_idx <- c(boot_ref_idx, boot_comp_idx)
    boot_sample <- sub_data[boot_idx, , drop = FALSE]
    
    # Run single estimation iteration
    res <- tryCatch({
      estimate_disparities_core(
        data = boot_sample,
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
    }, error = function(e) NULL)
    
    if (!is.null(res) && !any(is.na(c(res$iom_disparity, res$rde_disparity, res$unadjusted_difference)))) {
      valid_reps <- valid_reps + 1
      boot_reps$iom_disp[valid_reps] <- res$iom_disparity
      boot_reps$rde_disp[valid_reps] <- res$rde_disparity
      boot_reps$unadj_disp[valid_reps] <- res$unadjusted_difference
    }
  }
  
  if (valid_reps < max(10, n_boot * 0.5)) {
    warning("High rate of bootstrap convergence failures. Results may be unreliable.", call. = FALSE)
  }
  
  boot_reps <- boot_reps[seq_len(valid_reps), , drop = FALSE]
  alpha <- 1 - conf_level
  probs <- c(alpha / 2, 1 - alpha / 2)
  
  se_iom <- stats::sd(boot_reps$iom_disp, na.rm = TRUE)
  se_rde <- stats::sd(boot_reps$rde_disp, na.rm = TRUE)
  se_unadj <- stats::sd(boot_reps$unadj_disp, na.rm = TRUE)
  
  ci_iom <- stats::quantile(boot_reps$iom_disp, probs = probs, na.rm = TRUE)
  ci_rde <- stats::quantile(boot_reps$rde_disp, probs = probs, na.rm = TRUE)
  ci_unadj <- stats::quantile(boot_reps$unadj_disp, probs = probs, na.rm = TRUE)
  
  list(
    replicates = boot_reps,
    se = c(iom = se_iom, rde = se_rde, unadjusted = se_unadj),
    ci = list(
      iom = ci_iom,
      rde = ci_rde,
      unadjusted = ci_unadj
    ),
    valid_replicates = valid_reps
  )
}