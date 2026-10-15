test_that("calculate_health_index handles numeric and factor variables correctly", {
  data("meps_synthetic", package = "iomdisparity")
  
  fit <- stats::glm(
    any_mental_health_use ~ age + female + activity_limitations + income_poverty_ratio + race,
    data = meps_synthetic,
    family = stats::binomial()
  )
  
  # activity_limitations is a 3-level factor: "None", "Some", "Severe"
  need_vars <- c("age", "female", "activity_limitations")
  hi <- calculate_health_index(fit, meps_synthetic, need_vars = need_vars)
  
  expect_type(hi, "double")
  expect_length(hi, nrow(meps_synthetic))
  expect_false(any(is.na(hi)))
  
  # Hand-calculate expected linear combination for row 1
  tt <- stats::delete.response(stats::terms(fit))
  mm <- stats::model.matrix(tt, data = meps_synthetic[1, , drop = FALSE], xlev = fit$xlevels)
  cols_expected <- c("age", "female", "activity_limitationsSome", "activity_limitationsSevere")
  manual_hi <- sum(mm[1, cols_expected] * stats::coef(fit)[cols_expected])
  
  expect_equal(hi[1], manual_hi)
})

test_that("calculate_health_index errors on invalid need variables", {
  data("meps_synthetic", package = "iomdisparity")
  
  fit <- stats::glm(any_mental_health_use ~ age + race, data = meps_synthetic, family = stats::binomial())
  
  expect_error(
    calculate_health_index(fit, meps_synthetic, need_vars = c("nonexistent_var")),
    "None of the specified `need_vars` match terms"
  )
})

test_that("calculate_health_index works for two-part models", {
  data("meps_synthetic", package = "iomdisparity")
  
  tpm <- two_part_model(
    mental_health_expenditure ~ age + female + chronic_conditions + race,
    data = meps_synthetic
  )
  
  hi_df <- calculate_health_index(tpm, meps_synthetic, need_vars = c("age", "female", "chronic_conditions"))
  
  expect_s3_class(hi_df, "data.frame")
  expect_named(hi_df, c("health_index_part1", "health_index_part2"))
  expect_equal(nrow(hi_df), nrow(meps_synthetic))
})
