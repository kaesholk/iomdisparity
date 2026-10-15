test_that("iom_disparity works for binary GLM outcomes", {
  data("meps_synthetic", package = "iomdisparity")
  
  res <- iom_disparity(
    formula = any_mental_health_use ~ age + female + chronic_conditions + sf12_physical + income_poverty_ratio + race,
    data = meps_synthetic,
    race_col = "race",
    ref_race = "White",
    comp_race = "Black",
    need_vars = c("age", "female", "chronic_conditions", "sf12_physical"),
    model_type = "glm",
    family = stats::binomial(),
    n_boot = 0
  )
  
  expect_s3_class(res, "iom_disparity")
  expect_true(is.data.frame(res$estimates))
  expect_equal(nrow(res$estimates), 3)
  expect_named(res$estimates, c("Method", "Disparity", "Std_Error", "CI_Lower", "CI_Upper", "Relative_Ratio"))
  
  # Disparity should be positive (White > Black)
  expect_true(res$estimates["IOM", "Disparity"] > 0)
  expect_true(res$estimates["Unadjusted", "Disparity"] > 0)
})

test_that("iom_disparity is invariant to lexical ordering of race levels", {
  data("meps_synthetic", package = "iomdisparity")
  
  # Test with White as ref, Black as comp
  res1 <- iom_disparity(
    formula = any_mental_health_use ~ age + race,
    data = meps_synthetic,
    race_col = "race",
    ref_race = "White",
    comp_race = "Black",
    need_vars = "age",
    n_boot = 0
  )
  
  # Test with Black as ref, White as comp (should yield negative disparity of equal magnitude)
  res2 <- iom_disparity(
    formula = any_mental_health_use ~ age + race,
    data = meps_synthetic,
    race_col = "race",
    ref_race = "Black",
    comp_race = "White",
    need_vars = "age",
    n_boot = 0
  )
  
  expect_equal(res1$estimates["IOM", "Disparity"], -res2$estimates["IOM", "Disparity"], tolerance = 1e-2)
})

test_that("iom_disparity works with two_part expenditure models", {
  data("meps_synthetic", package = "iomdisparity")
  
  res <- iom_disparity(
    formula = mental_health_expenditure ~ age + female + chronic_conditions + income_poverty_ratio + race,
    data = meps_synthetic,
    race_col = "race",
    ref_race = "White",
    comp_race = "Hispanic",
    need_vars = c("age", "female", "chronic_conditions"),
    model_type = "two_part",
    n_boot = 0
  )
  
  expect_s3_class(res, "iom_disparity")
  expect_false(any(is.na(res$estimates$Disparity)))
})

test_that("S3 methods print, summary, coef, confint, plot execute without error", {
  data("meps_synthetic", package = "iomdisparity")
  
  res <- iom_disparity(
    formula = any_mental_health_use ~ age + race,
    data = meps_synthetic,
    race_col = "race",
    ref_race = "White",
    comp_race = "Black",
    need_vars = "age",
    n_boot = 10,
    seed = 123
  )
  
  expect_output(print(res), "Institute of Medicine")
  expect_output(print(summary(res)), "Detailed IOM Healthcare Disparities")
  
  cfs <- coef(res)
  expect_length(cfs, 3)
  
  ci <- confint(res)
  expect_equal(nrow(ci), 3)
  expect_false(any(is.na(ci)))
  
  # Plots without errors
  tmp_file <- tempfile(fileext = ".png")
  grDevices::png(tmp_file)
  expect_silent(plot(res, type = "distributions"))
  expect_silent(plot(res, type = "disparity"))
  grDevices::dev.off()
  unlink(tmp_file)
})
