test_that("bootstrap_iom computes valid standard errors and confidence intervals", {
  data("meps_synthetic", package = "iomdisparity")
  
  sub_df <- meps_synthetic[meps_synthetic$race %in% c("White", "Black"), ]
  
  res <- bootstrap_iom(
    sub_data = sub_df,
    formula = any_mental_health_use ~ age + female + race,
    race_col = "race",
    ref_race = "White",
    comp_race = "Black",
    need_vars = c("age", "female"),
    model_type = "glm",
    family = stats::binomial(),
    n_boot = 20,
    seed = 101
  )
  
  expect_named(res, c("replicates", "se", "ci", "valid_replicates"))
  expect_equal(res$valid_replicates, 20)
  expect_true(all(res$se > 0))
  expect_true(res$ci$iom[1] < res$ci$iom[2])
  expect_true(res$ci$rde[1] < res$ci$rde[2])
})
