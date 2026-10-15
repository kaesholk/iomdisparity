test_that("two_part_model fits both parts and predicts correctly", {
  data("meps_synthetic", package = "iomdisparity")
  
  tpm <- two_part_model(
    mental_health_expenditure ~ age + female + income_poverty_ratio + race,
    data = meps_synthetic
  )
  
  expect_s3_class(tpm, "two_part_model")
  expect_s3_class(tpm$part1, "glm")
  expect_s3_class(tpm$part2, "glm")
  
  # Predictions
  p1 <- predict(tpm, newdata = meps_synthetic[1:10, ], type = "part1")
  p2 <- predict(tpm, newdata = meps_synthetic[1:10, ], type = "part2")
  resp <- predict(tpm, newdata = meps_synthetic[1:10, ], type = "response")
  
  expect_equal(resp, p1 * p2)
  expect_true(all(p1 >= 0 & p1 <= 1))
  expect_true(all(p2 >= 0))
})

test_that("two_part_model S3 methods work as expected", {
  data("meps_synthetic", package = "iomdisparity")
  
  tpm <- two_part_model(
    mental_health_expenditure ~ age + race,
    data = meps_synthetic
  )
  
  cf <- coef(tpm)
  expect_type(cf, "list")
  expect_named(cf, c("part1", "part2"))
  
  s <- summary(tpm)
  expect_s3_class(s, "summary.two_part_model")
  
  output <- capture.output(print(tpm))
  expect_true(any(grepl("Two-Part Model", output)))
})
