test_that("rank_replace preserves vector lengths and bounds", {
  set.seed(42)
  ref <- rnorm(1000, mean = 5, sd = 2)
  comp <- rnorm(250, mean = 7, sd = 3)
  
  comp_mapped <- rank_replace(ref, comp)
  
  # Crucial test: length must strictly match comparison group
  expect_equal(length(comp_mapped), length(comp))
  
  # Moments of mapped comparison should closely match reference
  expect_equal(mean(comp_mapped), mean(ref), tolerance = 0.25)
  expect_equal(stats::sd(comp_mapped), stats::sd(ref), tolerance = 0.25)
})

test_that("rank_replace preserves monotonicity of comparison group ranks", {
  set.seed(123)
  ref <- rnorm(500)
  comp <- c(1.2, 4.5, -2.1, 0.0, 3.3)
  
  comp_mapped <- rank_replace(ref, comp)
  
  # Relative order should be perfectly preserved
  expect_equal(order(comp), order(comp_mapped))
})

test_that("rank_replace handles sampling weights", {
  set.seed(99)
  ref <- rnorm(300, mean = 10, sd = 1)
  comp <- rnorm(100, mean = 15, sd = 2)
  w_ref <- runif(300, 1, 2)
  w_comp <- runif(100, 1, 2)
  
  comp_mapped <- rank_replace(ref, comp, weights_ref = w_ref, weights_comp = w_comp)
  expect_length(comp_mapped, length(comp))
  expect_false(any(is.na(comp_mapped)))
})

test_that("rank_replace errors on invalid inputs", {
  expect_error(rank_replace("a", 1:5), "must both be numeric")
  expect_error(rank_replace(numeric(0), 1:5), "cannot be empty")
  expect_error(rank_replace(1:10, 1:5, weights_comp = 1:4), "same length")
})
