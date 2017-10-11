set.seed(2094)
data <- array(rnorm(50), c(10, 5))



context("")
test_that("the best lag is", {
  expect_equal( best.lag(data, timepoints = (0, 5, 10, 15, 20, 25), C = 20), 0)
  expect_error( best.lag(data, timepoints = (0, 5, 10, 15, 20, 25, 30), C = 20))
  expect_error( best.lag(data, timepoints = (0, 5, 10, 15, 20, 25, 30), C = 20, max.lag = 6))
})
