set.seed(2094)
rand.data <- array(rnorm(50), c(10, 5))


context("best.lag")
test_that("the best lag is", {
  expect_equal(best.lag(rand.data, timepoints = c(0, 5, 10, 15, 20), C = 20), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  expect_equal(best.lag(rand.data, timepoints = c(0, 5, 10, 15, 20), C = 200), c(-1, 0, 0, 0, -1, 1, -1, 0, 0, 0))
  expect_equal(as.numeric(best.lag(simdata[1:15,], timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72), max.lag = 2, C = 5000)),
               c(0, 0, 0, 0, 1, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0))
  expect_equal(as.numeric(best.lag(simdata[1:15, ], timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72), max.lag = 2, C = 50)),
               rep(0, 15))
  expect_equal(names(best.lag(simdata[1:15, ], timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72), max.lag = 2, C = 50)),
               row.names(simdata[1:15,]))
  expect_error(best.lag(rand.data, timepoints = c(0, 5, 10, 15, 20, 25, 30), C = 20))
  expect_error(best.lag(rand.data, timepoints = c(0, 5, 10, 15, 20, 25, 30), C = 20, max.lag = 6))
  expect_error(best.lag(rand.data, timepoints = c(0, 5, 10, 15, 20, 25, 30), C = -1.3, max.lag = 1))
  expect_error(best.lag(rand.data, timepoints = c(0, 5, 10, 15, 20, 25, 30), C = 2.8, max.lag = 1.5))
  expect_error(best.lag(array(10, c(10, 10)), seq(1, 10, 1), max.lag = 1, C = 10))
})
