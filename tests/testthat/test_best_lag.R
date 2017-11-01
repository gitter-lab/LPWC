set.seed(2094)
sim.data <- array(rnorm(50), c(10, 5))


context("")
test_that("the best lag is", {
  expect_equal( best.lag(sim.data, timepoints = c(0, 5, 10, 15, 20), C = 20), c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  expect_equal( best.lag(sim.data, timepoints = c(0, 5, 10, 15, 20), C = 200), c(-1, 0, 0, 0, -1, 1, -1, 0, 0, 0))
  expect_equal( best.lag(impulse1[1:15, ], timepoints <- c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72), max.lag = 2, C = 5000),
                c(0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 1, 0, 0))
  expect_equal( best.lag(impulse1[1:15, ], timepoints <- c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72), max.lag = 2, C = 50),
                rep(0, 15))
  expect_error( best.lag(sim.data, timepoints = c(0, 5, 10, 15, 20, 25, 30), C = 20))
  expect_error( best.lag(sim.data, timepoints = c(0, 5, 10, 15, 20, 25, 30), C = 20, max.lag = 6))
})
