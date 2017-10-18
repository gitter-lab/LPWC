context("")
test_that("The optimal C is/are", {
  expect_equal(findC(timepoints = c(0, 5, 10, 15, 20, 25, 30, 35), max.lag = 2)[4], c(0.3592573))
  expect_error(findC(timepoints = c(0, 5, 10, 15, 20, 25, 30, 35), max.lag = 3))
})
