context("")
test_that("The optimal C is/are", {
  expect_equal(round(findC(timepoints = c(0, 5, 15, 30, 60, 75, 90, 120), max.lag = 2)[1], 2), 1065.97)
  expect_equal(round(findC(timepoints = c(0, 5, 10, 15, 20, 25, 30, 35), max.lag = 2)[4], 2), 132.97)
  expect_equal(round(findC(timepoints = c(0, 5, 15, 30, 60, 75, 90, 120), max.lag = 2)[5], 2), 2212.09)
  expect_error(findC(timepoints = c(0, 5, 10, 15, 20, 25, 30, 35), max.lag = 3))
})
