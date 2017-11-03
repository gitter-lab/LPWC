context("")
test_that("the prep data is", {
  expect_equal(as.numeric(prep.data(data = array(c(0, 5, 6, 7, 9, 10, 2, 3), c(1, 8)), lags = 2, timepoints = c(0, 5, 10, 15, 20, 25, 30, 35))$data),
                c(NA, NA, 0, 5, 6, 7, 9, 10))
  expect_equal(as.numeric(prep.data(data = array(c(3, 6, 7, 7, 9), c(1, 5)), lags = -1, timepoints = c(0, 5, 10, 15, 20))$data),
               c(6, 7, 7, 9 , NA))
  expect_equal(as.numeric(prep.data(data = array(c(3, 6, 7, 7, 9), c(1, 5)), lags = -1, timepoints = c(0, 5, 10, 15, 20))$time),
               c(5, 10, 15, 20, NA))
  expect_equal(as.numeric(prep.data(data = array(c(0, 5, 6, 7, 9), c(1, 5)), lags = 0, timepoints = c(0, 5, 10, 15, 20))$data), c(0, 5, 6, 7, 9))
  expect_error(prep.data(data = array(c(0, 5, 6, 7, 9), c(1, 5)), timepoints = c(0, 5, 10, 15, 20), lags = 10))
  expect_error(prep.data(data = array(c(0, 5, 6, 7, 9), c(1, 5)), timepoints = c(0, 5, 10, 15, 20), lags = "a"))
})
