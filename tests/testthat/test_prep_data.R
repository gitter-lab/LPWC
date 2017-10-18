context("")
test_that("the prep data is", {
  expect_equal( as.numeric(prep.data(data = array(c(0, 5, 6, 7, 9), c(1, 5)), lags = 2)), c(6, 7, 9, NA, NA))
  expect_equal( as.numeric(prep.data(data = array(c(3, 6, 7, 7, 9), c(1, 5)), lags = -2)), c(NA, NA, 3, 6, 7))
  expect_equal( as.numeric(prep.data(data = array(c(0, 5, 6, 7, 9), c(1, 5)), lags = 0)), c(0, 5, 6, 7, 9))
  expect_error( prep.data(data = array(c(0, 5, 6, 7, 9), c(1, 5)), lags = 10))
  expect_error( prep.data(data = array(c(0, 5, 6, 7, 9), c(1, 5)), lags = "a"))
})
