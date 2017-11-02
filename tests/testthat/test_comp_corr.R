
context("")
test_that("the weighted corr is", {
  expect_equal( as.numeric(comp.corr(data = rbind(0:5, 1:6), timepoints = c(0, 5, 10, 15, 20, 25), C = 10)), 1)
  expect_equal( as.numeric(comp.corr(data = rbind(c(1:5, NA), 1:6), timepoints = c(0, 5, 10, 15, 20, 25), C = 10)), 0.082085)
  expect_error( as.numeric(comp.corr(data = rbind(0:5, 1:6), timepoints = c(0, 5, 10, 15, 20, 25, 27), C = 10)))
})
