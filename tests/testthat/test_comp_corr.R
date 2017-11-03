
context("")
test_that("the correlation is/are", {
  expect_equal( round(as.numeric(comp.corr(data = rbind(c(6, 3, 4, 8, NA, NA), c(6, 9, 7, 9, 10, NA)),
                                     time = rbind(c(10, 15, 20, 25, NA, NA), c(5, 10, 15, 20, 25, NA)), C = 7.8)), 3), 0.001)
  expect_equal( round(as.numeric(comp.corr(data = rbind(c(6, 3, 4, 8, 7, NA), c(NA, NA, 9, 7, 2, 10)),
                                           time = rbind(c(5, 10, 15, 20, 25, NA), c(NA, NA, 0, 5, 10, 15)), C = 12.2)), 3), 0.087)
  expect_equal( round(as.numeric(comp.corr(data = rbind(c(NA, NA, 12, 20, 2, 7), c(NA, 3, 1, 4, 14, 20)),
                                           time = rbind(c(NA, NA, 0, 5, 10, 15), c(NA, 0, 5, 10, 15, 20)), C = 7.8)), 3), -0.475)
  expect_equal( as.numeric(comp.corr(data = rbind(0:5, 1:6), time = rbind(c(0, 5, 10, 15, 20, 25), c(0, 5, 10, 15, 20, 25)), C = 10)), 1)
  expect_equal( as.numeric(comp.corr(data = rbind(c(1:5, NA), 1:6), time = rbind(c(5, 10, 15, 20, 25, NA), c(0, 5, 10, 15, 20, 25)), C = 10)), 0.082085)
  expect_error( as.numeric(comp.corr(data = rbind(0:5, 1:6), time = rbind(c(0, 5, 10, 15, 20, 25, 27), c(0, 5, 10, 15, 20, 25, 27)), C = 10)))
})
