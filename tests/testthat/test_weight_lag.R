context("")
test_that("the weight lag is", {
  expect_equal(weight.lag(x1 = c(0, 5, 6), x2 = c(9, 8, 3, 4))[1, ], c(0, 5, 6))
  expect_equal(weight.lag(x1 = c(0, 5, 6), x2 = c(9, 4, 3, 4))[2, ], c(9, 4, 3))
  expect_error(weight.lag(x1 = c(0, 5, 6), x2 = c(9, 2, 3, "a")))
  expect_error(weight.lag(x1 = c("h", 5, 6), x2 = c(9, 6, 3, 4)))
})
