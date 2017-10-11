context("")
test_that("the mean squared dist is", {
  expect_equal(meansq.dist(1:6, 2:7, c(2, 1, 2, 2, 1, 1)), 1)
  expect_equal(meansq.dist(c(1, 2, -9, 4, 5), c(2:6), 0.25))
  expect_error(meansq.dist(c(1, 4, 2), c(2, 6, 5, 7)))
  expect_error(meansq.dist(c("a", 2:6), 2:7))
})
