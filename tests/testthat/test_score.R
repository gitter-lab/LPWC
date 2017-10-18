context("")
test_that("score is", {
  expect_equal(score(c(0.2, 0.5, 0.6, 0.3, 0.7, 0.42), c(0, 0, 0, 1, 0, 1)), 0)
  expect_error(score(c(0, 2, 5, 5), c(0, 1, 0, 7, 2, 4)))
  expect_error(score(c(0, 2, 5, 5), c(0, 1, 0, "a")))
  expect_error(score(c(0, 2, 5, "d"), c(0, 1, 0, 7)))
})
