context("")
test_that("the weighted correlation is", {
  expect_equal(wt.corr(1:8, 2:9, rep(0, 8)), 1)
  expect_equal(wt.corr(1:6, 2:7, c(2, 1, 2, 2, 1, 1)), 1)
  expect_equal(round(wt.corr(c(1, 2, -9, 4, 5), c(2:6), c(0.5, 1, 2, 0.5, 2)), 2), 0.42)
  expect_error(wt.corr(1:6, 2:7, c(2, 1, 2, 2, 1, 1, 6)))
  expect_error(wt.corr(c("a", 2:6), 2:7, c(2, 1, 2, 2, 1, 1, 6)))
})
