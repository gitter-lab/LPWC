context("")
test_that("the corr is", {
  expect_equal(signif(corr.bestlag(data = simdata[1:10, ],
               timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72))$corr[5], 2), 0.93)
  expect_equal(signif(corr.bestlag(data = simdata[20:30, ],
               timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72))$corr[5], 2), 0.91)
  expect_error(signif(corr.bestlag(data = simdata,
               timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72, 75))$corr[10], 2))
  expect_error(signif(corr.bestlag(data = simdata,
  timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72), max.lag = 4)$corr[10], 2))
})
