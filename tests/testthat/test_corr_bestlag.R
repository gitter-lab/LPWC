context("")
test_that("the corr is", {
  expect_equal(signif(corr.bestlag(data = simdata[1:10, ],
               timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72))$corr[5], 2), 0.93)
  expect_equal(as.numeric(corr.bestlag(data = simdata[50:70, ],
               timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72), C = 10000000)$lags[1]), 2)
  expect_equal(as.numeric(corr.bestlag(data = simdata[50:70, ],
               timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72), C = 10000000, penalty = "high")$lags[1]), 2)
  expect_equal(names(corr.bestlag(data = simdata[50:70, ], timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72),
              C = 10000000, penalty = "high")$lags[1]), rownames(simdata[50, ]))
  expect_equal(as.numeric(corr.bestlag(data = simdata[50:70, ],
               timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72), penalty = "high")$lags[1]), 1)
  expect_equal(as.numeric(corr.bestlag(data = simdata[50:60, ],
               timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72), penalty = "low")$lags[1]), 1)
  expect_equal(signif(corr.bestlag(data = simdata[20:30, ],
               timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72))$corr[5], 2), 0.91)
  expect_equal(signif(corr.bestlag(data = simdata[1:10, ],
               timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72), penalty = "low")$corr[10], 2), 0.91)
  expect_error(signif(corr.bestlag(data = simdata,
               timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72), max.lag = 4)$corr[10], 2))
  expect_error(corr.bestlag(data = simdata, timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72, 75)))
  expect_error(corr.bestlag(data = simdata, timepoints = c(0, 2, 4, 6, 8, 18, 24, 32, 48, 72),
                            max.lag = 2, C = -1))
  expect_error(corr.bestlag(data = simdata, timepoints = c(0, 1, 2, 4, 6, 8, 12, 16, 20, 25),
                            max.lag = 2.4))
  expect_error(corr.bestlag(data = simdata, timepoints = c(0, 1, 2, 4, 6, 8, 12, 16, 20, 25),
                            max.lag = 2, iter = 1))
  expect_error(corr.bestlag(data = simdata, timepoints = c(0, 1, 2, 4, 6, 8, 12, 16, 20, 25),
                            max.lag = 2, penalty = "nopenalty"))
  expect_error(corr.bestlag(data = simdata, timepoints = c(0, 1, 2, 4, 6, 8, 12, 16, 20, 25),
                            max.lag = 2, iter = 2.2))
  expect_error(corr.bestlag(array(10, c(10, 10)), seq(1, 10, 1), max.lag = 1))
})
