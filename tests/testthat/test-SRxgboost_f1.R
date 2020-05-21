test_that("result", {
  expect_equal(SRxgboost_f1(pred = seq(0.1, 1, 0.1),
                            labels = c(1, 0, 0, 0, 1, 1, 0, 1, 1, 1)),
               list(f1 = 0.83333, opt_cutoff = 0.5))
})
