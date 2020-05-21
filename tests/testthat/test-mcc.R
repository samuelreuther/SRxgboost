test_that("result", {
  expect_equal(SRxgboost_mcc(pred = seq(0.1, 1, 0.1),
                             labels = c(1, 0, 0, 0, 1, 1, 0, 1, 1, 1)),
               list(mcc = 0.58333, opt_cutoff = 0.5))
})
