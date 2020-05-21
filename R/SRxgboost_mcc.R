#' Matthews Correlation Coefficient
#'
#' "The Best Classification Metric You’ve Never Heard Of"
#' https://towardsdatascience.com/the-best-classification-metric-youve-never-heard-of-the-matthews-correlation-coefficient-3bf50a2f3e9a
#'
#' @param pred numeric vector
#' @param labels integer vector 0/1
#'
#' @return list with mcc value und optimal cutoff
#'
#' @examples
#' SRxgboost_mcc(pred = seq(0.1, 1, 0.1),
#'               labels = c(1, 0, 0, 0, 1, 1, 0, 1, 1, 1))
#'
#' @export
SRxgboost_mcc <- function(pred,
                          labels) {
  # initialise data.frame
  temp <- data.frame(pred = pred, labels = labels)
  #
  # NAs produced by integer overflow > 500'000
  if (length(pred) > 100000) {
    set.seed(12345)
    temp <- temp %>% sample_n(100000)
    set.seed(Sys.time())
  }
  #
  # calculate mcc and optimal cutoff value
  prediction <- ROCR::prediction(temp$pred, temp$labels)
  mcc <- ROCR::performance(prediction, "mat")
  err <- max(mcc@y.values[[1]], na.rm = T)
  opt_cutoff = mcc@x.values[[1]][which.max(mcc@y.values[[1]])]
  #
  # return results
  return(list(mcc = round(err, 5), opt_cutoff = round(opt_cutoff, 5)))
}

