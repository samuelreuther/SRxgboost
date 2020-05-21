#' F1 score
#'
#' The F1 score is the harmonic mean of the precision and recall in binary
#' classification.
#'
#' @param pred numeric vector
#' @param labels integer vector 0/1
#'
#' @return list with f1 value und optimal cutoff
#'
#' @examples
#' SRxgboost_f1(pred = seq(0.1, 1, 0.1),
#'              labels = c(1, 0, 0, 0, 1, 1, 0, 1, 1, 1))
#'
#' @export
SRxgboost_f1 <- function(pred,
                         labels) {
  # calculate f1 and optimal cutoff value
  pred <- ROCR::prediction(pred, labels)
  f <- ROCR::performance(pred, "f")
  err <- max(f@y.values[[1]], na.rm = T)
  opt_cutoff = f@x.values[[1]][which.max(f@y.values[[1]])]
  #
  # return results
  return(list(f1 = round(err, 5), opt_cutoff = round(opt_cutoff, 5)))
}

