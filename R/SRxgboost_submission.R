#' SRxgboost_submission
#'
#' Get predictions from TESTforecast.csv
#'
#' @param lauf character
#' @param rank integer
#' @param rounding integer
#' @param id_name character
#' @param y_name character
#' @param y_transformation character
#'
#' @return prediction as csv-file and as data.frame in memory
#'
#' @export
SRxgboost_submission <- function(lauf,
                                 rank = 1,
                                 rounding = 4,
                                 id_name,
                                 y_name,
                                 y_transformation = NULL) {
  # load result table
  SRxgboost_get_summary_CV(lauf)
  #
  # load TESTforecast
  TESTforecast <- utils::read.table(file = paste0(path_output, gsub(".csv", "/", lauf),
                                                  gsub(".csv", "", lauf), "_TESTforecast.csv"),
                                    header = TRUE, sep = ";", dec = ",")
  #
  # get TESTforecast of single model
  i_TESTforecast <-
    stringdist::stringdistmatrix(gsub("-",".",gsub(":",".",SummaryCV$date)),
                                 gsub("X","",colnames(TESTforecast)[2:ncol(TESTforecast)]))[rank, ]
  i_TESTforecast <- which.min(i_TESTforecast)
  as.character(SummaryCV$date[rank])
  colnames(TESTforecast[i_TESTforecast + 1])
  pr <- TESTforecast[, i_TESTforecast + 1]
  if (!is.null(y_transformation)) {
    if (y_transformation == "log_p1") {
      pr <- round(exp(TESTforecast[, i_TESTforecast + 1]) - 1, rounding)
    }
  }
  pr <- round(pr, rounding)
  print(summary(pr))
  print(ggplot2::ggplot(data.frame(pr), ggplot2::aes(x = pr)) +
          ggplot2::geom_histogram(bins = 100))
  #
  # TODO: Forecast Median of x models
  # for (i_SummaryCV in 1:anzahl_models) {
  #   i_TESTforecast <- stringdist::stringdistmatrix(
  #     gsub("-", ".", gsub(":",".", SummaryCV$date)),
  #     gsub("X", "", colnames(TESTforecast)[2:ncol(TESTforecast)]))[i_SummaryCV, ]
  #   i_TESTforecast <- which.min(i_TESTforecast); as.character(SummaryCV$date[i_SummaryCV])
  #   colnames(TESTforecast[i_TESTforecast + 1])
  #   pr[, paste0("pr", i_SummaryCV)] <- round(TESTforecast[, i_TESTforecast+1], rounding)
  # }; summary(pr)
  # pr$median <- apply(pr[, 2:ncol(pr)], 1, "median"); summary(pr); hist(pr$median)
  # submit <- data.frame(ID=test$ID, PredictedProb=pr$median); utils::head(submit)
  #
  # TODO: convert long columns to matrix MULTILABEL
  # pr <- matrix(pr, ncol=length(unique(y)), byrow=TRUE)
  #
  # return result
  submit <- data.frame(id = TESTforecast[, 1], pr)
  data.table::setnames(submit, c(id_name, y_name))
  print(utils::head(submit))
  modelname <- paste0("Submission - ", substr(lauf, 1, nchar(lauf) - 4), " - ",
                      gsub(":", ".", as.character(SummaryCV$date)),
                      " d", SummaryCV$depth, " e", SummaryCV$eta, " n", SummaryCV$nround,
                      " mcw", SummaryCV$min_child_weight, " g", SummaryCV$gamma,
                      " s", SummaryCV$subsample, " c", SummaryCV$colsample,
                      " train", round(SummaryCV$train, 4), " test", round(SummaryCV$test, 4),
                      " 1fold", round(SummaryCV$eval_1fold, 4),
                      " time", SummaryCV$runtime, ".csv")
  print(modelname[rank])
  utils::write.csv(submit, file = paste0(path_output, modelname[rank]),
                   row.names = FALSE, quote = FALSE)
  assign('Submission', submit, envir = .GlobalEnv)
}
