#' SRxgboost_get_summary_CV
#'
#' Import the summary of 'SRxgboost_run()'.
#'
#' @param lauf character
#' @param ensemble boolean
#'
#' @return
#'
#' @examples
#'
#' @export
SRxgboost_get_summary_CV <- function(lauf,
                                     ensemble = FALSE) {
  # Read results
  if (!ensemble) {
    SummaryCV <- utils::read.table(paste0(path_output, gsub(".csv", "/", lauf),
                                          "Summary.csv"),
                                   header = TRUE, sep = ";", dec = ",")
  } else {
    SummaryCV <- utils::read.table(paste0(path_output, gsub(".csv", "/", lauf),
                                          "Summary ensemble.csv"),
                                   header = TRUE, sep = ";", dec = ",")
  }
  SummaryCV <- SummaryCV[!is.na(SummaryCV$date), ]
  SummaryCV$index <- as.numeric(row.names(SummaryCV))
  SummaryCV$date <- as.POSIXct(SummaryCV$date, tz = "")
  #
  # Maximise metric
  metrics_maximize <- ifelse(SummaryCV$metric[1] %in% c("auc", "qwk_score", "f1_score",
                                                        "mcc_score"), TRUE, FALSE)
  #
  if (sum(is.na(SummaryCV$test)) == nrow(SummaryCV)) {
    relevant <- "eval_1fold"
  } else {
    relevant <- "test"
  }
  if (metrics_maximize == TRUE) {
    for (j in 1:length(SummaryCV[, relevant])) {
      SummaryCV$best_test[j] <- max(SummaryCV[, relevant][1:j], na.rm = TRUE)
    }
    SummaryCV <- SummaryCV[order(-SummaryCV[, relevant], -SummaryCV$eval_1fold),]
  } else {
    for (j in 1:length(SummaryCV[, relevant])) {
      SummaryCV$best_test[j] <- min(SummaryCV[, relevant][1:j], na.rm = TRUE)
    }
    SummaryCV <- SummaryCV[order(SummaryCV$test, SummaryCV$eval_1fold),]
  }
  #
  # Give back data.frame
  assign("SummaryCV", SummaryCV, envir = .GlobalEnv)
}
