#' SRxgboost_get_OOFforecast_TESTforecast
#'
#' Load OOFforecast and TESTforecast from rds-files.
#'
#' @param lauf character
#' @param top_rank integer
#' @param ensemble boolean
#'
#' @return predictions as data.frames in memory
#'
#' @export
SRxgboost_get_OOFforecast_TESTforecast <- function(lauf,
                                                   top_rank = 1,
                                                   ensemble = FALSE) {
  # check lauf ends with ".csv"
  if (!grepl('.csv$', lauf)) lauf <- paste0(lauf, ".csv")
  #
  # load result table
  SRxgboost_get_summary_CV(lauf, ensemble = ensemble)
  if (!exists("SummaryCV_META")) {
    SummaryCV_META <- SummaryCV %>%
      dplyr::filter(date < "2000-01-01") %>%
      dplyr::mutate(Lauf = lauf)
  }
  #
  # get OOFforecast
  try({
    if (!ensemble) {
      temp <- readRDS(paste0(path_output, gsub(".csv", "/", lauf),
                             "Data/OOFforecast.rds"))
    } else {
      temp <- readRDS(paste0(path_output, gsub(".csv", "/", lauf),
                             "Data/OOFforecast ensemble.rds"))
    }
    OOFforecast <- data.frame(id = temp[, 1])
    for (i in 1:top_rank) {
      if (i > ncol(temp) - 1) next()
      i_OOFforecast <-
        stringdist::stringdistmatrix(gsub("-",".", gsub(":", ".", SummaryCV$date)),
                                     gsub("X","", colnames(temp)[2:ncol(temp)]))[i, ]
      i_OOFforecast <- which(i_OOFforecast == min(i_OOFforecast))
      # i_OOFforecast <- which.min(i_OOFforecast)
      select <- temp %>%
        dplyr::select(i_OOFforecast + 1) %>%
        stats::setNames(paste0("m", names(.) %>%
                               gsub("-", "", .) %>%
                               gsub(":", "", .) %>%
                                 gsub(".", "", ., fixed = TRUE) %>%
                               gsub(" ", "x", .)))
      # if "multi:softprob" then determine class
      if (length(i_OOFforecast) > 1) {
        select <- select %>% mutate(class = apply(select, MARGIN = 1, which.max))
      }
      # select <- data.frame(temp[, i_OOFforecast + 1])
      # data.table::setnames(select, paste0("m",
      #                                     gsub("-", "",
      #                                          gsub(":", "",
      #                                               gsub(" ", "x", SummaryCV$date[i])))))
      OOFforecast <- cbind(OOFforecast, select)
      # save information of model performance
      SummaryCV_META <- rbind(SummaryCV_META, SummaryCV[i, ] %>% dplyr::mutate(Lauf = lauf))
      rm(temp)
    }
    assign('OOFforecast', OOFforecast, envir = .GlobalEnv)
    assign('SummaryCV_META', SummaryCV_META, envir = .GlobalEnv)
  })
  #
  # get TESTforecast
  if (!ensemble) {
    temp <- readRDS(paste0(path_output, gsub(".csv", "/", lauf),
                           "Data/TESTforecast.rds"))
  } else {
    temp <- readRDS(paste0(path_output, gsub(".csv", "/", lauf),
                           "Data/TESTforecast ensemble.rds"))
  }
  TESTforecast <- data.frame(id = temp[, 1])
  for (i in 1:top_rank) {
    if (i > ncol(temp) - 1) next()
    i_TESTforecast <-
      stringdist::stringdistmatrix(gsub("-", ".", gsub(":", ".", SummaryCV$date)),
                                   gsub("X", "", colnames(temp)[2:ncol(temp)]))[i, ]
    i_TESTforecast <- which(i_TESTforecast == min(i_TESTforecast))
    # i_TESTforecast <- which.min(i_TESTforecast)
    select <- temp %>%
      dplyr::select(i_TESTforecast + 1) %>%
      stats::setNames(paste0("m", names(.) %>%
                               gsub("-", "", .) %>%
                               gsub(":", "", .) %>%
                               gsub(".", "", ., fixed = TRUE) %>%
                               gsub(" ", "x", .)))
    # if "multi:softprob" then determine class
    if (length(i_OOFforecast) > 1) {
      select <- select %>% mutate(class = apply(select, MARGIN = 1, which.max))
    }
    # select <- data.frame(temp[, i_TESTforecast + 1])
    # data.table::setnames(select, paste0("m",
    #                                     gsub("-", "",
    #                                          gsub(":", "",
    #                                               gsub(" ", "x", SummaryCV$date[i])))))
    TESTforecast <- cbind(TESTforecast, select)
    rm(temp)
  }
  assign('TESTforecast', TESTforecast, envir = .GlobalEnv)
  #
  # get y_OOF
  temp <- readRDS(paste0(path_output, gsub(".csv", "/", lauf), "Data/y_OOF.rds"))
  y_OOF <- data.frame(y = temp[, 1])
  assign('y_OOF', y_OOF, envir = .GlobalEnv)
  #
  # get y_test
  suppressWarnings({
    try({
      temp <- readRDS(paste0(path_output, gsub(".csv", "/", lauf), "Data/y_test.rds"))
      y_test <- data.frame(y = temp)
      assign('y_test', y_test, envir = .GlobalEnv)
    }, TRUE)})
}
