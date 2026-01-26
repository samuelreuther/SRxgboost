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
      temp_OOF <- readRDS(paste0(path_output, gsub(".csv", "/", lauf),
                                 "Data/OOFforecast.rds"))
    } else {
      temp_OOF <- readRDS(paste0(path_output, gsub(".csv", "/", lauf),
                                 "Data/OOFforecast ensemble.rds"))
    }
    OOFforecast <- data.frame(id = temp_OOF[, 1])
    for (i in 1:top_rank) {
      if (i > ncol(temp_OOF) - 1) next()
      i_OOFforecast <-
        colnames(temp_OOF)[gsub("_X.?$", "", colnames(temp_OOF)) == gsub(":", ".", SummaryCV$date[i])]
      if (length(i_OOFforecast) > 0) {
        select <- temp_OOF %>%
          dplyr::select(one_of(i_OOFforecast)) %>%
          # dplyr::select(i_OOFforecast + 1) %>%
          stats::setNames(paste0("m", names(.) %>%
                                   gsub("-", "", .) %>%
                                   gsub(":", "", .) %>%
                                   gsub(".", "", ., fixed = TRUE) %>%
                                   gsub(" ", "x", .)))
        # if "multi:softprob" then determine class
        if (length(i_OOFforecast) > 1) {
          select <- select %>%
            dplyr::mutate(class = apply(select, MARGIN = 1, which.max) - 1)
        }
        OOFforecast <- dplyr::bind_cols(OOFforecast, select)
        # save information of model performance
        SummaryCV_META <- rbind(SummaryCV_META, SummaryCV[i, ] %>% dplyr::mutate(Lauf = lauf))
        rm(select)
      }
    }
    assign('OOFforecast', OOFforecast, envir = .GlobalEnv)
    assign('SummaryCV_META', SummaryCV_META, envir = .GlobalEnv)
    rm(temp_OOF)
  })
  #
  # get TESTforecast
  if (!ensemble) {
    temp_TEST <- readRDS(paste0(path_output, gsub(".csv", "/", lauf),
                                "Data/TESTforecast.rds"))
  } else {
    temp_TEST <- readRDS(paste0(path_output, gsub(".csv", "/", lauf),
                                "Data/TESTforecast ensemble.rds"))
  }
  TESTforecast <- data.frame(id = temp_TEST[, 1])
  for (i in 1:top_rank) {
    if (i > ncol(temp_TEST) - 1) next()
    i_TESTforecast <-
      colnames(temp_TEST)[gsub("_X.?$", "", colnames(temp_TEST)) == gsub(":", ".", SummaryCV$date[i])]
    if (length(i_TESTforecast) > 0) {
      select <- temp_TEST %>%
        dplyr::select(one_of(i_TESTforecast)) %>%
        # dplyr::select(i_TESTforecast + 1) %>%
        stats::setNames(paste0("m", names(.) %>%
                                 gsub("-", "", .) %>%
                                 gsub(":", "", .) %>%
                                 gsub(".", "", ., fixed = TRUE) %>%
                                 gsub(" ", "x", .)))
      # if "multi:softprob" then determine class
      if (length(i_OOFforecast) > 1) {
        select <- select %>%
          dplyr::mutate(class = apply(select, MARGIN = 1, which.max) - 1)
      }
      TESTforecast <- dplyr::bind_cols(TESTforecast, select)
      rm(select)
    }
  }
  assign('TESTforecast', TESTforecast, envir = .GlobalEnv)
  rm(temp_TEST)
  #
  # get y_OOF
  temp_y_OOF <- readRDS(paste0(path_output, gsub(".csv", "/", lauf), "Data/y_OOF.rds"))
  y_OOF <- data.frame(y = temp_y_OOF[, 1])
  assign('y_OOF', y_OOF, envir = .GlobalEnv)
  rm(temp_y_OOF)
  #
  # get y_test
  suppressWarnings({
    try({
      temp_y_TEST <- readRDS(paste0(path_output, gsub(".csv", "/", lauf), "Data/y_test.rds"))
      y_test <- data.frame(y = temp_y_TEST)
      assign('y_test', y_test, envir = .GlobalEnv)
      rm(temp_y_TEST)
    }, TRUE)})
}
