#' SRxgboost_create_folds
#'
#' Creates custom folds for 'SRxgboost_run(folds = ...)', for example time based,
#' groups, ...
#'
#' @param df data.frame
#' @param foldcolumn character
#' @param k integer
#'
#' @return list
#'
#' @export
SRxgboost_create_folds <- function(df,
                                   foldcolumn,
                                   k = 5) {
  # convert foldcolumn as.factor
  if (class(df[, foldcolumn]) != "factor") {
    df[, foldcolumn] <- as.factor(df[, foldcolumn])
  }
  #
  # create fold index in df
  set.seed(12345)
  df <- groupdata2::fold(df, k = k, id_col = foldcolumn) %>%
    dplyr::select(folds = .folds, dplyr::everything()) %>%
    data.frame()
  set.seed(Sys.time())
  #
  # create list of folds indices for XGBOOST
  fold_ids <- unique(as.numeric(df[, "folds"]))
  folds <- vector("list", dplyr::n_distinct(df[, "folds"]))
  for (fold_id in fold_ids) {
    folds[[fold_id]] <- which(as.numeric(df[, "folds"]) %in% fold_id)
  }
  #
  # return folds
  return(folds)
}
