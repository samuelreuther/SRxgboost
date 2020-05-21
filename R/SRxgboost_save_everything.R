#' SRxgboost_save_everything
#'
#' Saves all objects for SRxgboost modeling as rds-files.
#'
#' @param lauf character
#'
#' @return saves rds files in lauf-path
#'
#' @examples
#'
#' @export
SRxgboost_save_everything <- function(lauf) {
  ### checks
  if (!exists("path_output")) cat("'path_output' is missing \n")
  #
  # create path
  path_temp <- paste0(path_output, gsub(".csv", "/", lauf), "Data/")
  if (!dir.exists(path_temp)) dir.create(path_temp)
  #
  # save as rds
  if (!file.exists(paste0(path_temp, "train.rds"))) {
    if (exists("train_all")) {
      saveRDS(train_all, paste0(path_temp, "train_all.rds"))
      if (!is.null(train_all$y)) saveRDS(train_all$y,
                                         paste0(path_temp, "y_train_all.rds"))
    }
    saveRDS(train, paste0(path_temp, "train.rds"))
    saveRDS(y, paste0(path_temp, "y_train.rds"))
    if (exists("test")) saveRDS(test, paste0(path_temp, "test.rds"))
    if (exists("y_test")) saveRDS(y_test, paste0(path_temp, "y_test.rds"))
    #
    # saveRDS(d_train_eval, paste0(path_temp, "d_train_eval.rds"))
    # saveRDS(d_test_eval, paste0(path_temp, "d_test_eval.rds"))
    saveRDS(datenModell, paste0(path_temp, "datenModell.rds"))
    saveRDS(datenModell_train, paste0(path_temp, "datenModell_train.rds"))
    saveRDS(datenModell_eval, paste0(path_temp, "datenModell_eval.rds"))
    saveRDS(datenModelltest, paste0(path_temp, "datenModelltest.rds"))
    # saveRDS(d_train, paste0(path_temp, "d_train.rds"))
    # saveRDS(d_test, paste0(path_temp, "d_test.rds"))
    saveRDS(y, paste0(path_temp, "y.rds"))
    saveRDS(y_train_eval, paste0(path_temp, "y_train_eval.rds"))
    saveRDS(y_test_eval, paste0(path_temp, "y_test_eval.rds"))
    saveRDS(train_mat, paste0(path_temp, "train_mat.rds"))
    saveRDS(test_mat, paste0(path_temp, "test_mat.rds"))
    saveRDS(train_eval_mat, paste0(path_temp, "train_eval_mat.rds"))
    saveRDS(test_eval_mat, paste0(path_temp, "test_eval_mat.rds"))
    if (exists("no_folds")) saveRDS(no_folds, paste0(path_temp, "no_folds.rds"))
    if (exists("folds")) saveRDS(folds, paste0(path_temp, "folds.rds"))
    if (exists("eval_index")) saveRDS(eval_index, paste0(path_temp, "eval_index.rds"))
    if (exists("factor_encoding")) saveRDS(factor_encoding,
                                           paste0(path_temp, "factor_encoding.rds"))
    if (exists("id_unique_train")) saveRDS(id_unique_train,
                                           paste0(path_temp, "id_unique_train.rds"))
    if (exists("id_unique_test")) saveRDS(id_unique_test,
                                          paste0(path_temp, "id_unique_test.rds"))
    if (exists("id_unique_train_all")) saveRDS(id_unique_train_all,
                                               paste0(path_temp, "id_unique_train_all.rds"))
  }
  #
  # return results
  return(invisible(NULL))
}
