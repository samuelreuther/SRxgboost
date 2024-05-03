#' SRxgboost_load_everything
#'
#' Loads all previously prepared objects for SRxgboost modeling into global
#' environment.
#'
#' @param lauf character
#' @param nthreads integer
#'
#' @return loads rds-files into global environment
#'
#' @export
SRxgboost_load_everything <- function(lauf,
                                      nthreads = NULL) {
  ### checks
  # check path_output exists
  if (!exists("path_output")) cat("'path_output' is missing \n")
  # check lauf ends with ".csv"
  if (!grepl('.csv$', lauf)) lauf <- paste0(lauf, ".csv")
  #
  ### general options
  if (is.null(nthreads)) nthreads <- parallel::detectCores()
  #
  # create path_temp for loading data
  path_temp <- paste0(path_output, gsub(".csv", "/", lauf), "Data/")
  #
  ### load rds
  suppressWarnings(try(train_all <- readRDS(paste0(path_temp, "train_all.rds")), TRUE))
  train <- readRDS(paste0(path_temp, "train.rds"))
  y_train <- readRDS(paste0(path_temp, "y_train.rds"))
  suppressWarnings(try(y_train_all <- readRDS(paste0(path_temp, "y_train_all.rds")), TRUE))
  suppressWarnings(try(test <- readRDS(paste0(path_temp, "test.rds")), TRUE))
  suppressWarnings(try(y_test <- readRDS(paste0(path_temp, "y_test.rds")), TRUE))
  #
  # d_train_eval <- readRDS(paste0(path_temp, "d_train_eval.rds"))
  # d_test_eval <- readRDS(paste0(path_temp, "d_test_eval.rds"))
  objective <- readRDS(paste0(path_temp, "objective.rds"))
  datenModell <- readRDS(paste0(path_temp, "datenModell.rds"))
  datenModell_train <- readRDS(paste0(path_temp, "datenModell_train.rds"))
  datenModell_eval <- readRDS(paste0(path_temp, "datenModell_eval.rds"))
  datenModelltest <- readRDS(paste0(path_temp, "datenModelltest.rds"))
  # d_train <- readRDS(paste0(path_temp, "d_train.rds"))
  # d_test <- readRDS(paste0(path_temp, "d_test.rds"))
  y <- readRDS(paste0(path_temp, "y.rds"))
  y_train_eval <- readRDS(paste0(path_temp, "y_train_eval.rds"))
  y_test_eval <- readRDS(paste0(path_temp, "y_test_eval.rds"))
  index_train_eval <- readRDS(paste0(path_temp, "index_train_eval.rds"))
  index_test_eval <- readRDS(paste0(path_temp, "index_test_eval.rds"))
  train_mat <- readRDS(paste0(path_temp, "train_mat.rds"))
  test_mat <- readRDS(paste0(path_temp, "test_mat.rds"))
  train_eval_mat <- readRDS(paste0(path_temp, "train_eval_mat.rds"))
  test_eval_mat <- readRDS(paste0(path_temp, "test_eval_mat.rds"))
  suppressWarnings(try(no_folds <- readRDS(paste0(path_temp, "no_folds.rds")), TRUE))
  suppressWarnings(try(folds <- readRDS(paste0(path_temp, "folds.rds")), TRUE))
  suppressWarnings(try(eval_index <- readRDS(paste0(path_temp, "eval_index.rds")), TRUE))
  suppressWarnings(try(factor_encoding <-
                         readRDS(paste0(path_temp, "factor_encoding.rds")), TRUE))
  suppressWarnings(try(id_unique_train <-
                         readRDS(paste0(path_temp, "id_unique_train.rds")), TRUE))
  suppressWarnings(try(id_unique_test <-
                         readRDS(paste0(path_temp, "id_unique_test.rds")), TRUE))
  suppressWarnings(try(id_unique_train_all <-
                         readRDS(paste0(path_temp, "id_unique_train_all.rds")), TRUE))
  #
  suppressWarnings(try(var_imp <-
                         utils::read.table(paste0(path_temp,
                                                  "Best Model/VarImp 0.csv"),
                                           header = TRUE, sep = ";", dec = ","), TRUE))
  suppressWarnings(try(var_imp <-
                         utils::read.table(paste0(path_temp,
                                                  "Best Model/VarImpInt 0.csv"),
                                           header = TRUE, sep = ";", dec = ","), TRUE))
  #
  # generate DMatrix
  d_train <- xgboost::xgb.DMatrix(data = train_mat, label = y, nthread = nthreads)
  d_test <- xgboost::xgb.DMatrix(data = test_mat, nthread = nthreads)
  d_train_eval <- xgboost::xgb.DMatrix(data = train_eval_mat, label = y_train_eval,
                                       nthread = nthreads)
  d_test_eval <- xgboost::xgb.DMatrix(data = test_eval_mat, label = y_test_eval,
                                      nthread = nthreads)
  #
  #
  ### assign to global environment
  try(assign('train_all', train_all, envir = .GlobalEnv), TRUE)
  assign('train', train, envir = .GlobalEnv)
  assign('y_train', y_train, envir = .GlobalEnv)
  try(assign('y_train_all', y_train_all, envir = .GlobalEnv), TRUE)
  try(assign('test', test, envir = .GlobalEnv), TRUE)
  try(assign('y_test', y_test, envir = .GlobalEnv), TRUE)
  #
  assign('d_train_eval', d_train_eval, envir = .GlobalEnv)
  assign('d_test_eval', d_test_eval, envir = .GlobalEnv)
  assign('objective', objective, envir = .GlobalEnv)
  assign('datenModell', datenModell, envir = .GlobalEnv)
  assign('datenModell_train', datenModell_train, envir = .GlobalEnv)
  assign('datenModell_eval', datenModell_eval, envir = .GlobalEnv)
  assign('datenModelltest', datenModelltest, envir = .GlobalEnv)
  assign('d_train', d_train, envir = .GlobalEnv)
  assign('d_test', d_test, envir = .GlobalEnv)
  assign('y', y, envir = .GlobalEnv)
  assign('y_train_eval', y_train_eval, envir = .GlobalEnv)
  assign('y_test_eval', y_test_eval, envir = .GlobalEnv)
  assign('train_mat', train_mat, envir = .GlobalEnv)
  assign('test_mat', test_mat, envir = .GlobalEnv)
  assign('train_eval_mat', train_eval_mat, envir = .GlobalEnv)
  assign('test_eval_mat', test_eval_mat, envir = .GlobalEnv)
  #
  try(assign('no_folds', no_folds, envir = .GlobalEnv), TRUE)
  try(assign('folds', folds, envir = .GlobalEnv), TRUE)
  try(assign('eval_index', eval_index, envir = .GlobalEnv), TRUE)
  try(assign('factor_encoding', factor_encoding, envir = .GlobalEnv), TRUE)
  try(assign('id_unique_train', id_unique_train, envir = .GlobalEnv), TRUE)
  try(assign('id_unique_test', id_unique_test, envir = .GlobalEnv), TRUE)
  try(assign('id_unique_train_all', id_unique_train_all, envir = .GlobalEnv), TRUE)
  #
  try(assign('var_imp', var_imp, envir = .GlobalEnv), TRUE)
  #
  #
  #
  # return results
  return(invisible(NULL))
}
