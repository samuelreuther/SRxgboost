# Setup -------------------------------------------------------------------
#
# empty memory and cache
rm(list = ls()); gc()
# load libraries
library(dplyr)
# set paths
path_output <- "output_temp/"
assign('path_output', path_output, envir = .GlobalEnv)
if (dir.exists("D:/Samuel/Analysen in R/Analysen/")) {
  path_to_data <- "D:/Samuel/Analysen in R/Analysen/99 ML Algorithm Benchmark/"
} else if (dir.exists("/data/shared/CH00RHU/")) {
  path_to_data <- "/data/shared/CH00RHU/z-Sonstiges/99 ML Algorithm Benchmark/"
}
assign('path_to_data', path_to_data, envir = .GlobalEnv)
# get path and name of this file
this_file <- list.files()[1]
# try(this_file <- rstudioapi::getActiveDocumentContext()$path, T)
assign('this_file', this_file, envir = .GlobalEnv)




# Regression: read data --------------------------------------------------------------
#
house <- utils::read.csv(paste0(path_to_data,
                                "Regression/Kaggle - house prices/data/train.csv"))
assign('house', house, envir = .GlobalEnv)
id_unique_train <- house$Id
assign('id_unique_train', id_unique_train, envir = .GlobalEnv)
train <- house %>% dplyr::select(-Id)
assign('train', train, envir = .GlobalEnv)




# Regression: no_folds ----------------------------------------------------
#
test_that("regression", {
  skip('skip')
  # initialisation
  lauf <- "regression.csv"
  assign('lauf', lauf, envir = .GlobalEnv)
  cat(lauf, "\n")
  # prepare data and test
  SRxgboost_data_prep(yname = "SalePrice",
                      data_train = train,
                      no_folds = 5,
                      objective = "regression")
  # run models
  SRxgboost_run(nround = 1000, eta = 0.1, obj = "reg:squarederror", metric = "rmse", runs = 50,
                nfold = 5)
  # make ensemble
  SRxgboost_make_ensemble(lauf, top_rank = 3)
  #
  # tests
  # no. of files
  testthat::expect_equal(length(list.files(
    paste0(path_output, gsub(".csv", "", lauf), "/Ensemble/"))), 6)
  #
  # clean up
  suppressWarnings(rm(OOF_metrics, TEST_metrics, OOFforecast, TESTforecast,
                      y_OOF, y_TEST, SummaryCV_META))
  SRxgboost_cleanup()
})




# Regression: train/test --------------------------------------------------
#
test_that("regression train test", {
  skip('skip')
  # initialisation
  lauf <- "regression_train_test.csv"
  assign('lauf', lauf, envir = .GlobalEnv)
  cat(lauf, "\n")
  id_unique_train <- house$Id[seq(1, 1460, 2)]
  id_unique_test <- house$Id[seq(2, 1460, 2)]
  # prepare data and test
  SRxgboost_data_prep(yname = "SalePrice",
                      data_train = train[seq(1, 1460, 2), ],
                      data_test = train[seq(2, 1460, 2), ],
                      no_folds = 5,
                      objective = "regression")
  # run models
  SRxgboost_run(nround = 1000, eta = 0.1, obj = "reg:squarederror", metric = "rmse", runs = 50,
                nfold = 5)
  # make ensemble
  SRxgboost_make_ensemble(lauf, top_rank = 3)
  #
  # tests
  # no. of files
  testthat::expect_equal(length(list.files(
    paste0(path_output, gsub(".csv", "", lauf), "/Ensemble/"))), 10)
  #
  # clean up
  suppressWarnings(rm(OOF_metrics, TEST_metrics, OOFforecast, TESTforecast,
                      y_OOF, y_TEST, SummaryCV_META))
  SRxgboost_cleanup()
})




# Regression: clean up ----------------------------------------------------
#
rm(house, train, id_unique_train, id_unique_test)




# Classification: read data -----------------------------------------------
#
churn <- utils::read.csv(paste0(path_to_data,
                                "Classification/Telco Customer Churn/Telco-Customer-Churn.csv"))
assign('churn', churn, envir = .GlobalEnv)
id_unique_train <- churn$customerID
assign('id_unique_train', id_unique_train, envir = .GlobalEnv)
train <- churn %>%
  dplyr::mutate(Churn = dplyr::case_when(Churn == "No"  ~ 0,
                                         Churn == "Yes" ~ 1,
                                         T              ~ NA_real_)) %>%
  dplyr::select(-customerID)
assign('train', train, envir = .GlobalEnv)




# Classification ----------------------------------------------------------
#
test_that("classification", {
  skip('skip')
  # initialisation
  lauf <- "classification.csv"
  assign('lauf', lauf, envir = .GlobalEnv)
  cat(lauf, "\n")
  # prepare data and test
  SRxgboost_data_prep(yname = "Churn",
                      data_train = train,
                      no_folds = 5,
                      objective = "classification")
  # run models
  SRxgboost_run(nround = 1000, eta = 0.1, obj = "binary:logistic", metric = "auc", runs = 50,
                nfold = 5)
  # make ensemble
  SRxgboost_make_ensemble(lauf, top_rank = 3)
  #
  # tests
  # no. of files
  testthat::expect_equal(length(list.files(
    paste0(path_output, gsub(".csv", "", lauf), "/Ensemble/"))), 12)
  #
  # clean up
  suppressWarnings(rm(OOF_metrics, TEST_metrics, OOFforecast, TESTforecast,
                      y_OOF, y_TEST, SummaryCV_META))
  SRxgboost_cleanup()
})




# Classification: train/test --------------------------------------------------
#
test_that("classification train test", {
  skip('skip')
  # initialisation
  lauf <- "classification_train_test.csv"
  assign('lauf', lauf, envir = .GlobalEnv)
  cat(lauf, "\n")
  id_unique_train <- churn$customerID[seq(1, 7043, 2)]
  id_unique_test <- churn$customerID[seq(2, 7043, 2)]
  # prepare data and test
  SRxgboost_data_prep(yname = "Churn",
                      data_train = train[seq(1, 7043, 2), ],
                      data_test = train[seq(2, 7043, 2), ],
                      no_folds = 5,
                      objective = "classification")
  # run models
  SRxgboost_run(nround = 1000, eta = 0.1, obj = "binary:logistic", metric = "auc", runs = 50,
                nfold = 5)
  # make ensemble
  SRxgboost_make_ensemble(lauf, top_rank = 3)
  #
  # tests
  # no. of files
  testthat::expect_equal(length(list.files(
    paste0(path_output, gsub(".csv", "", lauf), "/Ensemble/"))), 20)
  #
  # clean up
  suppressWarnings(rm(OOF_metrics, TEST_metrics, OOFforecast, TESTforecast,
                      y_OOF, y_TEST, SummaryCV_META))
  SRxgboost_cleanup()
})




# Classification: clean up ----------------------------------------------------
#
rm(churn, train, id_unique_train, id_unique_test)




# Multilabel Classification: read data ------------------------------------
#
# WARNING: mlogloss CRASHES with xgboost v0.90.0.2 !!!
#
birds <- utils::read.csv(paste0(path_to_data,
                                "Multilabel Classification/Birds Bones and Living Habits/data.csv"))
assign('birds', birds, envir = .GlobalEnv)
id_unique_train <- birds$id
assign('id_unique_train', id_unique_train, envir = .GlobalEnv)
train <- birds %>%
  dplyr::mutate(type = as.numeric(type) - 1) %>%
  dplyr::select(-id)
assign('train', train, envir = .GlobalEnv)




# Multilabel Classification -----------------------------------------------
#
test_that("multilabel classification", {
  skip('skip')
  # initialisation
  lauf <- "mclass_softprob.csv"
  assign('lauf', lauf, envir = .GlobalEnv)
  cat(lauf, "\n")
  # prepare data and test
  SRxgboost_data_prep(yname = "type",
                      data_train = train,
                      no_folds = 5,
                      objective = "multilabel")
  # run models
  SRxgboost_run(nround = 1000, eta = 0.1, obj = "multi:softprob", metric = "mAUC", runs = 50,
                nfold = 5)
  # make ensemble
  SRxgboost_make_ensemble(lauf, top_rank = 3)
  #
  # tests
  # no. of files
  testthat::expect_equal(length(list.files(
    paste0(path_output, gsub(".csv", "", lauf), "/Ensemble/"))), 8)
  #
  # clean up
  suppressWarnings(rm(OOF_metrics, TEST_metrics, OOFforecast, TESTforecast,
                      y_OOF, y_TEST, SummaryCV_META))
  SRxgboost_cleanup()
})




# Multilabel Classification: train/test -----------------------------------
#
test_that("multilabel classification train test", {
  skip('skip')
  # initialisation
  lauf <- "mclass_softprob_train_test.csv"
  assign('lauf', lauf, envir = .GlobalEnv)
  cat(lauf, "\n")
  id_unique_train <- birds$id[seq(1, 420, 2)]
  id_unique_test <- birds$id[seq(2, 420, 2)]
  # prepare data and test
  SRxgboost_data_prep(yname = "type",
                      data_train = train[seq(1, 420, 2), ],
                      data_test = train[seq(2, 420, 2), ],
                      no_folds = 5,
                      objective = "multilabel")
  # run models
  SRxgboost_run(nround = 1000, eta = 0.1, obj = "multi:softprob", metric = "mAUC", runs = 50,
                nfold = 5)
  # make ensemble
  SRxgboost_make_ensemble(lauf, top_rank = 3)
  #
  # tests
  # no. of files
  testthat::expect_equal(length(list.files(
    paste0(path_output, gsub(".csv", "", lauf), "/Ensemble/"))), 14)
  #
  # clean up
  suppressWarnings(rm(OOF_metrics, TEST_metrics, OOFforecast, TESTforecast,
                      y_OOF, y_TEST, SummaryCV_META))
  SRxgboost_cleanup()
})




# Multilabel Classification: clean up ----------------------------------------------------
#
rm(birds, train, id_unique_train, id_unique_test)




# Clean up ----------------------------------------------------------------
#
unlink(path_output, recursive = TRUE)
rm(path_output, path_to_data, this_file)

