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
} else if (dir.exists("/home/sandbox/sandbox/z-Sonstige_Analysen/")) {
  path_to_data <- "/home/sandbox/sandbox/z-Sonstige_Analysen/99_ML_Algorithm_Benchmark/"
} else if (dir.exists("O:/Group/CC-Analytics/04_Organisatorisches/RHU/")) {
  path_to_data <- "O:/Group/CC-Analytics/04_Organisatorisches/RHU/AnalysenInR/99 ML Algorithm Benchmark/"
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




# Regression ---------------------------------------------------------------
#
test_that("regression", {
  skip('skip')
  # run models
  lauf <- "regression_automl.csv"
  assign('lauf', lauf, envir = .GlobalEnv)
  cat(lauf, "\n")
  SRxgboost_h2o_automl(y = "SalePrice",
                       data_train = train,
                       # data_test = NULL,
                       metric = "RMSE",
                       kfold = 5,
                       max_runtime_sec = 0.5 *60,
                       run_shap = TRUE)
  #
  # tests
  # no. of files
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 9)
  #
  # clean up
  rm(lauf)
})



# Regression: train/test --------------------------------------------------
#
test_that("regression train test", {
  skip('skip')
  # run models
  lauf <- "regression_train_test_automl.csv"
  assign('lauf', lauf, envir = .GlobalEnv)
  cat(lauf, "\n")
  SRxgboost_h2o_automl(y = "SalePrice",
                       data_train = train[1:999, ],
                       data_test = train[1000:1460, ],
                       metric = "RMSE",
                       kfold = 5,
                       max_runtime_sec = 0.5 *60,
                       run_shap = TRUE)
  #
  # tests
  # no. of files
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 12)
  #
  # clean up
  rm(lauf)
})




# Regression: clean up ----------------------------------------------------
#
rm(house, train, id_unique_train)




# Classification: read data -----------------------------------------------
#
churn <- utils::read.csv(paste0(path_to_data,
                                "Classification/Telco Customer Churn/Telco-Customer-Churn.csv"))
assign('churn', churn, envir = .GlobalEnv)
id_unique_train <- churn$customerID
assign('id_unique_train', id_unique_train, envir = .GlobalEnv)
train <- churn %>%
  # dplyr::mutate(Churn = dplyr::case_when(Churn == "No"  ~ 0,                  # must be factor for h2o!
  #                                        Churn == "Yes" ~ 1,
  #                                        T              ~ NA_real_)) %>%
  dplyr::select(-customerID)
assign('train', train, envir = .GlobalEnv)




# Classification ----------------------------------------------------------
#
test_that("classification", {
  skip('skip')
  # run models
  lauf <- "classification_automl.csv"
  assign('lauf', lauf, envir = .GlobalEnv)
  cat(lauf, "\n")
  SRxgboost_h2o_automl(y = "Churn",
                       data_train = train,
                       # data_test = NULL,
                       metric = "AUC",
                       kfold = 5,
                       max_runtime_sec = 0.5 *60,
                       run_shap = FALSE)   # (not supported for "multinomial" as of 2020-06-27)
  #
  # tests
  # no. of files
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 9)
  #
  # clean up
  rm(lauf)
})




# Classification: clean up ----------------------------------------------------
#
rm(churn, train, id_unique_train)




# Multilabel Classification: read data ------------------------------------
#
birds <- utils::read.csv(paste0(path_to_data,
                                "Multilabel Classification/Birds Bones and Living Habits/data.csv"))
assign('birds', birds, envir = .GlobalEnv)
id_unique_train <- birds$id
assign('id_unique_train', id_unique_train, envir = .GlobalEnv)
train <- birds %>%
  # dplyr::mutate(type = as.numeric(type) - 1) %>%                              # must be factor for h2o!
  dplyr::select(-id)
assign('train', train, envir = .GlobalEnv)




# Multilabel Classification -----------------------------------------------
#
test_that("multilabel classification", {
  skip('skip')
  # run models
  lauf <- "multilabel_classification_automl.csv"
  assign('lauf', lauf, envir = .GlobalEnv)
  cat(lauf, "\n")
  SRxgboost_h2o_automl(y = "type",
                       data_train = train,
                       # data_test = NULL,
                       metric = "logloss",
                       kfold = 5,
                       max_runtime_sec = 0.5 *60,
                       run_shap = TRUE)
  #
  # tests
  # no. of files
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 8)
  #
  # clean up
  rm(lauf)
})




# Multilabel Classification: clean up ----------------------------------------------------
#
rm(birds, train, id_unique_train)




# Clean up ----------------------------------------------------------------
#
unlink(path_output, recursive = TRUE)
rm(path_output, path_to_data, this_file)

