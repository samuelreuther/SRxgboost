# Setup -------------------------------------------------------------------
#
# empty memory and cache
rm(list = ls(all.names = TRUE)); gc()
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




# Regression: read data ---------------------------------------------------
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
lauf <- "SRxgboost_test_regression_no_folds.csv"
assign('lauf', lauf, envir = .GlobalEnv)
# prepare data and test
test_that("regression / no_folds", {
  expect_equal(class(SRxgboost_data_prep(yname = "SalePrice",
                                         data_train = train,
                                         no_folds = 5,
                                         objective = "regression")),
               "NULL")})
# no. ob objects in memory
test_that("regression / no_folds: no. ob objects in memory", {
  expect_equal(nrow(SRfunctions::SR_memory_usage()), 22)
})
# no_folds
test_that("regression / no_folds: nrow(datenModell_eval) / nrow(datenModell)", {
  expect_equal(round(nrow(datenModell_eval) / nrow(datenModell), 1), 1/5)
})
# clean up
SRxgboost_cleanup()




# Regression: eval_index --------------------------------------------------
#
lauf <- "SRxgboost_test_regression_eval_index.csv"
assign('lauf', lauf, envir = .GlobalEnv)
# create eval_index
eval_index <- which(train$MSSubClass > 90)
assign('eval_index', eval_index, envir = .GlobalEnv)
# prepare data and test
test_that("regression / eval_index", {
  expect_equal(class(SRxgboost_data_prep(yname = "SalePrice",
                                         data_train = train,
                                         eval_index = eval_index,
                                         objective = "regression")),
               "NULL")})
# no. ob objects in memory
test_that("regression / eval_index: no. ob objects in memory", {
  expect_equal(nrow(SRfunctions::SR_memory_usage()), 23)
})
# eval_index
test_that("regression / eval_index: length(id_unique_train[eval_index])", {
  expect_equal(length(id_unique_train[eval_index]), nrow(datenModell_eval))
})
# clean up
SRxgboost_cleanup()




# Regression: folds -------------------------------------------------------
#
lauf <- "SRxgboost_test_regression_folds.csv"
assign('lauf', lauf, envir = .GlobalEnv)
# create folds
train$group <- rep(1:(nrow(train) / 10), each = 10)
folds <- SRxgboost_create_folds(df = train, foldcolumn = "group", k = 5)
assign('folds', folds, envir = .GlobalEnv)
train <- train %>% dplyr::select(-group)
# prepare data and test
test_that("regression / folds", {
  expect_equal(class(SRxgboost_data_prep(yname = "SalePrice",
                                         data_train = train,
                                         folds = folds,
                                         objective = "regression")),
               "NULL")})
# no. ob objects in memory
test_that("regression / folds: no. ob objects in memory", {
  expect_equal(nrow(SRfunctions::SR_memory_usage()), 23)
})
# eval_index
test_that("regression / folds: length(folds[[1]])", {
  expect_equal(length(folds[[1]]), nrow(datenModell_eval))
})
# clean up
SRxgboost_cleanup()




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
  mutate(Churn = case_when(Churn == "No"  ~ 0,
                           Churn == "Yes" ~ 1,
                           T              ~ NA_real_)) %>%
  select(-customerID)
assign('train', train, envir = .GlobalEnv)




# Classification: no_folds ------------------------------------------------
#
lauf <- "SRxgboost_test_classification_no_folds.csv"
assign('lauf', lauf, envir = .GlobalEnv)
# prepare data and test
test_that("classification / no_folds", {
  expect_equal(class(SRxgboost_data_prep(yname = "Churn",
                                         data_train = train,
                                         no_folds = 5,
                                         objective = "binary")),
               "NULL")})
# no. ob objects in memory
test_that("classification / no_folds, no. ob objects in memory", {
  expect_equal(nrow(SRfunctions::SR_memory_usage()), 23)
})
# no_folds
test_that("classification / no_folds: nrow(datenModell_eval) / nrow(datenModell)", {
  expect_equal(round(nrow(datenModell_eval) / nrow(datenModell), 1), 1/5)
})
# clean up
SRxgboost_cleanup()




# Classification: eval_index ----------------------------------------------
#
lauf <- "SRxgboost_test_classification_eval_index.csv"
assign('lauf', lauf, envir = .GlobalEnv)
# create eval_index
eval_index <- which(train$MonthlyCharges > 100)
assign('eval_index', eval_index, envir = .GlobalEnv)
# prepare data and test
test_that("classification / eval_index", {
  expect_equal(class(SRxgboost_data_prep(yname = "Churn",
                                         data_train = train,
                                         eval_index = eval_index,
                                         objective = "binary")),
               "NULL")})
# no. ob objects in memory
test_that("classification / eval_index, no. ob objects in memory", {
  expect_equal(nrow(SRfunctions::SR_memory_usage()), 24)
})
# eval_index
test_that("classification / eval_index: length(id_unique_train[eval_index])", {
  expect_equal(length(id_unique_train[eval_index]), nrow(datenModell_eval))
})
# clean up
SRxgboost_cleanup()




# Classification: folds ---------------------------------------------------
#
lauf <- "SRxgboost_test_classification_folds.csv"
assign('lauf', lauf, envir = .GlobalEnv)
# create folds
train$group <- c(1, 1, 1, rep(1:(nrow(train) / 10), each = 10))
folds <- SRxgboost_create_folds(df = train, foldcolumn = "group", k = 5)
assign('folds', folds, envir = .GlobalEnv)
train <- train %>% dplyr::select(-group)
# prepare data and test
test_that("classification / folds", {
  expect_equal(class(SRxgboost_data_prep(yname = "Churn",
                                         data_train = train,
                                         folds = folds,
                                         objective = "binary")),
               "NULL")})
# no. ob objects in memory
test_that("classification / folds: no. ob objects in memory", {
  expect_equal(nrow(SRfunctions::SR_memory_usage()), 24)
})
# folds
test_that("regression / folds: length(folds[[1]])", {
  expect_equal(length(folds[[1]]), nrow(datenModell_eval))
})
# clean up
SRxgboost_cleanup()




# Classification: clean up ------------------------------------------------
#
rm(churn, train, id_unique_train)




# Clean up ----------------------------------------------------------------
#
rm(path_output, path_to_data)
