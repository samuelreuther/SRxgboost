# Setup -------------------------------------------------------------------
#
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




# Regression --------------------------------------------------------------
#
## read data
#
house <- utils::read.csv(paste0(path_to_data,
                                "Regression/Kaggle - house prices/data/train.csv"))
assign('house', house, envir = .GlobalEnv)
id_unique_train <- house$Id
assign('id_unique_train', id_unique_train, envir = .GlobalEnv)
train <- house %>% dplyr::select(-Id)
assign('train', train, envir = .GlobalEnv)


## no_folds
#
lauf <- "SRxgboost_test"
assign('lauf', lauf, envir = .GlobalEnv)
# prepare data and test
test_that("regression / no_folds", {
  expect_equal(class(SRxgboost_data_prep(yname = "SalePrice",
                                         data_train = train,
                                         no_folds = 5,
                                         objective = "regression")),
               "NULL")})
test_that("regression / no_folds, no. ob objects", {
  expect_equal(nrow(SRfunctions::SR_memory_usage()), 23)
})
# clean up
SRxgboost_cleanup()


## eval_index
#
lauf <- "SRxgboost_test"
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
test_that("regression / no_folds, no. ob objects", {
  expect_equal(nrow(SRfunctions::SR_memory_usage()), 23)
})
# clean up
SRxgboost_cleanup()


## folds
#
lauf <- "SRxgboost_test"
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
test_that("regression / no_folds, no. ob objects", {
  expect_equal(nrow(SRfunctions::SR_memory_usage()), 24)
})
# clean up
SRxgboost_cleanup()


## clean up
rm(house, train, id_unique_train)



# Classification ----------------------------------------------------------
#
# read data
# train <- utils::read.csv(paste0(path_to_data,
#                                 "Classification/Telco Customer Churn/Telco-Customer-Churn.csv"))
# assign('train', train, envir = .GlobalEnv)



# Clean up ----------------------------------------------------------------
#
rm(path_output, path_to_data)
