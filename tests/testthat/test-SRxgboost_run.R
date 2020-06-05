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
# get path and name of this file
this_file <- list.files()[1]
# try(this_file <- rstudioapi::getActiveDocumentContext()$path, T)
assign('this_file', this_file, envir = .GlobalEnv)
# set span for smoothing
smooth_span = 0.5
assign('smooth_span', smooth_span, envir = .GlobalEnv)




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


### regression / no_folds
#
## run models
#
lauf <- "SRxgboost_test.csv"
assign('lauf', lauf, envir = .GlobalEnv)
# prepare data and test
SRxgboost_data_prep(yname = "SalePrice",
                    data_train = train,
                    no_folds = 5,
                    objective = "regression")
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "reg:squarederror", metric = "rmse", runs = 2,
              nfold = 5)
#
#
## tests
#
# no. ob objects in memory
test_that("regression / no_folds, no. ob objects", {
  expect_equal(nrow(SRfunctions::SR_memory_usage()), 27)
})
# eval_index
test_that("length(id_unique_train[eval_index])", {
  expect_equal(length(id_unique_train[eval_index]), nrow(datenModell_train))
})
# no. of files
test_that("files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 7)
})
test_that("files in path_output/lauf/All Models", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/All Models"))), 10)
})
# test_that("files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 19)
})
# runtime
test_that("runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 0.2)
})
test_that("runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 0.1)
})
# rmse
test_that("SummaryCV$eval_1fold[1]", {
  expect_equal(round(SummaryCV$eval_1fold[1], -3), 27000)
})
test_that("SummaryCV$train[1]", {
  expect_equal(round(SummaryCV$train[1], -3), 3000)
})
test_that("SummaryCV$test[1]", {
  expect_equal(round(SummaryCV$test[1], -3), 28000)
})
#
#
## clean up
#
SRxgboost_cleanup()
unlink(path_output, recursive = TRUE)
rm(house, train, id_unique_train)



# Clean up ----------------------------------------------------------------
#
rm(path_output, path_to_data, smooth_span, this_file)
