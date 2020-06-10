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
# set span for smoothing
smooth_span = 0.5
assign('smooth_span', smooth_span, envir = .GlobalEnv)




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
## run models
#
lauf <- "SRxgboost_test_regression_no_folds.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat(lauf, "\n")
# prepare data and test
SRxgboost_data_prep(yname = "SalePrice",
                    data_train = train,
                    no_folds = 5,
                    objective = "regression")
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "reg:squarederror", metric = "rmse", runs = 2,
              nfold = 5)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, min_rel_Gain = 0.01)
#
#
## tests
#
# no. of files
test_that("regression / no_folds: files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 8)
})
test_that("regression / no_folds: files in path_output/lauf/All Models", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/All Models"))), 10)
})
test_that("regression / no_folds: files in path_output/lauf/Best Model", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
               read.csv2(paste0(path_output, gsub(".csv", "", lauf),
                                "/Best Model/0 Variable importance.csv")) %>%
                 filter(Gain >= 0.01) %>%
                 nrow() * 2 + 14)
})
# test_that("regression / no_folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("regression / no_folds: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 18)
})
# runtime
test_that("regression / no_folds: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 0.2)
})
test_that("regression / no_folds: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 0.1)
})
# rmse
test_that("regression / no_folds: SummaryCV$eval_1fold[1]", {
  expect_true(SummaryCV$eval_1fold[1] < 28000)
  # expect_equal(round(SummaryCV$eval_1fold[1], -3), 27000)
})
test_that("regression / no_folds: SummaryCV$train[1]", {
  expect_true(SummaryCV$train[1] < 3000)
  # expect_equal(round(SummaryCV$train[1], -3), 3000)
})
test_that("regression / no_folds: SummaryCV$test[1]", {
  expect_true(SummaryCV$test[1] < 29000)
  # expect_equal(round(SummaryCV$test[1], -3), 28000)
})
#
#
## clean up
#
SRxgboost_cleanup()




# Regression: eval_index --------------------------------------------------
#
## run models
#
lauf <- "SRxgboost_test_regression_eval_index.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat(lauf, "\n")
# create eval_index
eval_index <- which(train$MSSubClass > 90)
assign('eval_index', eval_index, envir = .GlobalEnv)
# prepare data and test
SRxgboost_data_prep(yname = "SalePrice",
                    data_train = train,
                    eval_index = eval_index,
                    objective = "regression")
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "reg:squarederror", metric = "rmse", runs = 2,
              run_final_model = FALSE)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, min_rel_Gain = 0.01)
#
#
## tests
#
# no. of files
test_that("regression / eval_index: files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 7)
})
test_that("regression / eval_index: files in path_output/lauf/All Models", {
  expect_true(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/All Models"))) %in% c(8, 16))
})
test_that("regression / eval_index: files in path_output/lauf/Best Model", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
               read.csv2(paste0(path_output, gsub(".csv", "", lauf),
                                "/Best Model/0 Variable importance.csv")) %>%
                 filter(Gain >= 0.01) %>%
                 nrow() * 2 + 12)
})
# test_that("regression / eval_index: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 28)
# })
test_that("regression / eval_index: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 19)
})
# runtime
test_that("regression / eval_index: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 0.1)
})
test_that("regression / eval_index: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 0.1)
})
# rmse
test_that("regression / eval_index: SummaryCV$eval_1fold[1]", {
  expect_true(SummaryCV$eval_1fold[1] < 23000)
  # expect_equal(round(SummaryCV$eval_1fold[1], -3), 21000)
})
#
#
## clean up
#
SRxgboost_cleanup()




# Regression: folds -------------------------------------------------------
#
## run models
#
lauf <- "SRxgboost_test_regression_folds.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat(lauf, "\n")
# create folds
train$group <- rep(1:(nrow(train) / 10), each = 10)
folds <- SRxgboost_create_folds(df = train, foldcolumn = "group", k = 5)
assign('folds', folds, envir = .GlobalEnv)
train <- train %>% dplyr::select(-group)
# prepare data and test
SRxgboost_data_prep(yname = "SalePrice",
                    data_train = train,
                    folds = folds,
                    objective = "regression")
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "reg:squarederror", metric = "rmse", runs = 2,
              folds = folds)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, min_rel_Gain = 0.01)
#
#
## tests
#
# no. of files
test_that("regression / folds: files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 8)
})
test_that("regression / folds: files in path_output/lauf/All Models", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/All Models"))), 10)
})
test_that("regression / folds: files in path_output/lauf/Best Model", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
               read.csv2(paste0(path_output, gsub(".csv", "", lauf),
                                "/Best Model/0 Variable importance.csv")) %>%
                 filter(Gain >= 0.01) %>%
                 nrow() * 2 + 14)
})
# test_that("regression / folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("regression / folds: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 19)
})
# runtime
test_that("regression / folds: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 0.2)
})
test_that("regression / folds: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 0.1)
})
# rmse
test_that("regression / folds: SummaryCV$eval_1fold[1]", {
  expect_true(SummaryCV$eval_1fold[1] < 28000)
  # expect_equal(round(SummaryCV$eval_1fold[1], -3), 27000)
})
test_that("regression / folds: SummaryCV$train[1]", {
  expect_true(SummaryCV$train[1] < 5000)
  # expect_equal(round(SummaryCV$train[1], -3), 4000)
})
test_that("regression / folds: SummaryCV$test[1]", {
  expect_true(SummaryCV$test[1] < 30000)
  # expect_equal(round(SummaryCV$test[1], -3), 29000)
})
#
#
## clean up
#
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
## run models
#
lauf <- "SRxgboost_test_classification_no_folds.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat(lauf, "\n")
# prepare data and test
SRxgboost_data_prep(yname = "Churn",
                    data_train = train,
                    no_folds = 5,
                    objective = "binary")
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "binary:logistic", metric = "auc", runs = 2,
              nfold = 5)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, min_rel_Gain = 0.05)
#
#
## tests
#
# no. of files
test_that("classification / no_folds: files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 8)
})
test_that("classification / no_folds: files in path_output/lauf/All Models", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/All Models"))), 10)
})
test_that("classification / no_folds: files in path_output/lauf/Best Model", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
               read.csv2(paste0(path_output, gsub(".csv", "", lauf),
                                "/Best Model/0 Variable importance.csv")) %>%
                 filter(Gain >= 0.05) %>%
                 nrow() * 2 + 20)
})
# test_that("classification / no_folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("classification / no_folds: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 18)
})
# runtime
test_that("classification / no_folds: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 0.2)
})
test_that("classification / no_folds: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 0.1)
})
# auc
test_that("classification / no_folds: SummaryCV$eval_1fold[1]", {
  expect_equal(round(SummaryCV$eval_1fold[1], 2), 0.84)
})
test_that("classification / no_folds: SummaryCV$train[1]", {
  expect_equal(round(SummaryCV$train[1], 2), 0.92)
})
test_that("classification / no_folds: SummaryCV$test[1]", {
  expect_equal(round(SummaryCV$test[1], 2), 0.84)
})
#
#
## clean up
#
SRxgboost_cleanup()




# Classification: eval_index ----------------------------------------------
#
## run models
#
lauf <- "SRxgboost_test_classification_eval_index.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat(lauf, "\n")
# create eval_index
eval_index <- which(train$MonthlyCharges > 100)
assign('eval_index', eval_index, envir = .GlobalEnv)
# prepare data and test
SRxgboost_data_prep(yname = "Churn",
                    data_train = train,
                    eval_index = eval_index,
                    objective = "binary")
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "binary:logistic", metric = "auc", runs = 2,
              run_final_model = FALSE)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, min_rel_Gain = 0.05)
#
#
## tests
#
# no. of files
test_that("classification / eval_index: files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 7)
})
test_that("classification / eval_index: files in path_output/lauf/All Models", {
  expect_true(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/All Models"))) %in% c(8, 16))
})
test_that("classification / eval_index: files in path_output/lauf/Best Model", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
               read.csv2(paste0(path_output, gsub(".csv", "", lauf),
                                "/Best Model/0 Variable importance.csv")) %>%
                 filter(Gain >= 0.05) %>%
                 nrow() * 2 + 18)
})
# test_that("classification / eval_index: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("classification / eval_index: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 19)
})
# runtime
test_that("classification / eval_index: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 0.1)
})
test_that("classification / eval_index: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 0.1)
})
# auc
test_that("classification / eval_index: SummaryCV$eval_1fold[1]", {
  expect_true(SummaryCV$eval_1fold[1] > 0.76)
  # expect_equal(round(SummaryCV$eval_1fold[1], 2), 0.77)
})
#
#
## clean up
#
SRxgboost_cleanup()




# Classification: folds ---------------------------------------------------
#
## run models
#
lauf <- "SRxgboost_test_classification_folds.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat(lauf, "\n")
# create folds
train$group <- c(1, 1, 1, rep(1:(nrow(train) / 10), each = 10))
folds <- SRxgboost_create_folds(df = train, foldcolumn = "group", k = 5)
assign('folds', folds, envir = .GlobalEnv)
train <- train %>% dplyr::select(-group)
# prepare data and test
SRxgboost_data_prep(yname = "Churn",
                    data_train = train,
                    folds = folds,
                    objective = "binary")
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "binary:logistic", metric = "auc", runs = 2,
              folds = folds)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, min_rel_Gain = 0.05)
#
#
## tests
#
# no. of files
test_that("classification / folds: files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 8)
})
test_that("classification / folds: files in path_output/lauf/All Models", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/All Models"))), 10)
})
test_that("classification / folds: files in path_output/lauf/Best Model", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
               read.csv2(paste0(path_output, gsub(".csv", "", lauf),
                                "/Best Model/0 Variable importance.csv")) %>%
                 filter(Gain >= 0.05) %>%
                 nrow() * 2 + 20)
})
# test_that("classification / folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("classification / folds: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 19)
})
# runtime
test_that("classification / folds: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 0.2)
})
test_that("classification / folds: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 0.1)
})
# auc
test_that("classification / folds: SummaryCV$eval_1fold[1]", {
  expect_equal(round(SummaryCV$eval_1fold[1], 2), 0.85)
})
test_that("classification / folds: SummaryCV$train[1]", {
  expect_equal(round(SummaryCV$train[1], 2), 0.90)
})
test_that("classification / folds: SummaryCV$test[1]", {
  expect_equal(round(SummaryCV$test[1], 2), 0.84)
})
#
#
## clean up
#
SRxgboost_cleanup()




# Classification: clean up ----------------------------------------------------
#
rm(churn, train, id_unique_train)




# Clean up ----------------------------------------------------------------
#
unlink(path_output, recursive = TRUE)
rm(path_output, path_to_data, smooth_span, this_file)
