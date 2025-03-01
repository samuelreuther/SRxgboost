# Setup -------------------------------------------------------------------
#
# empty memory and cache
rm(list = ls()); gc()
# load libraries
library(dplyr); library(testthat)
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
## run models
#
lauf <- "regr_no_folds.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat("\n", lauf, "\n")
# prepare data and test
SRxgboost_data_prep(yname = "SalePrice", data_train = train,
                    no_folds = 5, nthreads = 1,
                    objective = "regression")
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "reg:squarederror", metric = "rmse", runs = 2,
              nfold = 5 , nthreads = 1)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.01)
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
# test_that("regression / no_folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
#                utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                        "/Best Model/VarImp 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.01) %>%
#                  nrow() * 2 +
#                  utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                          "/Best Model/VarImpInt 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.01) %>%
#                  nrow() * 2 + 14)
# })
# test_that("regression / no_folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("regression / no_folds: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 21)
})
# runtime
test_that("regression / no_folds: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 1)
})
test_that("regression / no_folds: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 1)
})
# rmse
test_that("regression / no_folds: SummaryCV$eval_1fold[1]", {
  expect_true(SummaryCV$eval_1fold[1] < 29000)
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
lauf <- "regr_eval_index.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat("\n", lauf, "\n")
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
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.01)
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
# test_that("regression / eval_index: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
#                utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                        "/Best Model/VarImp 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.01) %>%
#                  nrow() * 2 +
#                  utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                          "/Best Model/VarImpInt 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.01) %>%
#                  nrow() * 2 + 12)
# })
# test_that("regression / eval_index: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 28)
# })
test_that("regression / eval_index: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 22)
})
# runtime
test_that("regression / eval_index: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 1)
})
test_that("regression / eval_index: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 1)
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
lauf <- "regr_folds.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat("\n", lauf, "\n")
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
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.01)
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
# test_that("regression / folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
#                utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                        "/Best Model/VarImp 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.01) %>%
#                  nrow() * 2 +
#                  utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                          "/Best Model/VarImpInt 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.01) %>%
#                  nrow() * 2 + 14)
# })
# test_that("regression / folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("regression / folds: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 22)
})
# runtime
test_that("regression / folds: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 1)
})
test_that("regression / folds: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 1)
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




# Regression: no_folds, sel_vars ------------------------------------------
#
## run model with all variables and 'add_random_variables = TRUE' to determine
## the most important variables
#
lauf <- "regr_no_folds_all.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat("\n", lauf, "\n")
# prepare data and test
SRxgboost_data_prep(yname = "SalePrice",
                    data_train = train,
                    no_folds = 5,
                    objective = "regression",
                    add_random_variables = TRUE)
# run model
SRxgboost_run(nround = 1000, eta = 0.1, obj = "reg:squarederror", metric = "rmse", runs = 2,
              nfold = 5)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.01)
# clean up
SRxgboost_cleanup()


## select relevant variables
#
sel_vars <- SRxgboost_select_variables(lauf_all_variables = "regr_no_folds_all.csv",
                                       threshold_cor = 0.8)


## run final model with selected variables
#
lauf <- "regr_no_folds_sel.csv"
assign('lauf', lauf, envir = .GlobalEnv)
# prepare data and test
SRxgboost_data_prep(yname = "SalePrice",
                    data_train = train %>%
                      select(SalePrice,
                             all_of(sel_vars$Feature[sel_vars$Select])),
                    no_folds = 5,
                    objective = "regression")
# run model
SRxgboost_run(nround = 1000, eta = 0.1, obj = "reg:squarederror", metric = "rmse", runs = 2,
              nfold = 5)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.01)


## tests
#
test_that("regression / sel_vars", {
  expect_equal(class(sel_vars), "data.frame")
})
#
test_that("regression / sel_vars: files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 8)
})


## clean up
#
rm(sel_vars); SRxgboost_cleanup()




# Regression: no_folds, feat_sel ------------------------------------------
#
# ## run models
# #
# # @param feat_sel boolean              Problem beim 1. run!
# # @param Feat_Sel data.frame           ???
# # @param Feat_Sel_Vars list            ???
# # @param Selected_Features boolean     ???
# #
# lauf <- "regr_no_folds_feat_sel.csv"
# assign('lauf', lauf, envir = .GlobalEnv)
# cat("\n", lauf, "\n")
#
#
# ## prepare data and test
# #
# SRxgboost_data_prep(yname = "SalePrice",
#                     data_train = train,
#                     no_folds = 5,
#                     objective = "regression")
#
#
# ## run models to determine most important variables
# #
# SRxgboost_run(nround = 1000, eta = 0.1, obj = "reg:squarederror", metric = "rmse", runs = 2,
#               nfold = 5,
#               feat_sel = TRUE,
#               # Feat_Sel = NULL,
#               # Feat_Sel_Vars = list("MSSubClass", "MSZoning_LabelEnc", "LotFrontage"),
#               Selected_Features = TRUE)
# #
# # XGBOOST_FEAT_SEL(Feat_Sel_Seq = c(seq(1, 0.2, -0.2), 0.1, 0.05, 0.03, 0.02, 0.01, 0.005),
# #                  nround = 1000, eta = 0.3, obj = "reg:linear", metric = "rmse", runs = 1,
# #                  nfold = no_folds, trees = 1, verbose = 0, test_param = T, save_model = F,
# #                  run_kfold = T, save_test_forecast = F)
#
#
# ## plot results of best model
# #
# SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.01)
#
#
# ## clean up
# #
# SRxgboost_cleanup()




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
  dplyr::mutate(Churn = dplyr::case_when(Churn == "No"  ~ 0,
                                         Churn == "Yes" ~ 1,
                                         T              ~ NA_real_)) %>%
  dplyr::select(-customerID)
assign('train', train, envir = .GlobalEnv)




# Classification: no_folds ------------------------------------------------
#
## run models
#
lauf <- "class_no_folds.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat("\n", lauf, "\n")
# prepare data and test
SRxgboost_data_prep(yname = "Churn",
                    data_train = train,
                    no_folds = 5,
                    objective = "classification")
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "binary:logistic", metric = "auc", runs = 2,
              nfold = 5)
# SRxgboost_run(nround = 1000, eta = 0.1, obj = "binary:logistic", metric = "prAUC", runs = 3,
#               nfold = 5)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.05)
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
# test_that("classification / no_folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
#                utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                        "/Best Model/VarImp 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.05) %>%
#                  nrow() * 2 +
#                  utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                          "/Best Model/VarImpInt 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.01) %>%
#                  nrow() * 2 + 2)
# })
# test_that("classification / no_folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("classification / no_folds: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 21)
})
# runtime
test_that("classification / no_folds: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 0.6)
})
test_that("classification / no_folds: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 1)
})
# auc
test_that("classification / no_folds: SummaryCV$eval_1fold[1]", {
  expect_equal(round(SummaryCV$eval_1fold[1], 2), 0.84)
})
test_that("classification / no_folds: SummaryCV$train[1]", {
  expect_equal(round(SummaryCV$train[1], 2), 0.91)
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
lauf <- "class_eval_index.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat("\n", lauf, "\n")
# create eval_index
eval_index <- which(train$MonthlyCharges > 100)
assign('eval_index', eval_index, envir = .GlobalEnv)
# prepare data and test
SRxgboost_data_prep(yname = "Churn",
                    data_train = train,
                    eval_index = eval_index,
                    objective = "classification")
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "binary:logistic", metric = "auc", runs = 2,
              run_final_model = FALSE)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.05)
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
# test_that("classification / eval_index: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
#                utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                        "/Best Model/VarImp 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.05) %>%
#                  nrow() * 2 +
#                  utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                          "/Best Model/VarImpInt 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.01) %>%
#                  nrow() * 2 - 12)
# })
# test_that("classification / eval_index: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("classification / eval_index: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 22)
})
# runtime
test_that("classification / eval_index: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 1)
})
test_that("classification / eval_index: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 1)
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
lauf <- "class_folds.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat("\n", lauf, "\n")
# create folds
train$group <- c(1, 1, 1, rep(1:(nrow(train) / 10), each = 10))
folds <- SRxgboost_create_folds(df = train, foldcolumn = "group", k = 5)
assign('folds', folds, envir = .GlobalEnv)
train <- train %>% dplyr::select(-group)
# prepare data and test
SRxgboost_data_prep(yname = "Churn",
                    data_train = train,
                    folds = folds,
                    objective = "classification")
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "binary:logistic", metric = "auc", runs = 2,
              folds = folds)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.05)
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
# test_that("classification / folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
#                utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                        "/Best Model/VarImp 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.05) %>%
#                  nrow() * 2 +
#                  utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                          "/Best Model/VarImpInt 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.01) %>%
#                  nrow() * 2 + 0)
# })
# test_that("classification / folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("classification / folds: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 22)
})
# runtime
test_that("classification / folds: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 0.3)
})
test_that("classification / folds: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 0.2)
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




# Classification: scale_pos_weight ----------------------------------------
#
## run models
#
lauf <- "class_scale_pos_weight.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat("\n", lauf, "\n")
# prepare data and test
SRxgboost_data_prep(yname = "Churn",
                    data_train = train,
                    no_folds = 5,
                    objective = "classification")
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "binary:logistic", metric = "auc", runs = 2,
              nfold = 5,
              scale_pos_weight = sum(train$Churn == 0) / sum(train$Churn == 1))
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.05)
#
#
## tests
#
# no. of files
test_that("classification / eval_index: files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 8)
})
test_that("classification / eval_index: files in path_output/lauf/All Models", {
  expect_true(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/All Models"))) %in%
                c(10, 20))
})
# test_that("classification / eval_index: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
#                utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                        "/Best Model/VarImp 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.05) %>%
#                  nrow() * 2 +
#                  utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                          "/Best Model/VarImpInt 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.01) %>%
#                  nrow() * 2 - 4)
# })
# test_that("classification / eval_index: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("classification / eval_index: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 21)
})
# runtime
test_that("classification / eval_index: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 1)
})
test_that("classification / eval_index: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 1)
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




# Classification: no_folds, sel_vars --------------------------------------
#
## run model with all variables and 'add_random_variables = TRUE' to determine
## the most important variables
#
lauf <- "class_no_folds_all.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat("\n", lauf, "\n")
# prepare data and test
SRxgboost_data_prep(yname = "Churn",
                    data_train = train,
                    no_folds = 5,
                    objective = "classification",
                    add_random_variables = TRUE)
# run model
SRxgboost_run(nround = 1000, eta = 0.1, obj = "binary:logistic", metric = "auc", runs = 2,
              nfold = 5)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.05)
# clean up
SRxgboost_cleanup()
#
#
## select relevant variables
#
sel_vars <- SRxgboost_select_variables(lauf_all_variables = "class_no_folds_all.csv",
                                       threshold_cor = 0.8)
#
#
## run final model with selected variables
#
lauf <- "class_no_folds_sel.csv"
assign('lauf', lauf, envir = .GlobalEnv)
# prepare data and test
SRxgboost_data_prep(yname = "Churn",
                    data_train = train %>%
                      select(Churn,
                             all_of(sel_vars$Feature[sel_vars$Select])),
                    no_folds = 5,
                    objective = "classification")
# run model
SRxgboost_run(nround = 1000, eta = 0.1, obj = "binary:logistic", metric = "auc", runs = 2,
              nfold = 5)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.05)
#
#
## tests
#
test_that("classification / sel_vars", {
  expect_equal(class(sel_vars), "data.frame")
})
#
test_that("classification / sel_vars: files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 8)
})
#
#
## clean up
#
rm(sel_vars); SRxgboost_cleanup()




# Classification: clean up ------------------------------------------------
#
rm(churn, train, id_unique_train)




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
  dplyr::mutate(type = as.numeric(as.factor(type)) - 1) %>%
  dplyr::select(-id)
assign('train', train, envir = .GlobalEnv)




# Multilabel Classification: no_folds softprob ----------------------------
#
lauf <- "mclass_no_folds_softprob.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat("\n", lauf, "\n")
# prepare data and test
SRxgboost_data_prep(yname = "type",
                    data_train = train,
                    no_folds = 5,
                    objective = "multilabel")
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "multi:softprob", metric = "mAUC", runs = 2,
              nfold = 5)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.05, pdp_parallel = FALSE)
# load OOFforecast and TESTforecast
SRxgboost_get_OOFforecast_TESTforecast(lauf = lauf)
#
#
#
## tests
#
# no. of files
test_that("multilabel classification / no_folds: files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 8)
})
test_that("multilabel classification / no_folds: files in path_output/lauf/All Models", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/All Models"))), 10)
})
# test_that("multilabel classification / no_folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
#                utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                        "/Best Model/VarImp 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.05) %>%
#                  nrow() * 2 + 18)
# })
# test_that("multilabel classification / no_folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("multilabel classification / no_folds: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 21)
})
# runtime
test_that("multilabel classification / no_folds: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 10)
})
test_that("multilabel classification / no_folds: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 10)
})
# mAUC
test_that("multilabel classification / no_folds: SummaryCV$eval_1fold[1]", {
  expect_equal(round(SummaryCV$eval_1fold[1], 2), 0.74)
})
test_that("multilabel classification / no_folds: SummaryCV$train[1]", {
  expect_equal(round(SummaryCV$train[1], 2), 1.00)
})
test_that("multilabel classification / no_folds: SummaryCV$test[1]", {
  expect_equal(round(SummaryCV$test[1], 2), 0.79)
})
#
# no. ob objects in memory: check OOFforecast and TESTforecast
test_that("multilabel classification / no_folds, no. ob objects in memory", {
  expect_equal(nrow(SRfunctions::SR_memory_usage()), 35)
})
#
#
## clean up
#
SRxgboost_cleanup()




# Multilabel Classification: no_folds softprob weighted_precision ---------
#
lauf <- "mclass_no_folds_softprob_wprec.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat("\n", lauf, "\n")
# prepare data and test
weights <-
  (prop.table(table(train$type)) + 0.15) /   # laplace / additive smoothing
  (prop.table(table(train$type[train$huml > 20])) + 0.15)
assign('weights', weights, envir = .GlobalEnv)
SRxgboost_data_prep(yname = "type",
                    data_train = train,
                    no_folds = 5,
                    objective = "multilabel",
                    weights = weights)
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "multi:softprob",
              metric = "weighted_precision", runs = 2,
              nfold = 5)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.05, pdp_parallel = FALSE)
#
#
#
## tests
#
# no. of files
test_that("multilabel classification wprec / no_folds: files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 8)
})
test_that("multilabel classification wprec / no_folds: files in path_output/lauf/All Models", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/All Models"))), 10)
})
# test_that("multilabel classification wprec / no_folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
#                utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                        "/Best Model/VarImp 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.05) %>%
#                  nrow() * 2 + 18)
# })
# test_that("multilabel classification wprec / no_folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("multilabel classification wprec / no_folds: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 21)
})
# runtime
test_that("multilabel classification wprec / no_folds: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 2)
})
test_that("multilabel classification wprec / no_folds: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 1)
})
# weighted_precision
test_that("multilabel classification wprec / no_folds: SummaryCV$eval_1fold[1]", {
  expect_true(round(SummaryCV$eval_1fold[1], 2) >= 0.77)
})
test_that("multilabel classification wprec / no_folds: SummaryCV$train[1]", {
  expect_true(round(SummaryCV$train[1], 2) >= 0.99)
})
test_that("multilabel classification wprec / no_folds: SummaryCV$test[1]", {
  expect_true(round(SummaryCV$test[1], 2) >= 0.79)
})
#
# no. ob objects in memory: check OOFforecast and TESTforecast
test_that("multilabel classification wprec / no_folds, no. ob objects in memory", {
  expect_equal(nrow(SRfunctions::SR_memory_usage()), 36)
})
#
#
## clean up
#
SRxgboost_cleanup(); rm(weights)




# Multilabel Classification: no_folds softmax -----------------------------
#
lauf <- "mclass_no_folds_softmax.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat("\n", lauf, "\n")
# prepare data and test
SRxgboost_data_prep(yname = "type",
                    data_train = train,
                    no_folds = 5,
                    objective = "multilabel")
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "multi:softmax", metric = "mAUC", runs = 2,
              nfold = 5)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.05, pdp_parallel = FALSE)
# load OOFforecast and TESTforecast
SRxgboost_get_OOFforecast_TESTforecast(lauf = lauf)
#
#
#
## tests
#
# no. of files
test_that("multilabel classification / no_folds: files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 8)
})
test_that("multilabel classification / no_folds: files in path_output/lauf/All Models", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/All Models"))), 10)
})
# test_that("multilabel classification / no_folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))),
#                utils::read.csv2(paste0(path_output, gsub(".csv", "", lauf),
#                                        "/Best Model/VarImp 0.csv")) %>%
#                  dplyr::filter(Gain >= 0.05) %>%
#                  nrow() * 2 + 18)
# })
# test_that("multilabel classification / no_folds: files in path_output/lauf/Best Model", {
#   expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 30)
# })
test_that("multilabel classification / no_folds: files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 21)
})
# runtime
test_that("multilabel classification / no_folds: runtime[1]", {
  expect_true(SummaryCV$runtime[1] < 2)
})
test_that("multilabel classification / no_folds: runtime[2]", {
  expect_true(SummaryCV$runtime[2] < 2)
})
# mAUC
test_that("multilabel classification / no_folds: SummaryCV$eval_1fold[1]", {
  expect_equal(round(SummaryCV$eval_1fold[1], 2), 0.74)
})
test_that("multilabel classification / no_folds: SummaryCV$train[1]", {
  expect_equal(round(SummaryCV$train[1], 2), 1)
})
test_that("multilabel classification / no_folds: SummaryCV$test[1]", {
  expect_equal(round(SummaryCV$test[1], 2), 0.79)
})
#
# no. ob objects in memory: check OOFforecast and TESTforecast
test_that("multilabel classification / no_folds, no. ob objects in memory", {
  expect_equal(nrow(SRfunctions::SR_memory_usage()), 35)
})
#
#
## clean up
#
SRxgboost_cleanup()




# Multilabel Classification: no_folds softprob, sel_vars ------------------
#
lauf <- "mclass_no_folds_softprob_selvars.csv"
assign('lauf', lauf, envir = .GlobalEnv)
cat("\n", lauf, "\n")
# prepare data and test
SRxgboost_data_prep(yname = "type",
                    data_train = train,
                    no_folds = 5,
                    objective = "multilabel",
                    add_random_variables = TRUE)
# run models
SRxgboost_run(nround = 1000, eta = 0.1, obj = "multi:softprob", metric = "mAUC", runs = 2,
              nfold = 5)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.05, pdp_parallel = FALSE)
# clean up
SRxgboost_cleanup()
#
#
## select relevant variables
#
sel_vars <- SRxgboost_select_variables(lauf_all_variables = "mclass_no_folds_softprob_selvars.csv",
                                       threshold_cor = 0.9)
#
#
## run final model with selected variables
#
lauf <- "mclass_no_folds_softprob_selvars_final.csv"
assign('lauf', lauf, envir = .GlobalEnv)
# prepare data and test
SRxgboost_data_prep(yname = "type",
                    data_train = train %>%
                      select(type,
                             all_of(sel_vars$Feature[sel_vars$Select])),,
                    no_folds = 5,
                    objective = "multilabel")
# run model
SRxgboost_run(nround = 1000, eta = 0.1, obj = "multi:softprob", metric = "mAUC", runs = 2,
              nfold = 5)
# plot results of best model
SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.05, pdp_parallel = FALSE)
#
#
## tests
#
test_that("multilabel classification / sel_vars", {
  expect_equal(class(sel_vars), "data.frame")
})
#
test_that("multilabel classification / sel_vars: files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 8)
})
#
#
## clean up
#
rm(sel_vars); SRxgboost_cleanup()




# Multilabel Classification: clean up -------------------------------------
#
rm(birds, train, id_unique_train)




# Clean up ----------------------------------------------------------------
#
unlink(path_output, recursive = TRUE)
rm(path_output, path_to_data, this_file)

