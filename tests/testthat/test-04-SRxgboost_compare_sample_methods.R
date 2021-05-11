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




# Classification: read data -----------------------------------------------
#
churn <- utils::read.csv(
  paste0(path_to_data, "Classification/Telco Customer Churn/Telco-Customer-Churn.csv"))
assign('churn', churn, envir = .GlobalEnv)
#
#
## prepare data
#
churn <- churn %>%
  # define y
  dplyr::mutate(y = dplyr::case_when(Churn == "No"  ~ 0,
                                     Churn == "Yes" ~ 1,
                                     T              ~ NA_real_), .before = 1) %>%
  # remove unnecesary columns
  SRfunctions::SR_remove_column_with_unique_value(., silent = T) %>%
  # clean up
  dplyr::select(-customerID, -Churn) %>%
  # only numeric, no NA (for "ubOSS", "ubCNN", "ubENN", "ubNCL", "ubTomek")
  # change character to factor (for "ubSMOTE")
  SRfunctions::SR_feat_eng_factors(df = .,
                                   label_encoding = TRUE,
                                   order_weekday_month = FALSE,
                                   make_na_explicit = TRUE,
                                   combine_rare_levels = TRUE, prop = 0.005) %>%
  SRfunctions::SR_feat_eng_numeric(df = .,
                                   trim_outliers = TRUE,
                                   replace_NA_median = TRUE,
                                   exception = "y") %>%
  SRfunctions::SR_feat_eng_date(df = ., only_date_to_numeric = TRUE) %>%
  dplyr::mutate_if(is.logical, ~dplyr::if_else(., 1, 0)) %>%
  data.frame()
#
# remove near zero variables
# nzv <- caret::nearZeroVar(churn, freqCut = 20, saveMetrics = T) %>%
#   tibble::rownames_to_column("variable")
# churn <- churn %>% dplyr::select(nzv$variable[!nzv$nzv])
# rm(nzv)
#
# downsample data to get 5% of y = 1
if (sum(churn$y == 1) / nrow(churn) < 0.05) {
  set.seed(12345)
  churn <- dplyr::bind_rows(churn %>%
                              dplyr::filter(y == 0) %>%
                              dplyr::sample_n(sum(churn$y == 1) * 19),
                            churn %>%
                              dplyr::filter(y == 1) %>%
                              dplyr::sample_n(sum(churn$y == 1) *  1)) %>%
    dplyr::sample_n(nrow(.)) %>%
    data.frame()
  set.seed(Sys.time())
}




# Classification: train ---------------------------------------------------
#
# create folds
no_folds <- 5
train <- churn %>%
  mutate(index = 1:nrow(.)) %>%
  select(index, y, everything())
folds5 <- SRxgboost_create_folds(df = train, foldcolumn = "index", k = no_folds)
train <- train %>%
  select(-index)
assign('train', train, envir = .GlobalEnv)
#
# show stats of y
train %>% count(y) %>% mutate(n_percent = n/sum(n))
dim(train)
#
#
## compare sample methods
#
path_output <- "output_temp/"
assign('path_output', path_output, envir = .GlobalEnv)
comparison <- SRxgboost_compare_sample_methods(df_train = train,
                                               # y_name = "Churn",              # TODO !!!
                                               df_test = NULL,
                                               folds = folds5, runs = 2,
                                               sample_methods = c("ubOver", "ubUnder",
                                                                  "ubSMOTE", "ubOSS",
                                                                  "ubCNN", "ubENN",
                                                                  "ubNCL", "ubTomek"))
#
#
## tests
#
test_that("files in path_output", {
  expect_equal(length(list.files(paste0(path_output, "compare_sample_methods/"))), 11)
})
#
#
## clean up
#
suppressWarnings(rm(folds5, no_folds, comparison, OOFforecast, SummaryCV_META,
                    TESTforecast, y_OOF, id_unique_train, lauf))
unlink(path_output, recursive = TRUE)




# Classification: train / test 5p -----------------------------------------
#
# create training data with y = 1 only 5%
churn$index <- 1:nrow(churn)
set.seed(12345)
train <- bind_rows(churn %>% filter(y == 1) %>% sample_n(200),
                   churn %>% filter(y == 0) %>% sample_n(3800)) %>%
  sample_n(nrow(.))
set.seed(Sys.time())
#
test <- churn %>%
  filter(!index %in% train$index) %>%
  select(-index)
churn <- churn %>% select(-index)
#
# # split train and test   OLD CODE
# set.seed(12345)
# inTrain <- caret::createDataPartition(y = churn$y, p = 0.8, list = FALSE) %>% as.vector()
# set.seed(Sys.time())
# test <- churn[-inTrain, ]
# train <- churn[inTrain, ]
#
# show stats of y
train %>% count(y) %>% mutate(n_percent = n/sum(n))
dim(train)
test %>% count(y) %>% mutate(n_percent = n/sum(n))
dim(test)
#
# create folds
no_folds <- 5
train <- train %>%
  mutate(index = 1:nrow(.), .before = 1)
folds5 <- SRxgboost_create_folds(df = train, foldcolumn = "index", k = no_folds)
train <- train %>% select(-index)
assign('train', train, envir = .GlobalEnv)
assign('test', test, envir = .GlobalEnv)
#
#
## compare sample methods
#
path_output <- "output_temp/"
assign('path_output', path_output, envir = .GlobalEnv)
comparison <- SRxgboost_compare_sample_methods(df_train = train,
                                               # y_name = "Churn",              # TODO !!!
                                               df_test = test,
                                               folds = folds5, runs = 2,
                                               sample_methods = c("ubOver", "ubUnder",
                                                                  "ubSMOTE", "ubENN",
                                                                  "ubNCL", "ubOSS",
                                                                  "ubCNN", "ubTomek"))
#
#
## tests
#
test_that("files in path_output", {
  expect_equal(length(list.files(paste0(path_output, "compare_sample_methods/"))), 12)
})
#
#
## clean up
#
suppressWarnings(rm(folds5, no_folds, comparison, test, inTrain, OOFforecast,
                    SummaryCV_META, TESTforecast, y_OOF, id_unique_train,
                    test_pr, lauf))
file.rename("output_temp/compare_sample_methods/",
            "output_temp/compare_sample_methods_05p/")
#
#
#
# ### scale_pos_weight
# #
# id_unique_train <- 1:nrow(train)
# id_unique_test <- 1:nrow(test)
#
# # data prep
# lauf <- "p10_XGB1_scale_pos_weight9.csv"
# no_folds <- 5
# SRxgboost_data_prep(yname = "y", data_train = train, data_test = test,
#                     no_folds = no_folds, objective = "classification",
#                     check_covariate_drift = FALSE)
#
# # run xgboost
# SRxgboost_run(nround = 10000, eta = 0.05, runs = 100,
#               obj = "binary:logistic", metric = "auc", max_overfit = -1,
#               scale_pos_weight = sum(train$y == 0) / sum(train$y == 1),
#               continue_threshold = 0.1, nfold = no_folds, verbose = 0,
#               test_param = FALSE, run_final_model = TRUE, best_params = NULL)
#
# # plot results of best model
# SRxgboost_plots(lauf = lauf, rank = 1, min_rel_Gain = 0.03)
#
# # make ensemble
# # debugonce("SRxgboost_make_ensemble")
# SRxgboost_make_ensemble(name = gsub(".csv", "_ensemble.csv", lauf),
#                         lauf = lauf, top_rank = 3)
#
# # clean up
# rm(df_temp, train, test, id_unique_train, id_unique_test, OOFforecast, TESTforecast,
#    SummaryCV_META, test_pr, y_OOF)
# SRxgboost_cleanup()




# Classification: train / test 2p -----------------------------------------
# #
# # create training data with y = 1 only 2%
# churn$index <- 1:nrow(churn)
# set.seed(12345)
# train <- bind_rows(churn %>% filter(y == 1) %>% sample_n(50),
#                    churn %>% filter(y == 0) %>% sample_n(2400)) %>%
#   sample_n(nrow(.))
# set.seed(Sys.time())
# #
# test <- churn %>%
#   filter(!index %in% train$index) %>%
#   select(-index)
# churn <- churn %>% select(-index)
# #
# # # split train and test   OLD CODE
# # set.seed(12345)
# # inTrain <- caret::createDataPartition(y = churn$y, p = 0.8, list = FALSE) %>% as.vector()
# # set.seed(Sys.time())
# # test <- churn[-inTrain, ]
# # train <- churn[inTrain, ]
# #
# # show stats of y
# train %>% count(y) %>% mutate(n_percent = n/sum(n))
# dim(train)
# test %>% count(y) %>% mutate(n_percent = n/sum(n))
# dim(test)
# #
# # create folds
# no_folds <- 5
# train <- train %>%
#   mutate(index = 1:nrow(.), .before = 1)
# folds5 <- SRxgboost_create_folds(df = train, foldcolumn = "index", k = no_folds)
# train <- train %>% select(-index)
# assign('train', train, envir = .GlobalEnv)
# assign('test', test, envir = .GlobalEnv)
# #
# #
# ## compare sample methods
# #
# path_output <- "output_temp/"
# assign('path_output', path_output, envir = .GlobalEnv)
# comparison <- SRxgboost_compare_sample_methods(df_train = train,
#                                                # y_name = "Churn",              # TODO !!!
#                                                df_test = test,
#                                                folds = folds5, runs = 2,
#                                                sample_methods = c("ubOver", "ubUnder",
#                                                                   "ubSMOTE", "ubENN",
#                                                                   "ubNCL", "ubOSS",
#                                                                   "ubCNN", "ubTomek"))
# #
# #
# ## tests
# #
# test_that("files in path_output", {
#   expect_equal(length(list.files(paste0(path_output, "compare_sample_methods/"))), 11)
# })
# #
# #
# ## clean up
# #
# suppressWarnings(rm(folds5, no_folds, comparison, test, inTrain, OOFforecast,
#                     SummaryCV_META, TESTforecast, y_OOF, id_unique_train,
#                     test_pr, lauf))
# file.rename("output_temp/compare_sample_methods/",
#             "output_temp/compare_sample_methods_02p/")




# Classification: train / test 10p -----------------------------------------
# #
# # create training data with y = 1 only 10%
# churn$index <- 1:nrow(churn)
# set.seed(12345)
# train <- bind_rows(churn %>% filter(y == 1) %>% sample_n(400),
#                    churn %>% filter(y == 0) %>% sample_n(3600)) %>%
#   sample_n(nrow(.))
# set.seed(Sys.time())
# #
# test <- churn %>%
#   filter(!index %in% train$index) %>%
#   select(-index)
# churn <- churn %>% select(-index)
# #
# # # split train and test   OLD CODE
# # set.seed(12345)
# # inTrain <- caret::createDataPartition(y = churn$y, p = 0.8, list = FALSE) %>% as.vector()
# # set.seed(Sys.time())
# # test <- churn[-inTrain, ]
# # train <- churn[inTrain, ]
# #
# # show stats of y
# train %>% count(y) %>% mutate(n_percent = n/sum(n))
# dim(train)
# test %>% count(y) %>% mutate(n_percent = n/sum(n))
# dim(test)
# #
# # create folds
# no_folds <- 5
# train <- train %>%
#   mutate(index = 1:nrow(.), .before = 1)
# folds5 <- SRxgboost_create_folds(df = train, foldcolumn = "index", k = no_folds)
# train <- train %>% select(-index)
# assign('train', train, envir = .GlobalEnv)
# assign('test', test, envir = .GlobalEnv)
#
#
# ## compare sample methods
# #
# path_output <- "output_temp/"
# assign('path_output', path_output, envir = .GlobalEnv)
# comparison <- SRxgboost_compare_sample_methods(df_train = train,
#                                                # y_name = "Churn",              # TODO !!!
#                                                df_test = test,
#                                                folds = folds5, runs = 2,
#                                                sample_methods = c("ubOver", "ubUnder",
#                                                                   "ubSMOTE", "ubENN",
#                                                                   "ubNCL", "ubOSS",
#                                                                   "ubCNN", "ubTomek"))
# #
# #
# ## tests
# #
# test_that("files in path_output", {
#   expect_equal(length(list.files(paste0(path_output, "compare_sample_methods/"))), 11)
# })
# #
# #
# ## clean up
# #
# suppressWarnings(rm(folds5, no_folds, comparison, test, inTrain, OOFforecast,
#                     SummaryCV_META, TESTforecast, y_OOF, id_unique_train,
#                     test_pr, lauf))
# file.rename("output_temp/compare_sample_methods/",
#             "output_temp/compare_sample_methods_10p/")




# ubRacing NOT RUN --------------------------------------------------------
#
# library(randomForest)
# debugonce("ubRacing")
# results <- ubRacing(y~., train %>% mutate(y = as.factor(y)),
#                     algo = "randomForest", ntree = 500,
#                     nFold = 5, maxFold = 5, positive = 1, metric = "auc",
#                     ubConf = list(type = c("ubOver", "ubUnder", "ubSMOTE", "ubENN",
#                                            "ubNCL", "ubOSS", "ubTomek"), # "ubCNN"
#                                   percOver = 300, percUnder = 150, k = 5,
#                                   perc = 25, method = "percPos", w = NULL))
#
# balanceTypes <- c("unbal", "ubOver", "ubUnder", "ubSMOTE",
#                   "ubOSS", "ubENN", "ubNCL", "ubTomek")
#
# Racing for unbalanced methods selection in 5 fold CV
#  Number of candidates...........................................8
#  Max number of folds in the CV..................................5
#  Max number of experiments....................................100
#  Statistical test...................................Friedman test
#
#                              Markers:
#                                 x No test is performed.
#                                 - The test is performed and
#                                   some candidates are discarded.
#                                 = The test is performed but
#                                   no candidate is discarded.
#
#  +-+-----------+-----------+-----------+-----------+-----------+
#  | |       Fold|      Alive|       Best|  Mean best| Exp so far|
#  +-+-----------+-----------+-----------+-----------+-----------+
#  |x|          1|          8|          4|     0.7886|          8|
#  |=|          2|          8|          3|     0.7964|         16|
#  |=|          3|          8|          3|     0.8036|         24|
#  |=|          4|          8|          3|     0.8009|         32|
#  |=|          5|          8|          3|     0.8058|         40|
#  +-+-----------+-----------+-----------+-----------+-----------+
# Selected candidate: ubUnder 	 metric: auc 	 mean value: 0.8058
# exiting from: ubRacing(y ~ ., train %>% mutate(y = as.factor(y)), algo = "randomForest",
#     ntree = 500, nFold = 5, maxFold = 5, positive = 1, metric = "auc",
#     ubConf = list(type = c("ubOver", "ubUnder", "ubSMOTE", "ubENN",
#         "ubNCL", "ubOSS", "ubTomek"), percOver = 300, percUnder = 150,
#         k = 5, perc = 25, method = "percPos", w = NULL))
#
#
#
# # use Racing to select the best technique for an unbalanced dataset
# library(unbalanced)
# data(ubIonosphere)
# # configure sampling parameters
# ubConf <-
# # load the classification algorithm that you intend to use inside the Race
# # see 'mlr' package for supported algorithms
# library(randomForest)
# # use only 5 trees
# results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, ubConf=ubConf, ntree=5)
# # try with 500 trees
# # results <- ubRacing(Class ~., ubIonosphere, "randomForest", positive=1, ubConf=ubConf, ntree=500)
# # let's try with a different algorithm
# # library(e1071)
# # results <- ubRacing(Class ~., ubIonosphere, "svm", positive=1, ubConf=ubConf)
# # library(rpart)
# # results <- ubRacing(Class ~., ubIonosphere, "rpart", positive=1, ubConf=ubConf)




# Classification: clean up ----------------------------------------------------
#
suppressWarnings(rm(churn, train, factor_encoding))




# Clean up ----------------------------------------------------------------
#
suppressWarnings(rm(path_output, path_to_data, this_file))
unlink(path_output, recursive = TRUE)

