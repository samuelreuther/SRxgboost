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
  path_to_data <- "/home/sandbox/sandbox/z-Sonstige_Analysen/99_ML_Algorithm_Benchmark/"
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
## check sample methods
#
# https://cran.r-project.org/web/packages/unbalanced/unbalanced.pdf
#
#
train_new <- SRxgboost_sample_training_data(df = train, folds = folds5,
                                            sample_method = "ubOver")
test_that("ubOver", {expect_equal(length(train_new), 2)})
rm(train_new)
#
# error:                                                                        # TODO !!!
# Proportion of positives after ubUnder : 25 % of 1392 observations
# Error in ubUnder(X, Y, perc, method, w) :
#   perc >= (N.1/N * 100) is not TRUE
# train_new <- SRxgboost_sample_training_data(df = train, folds = folds5,
#                                             sample_method = "ubUnder")
# test_that("ubUnder", {expect_equal(length(train_new), 2)})
# rm(train_new)
#
train_new <- SRxgboost_sample_training_data(df = train, folds = folds5,
                                            sample_method = "ubUnder", perc = 30)
test_that("ubUnder, perc = 30", {expect_equal(length(train_new), 2)})
rm(train_new)
#
train_new <- SRxgboost_sample_training_data(df = train, folds = folds5,
                                            sample_method = "ubSMOTE")
test_that("ubSMOTE", {expect_equal(length(train_new), 2)})
rm(train_new)
#
train_new <- SRxgboost_sample_training_data(df = train, folds = folds5,
                                            sample_method = "ubSMOTE",
                                            percOver = 200, percUnder = 200, k = 5)
test_that("ubSMOTE, percOver = 200, percUnder = 200", {expect_equal(length(train_new), 2)})
rm(train_new)
#
train_new <- SRxgboost_sample_training_data(df = train, folds = folds5,
                                            sample_method = "ubOSS")
test_that("ubOSS", {expect_equal(length(train_new), 2)})
rm(train_new)
#
train_new <- SRxgboost_sample_training_data(df = train, folds = folds5,
                                            sample_method = "ubCNN")
test_that("ubCNN", {expect_equal(length(train_new), 2)})
rm(train_new)
#
train_new <- SRxgboost_sample_training_data(df = train, folds = folds5,
                                            sample_method = "ubENN")            # = unbalanced?
test_that("ubENN", {expect_equal(length(train_new), 2)})
rm(train_new)
#
train_new <- SRxgboost_sample_training_data(df = train, folds = folds5,
                                            sample_method = "ubNCL")            # = unbalanced?
test_that("ubNCL", {expect_equal(length(train_new), 2)})
rm(train_new)
#
train_new <- SRxgboost_sample_training_data(df = train, folds = folds5,
                                            sample_method = "ubTomek")
test_that("ubTomek", {expect_equal(length(train_new), 2)})
rm(train_new)




# Classification: clean up ----------------------------------------------------
#
suppressWarnings(rm(churn, train, factor_encoding, folds5, no_folds))




# Clean up ----------------------------------------------------------------
#
unlink(path_output, recursive = TRUE)
rm(path_output, path_to_data, this_file)

