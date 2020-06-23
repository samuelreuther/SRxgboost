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
churn <- utils::read.csv(paste0(path_to_data,
                                "Classification/Telco Customer Churn/Telco-Customer-Churn.csv"))
assign('churn', churn, envir = .GlobalEnv)
train <- churn %>%
  dplyr::mutate(y = dplyr::case_when(Churn == "No"  ~ 0,
                                     Churn == "Yes" ~ 1,
                                     T              ~ NA_real_)) %>%
  dplyr::select(-customerID, -Churn)
assign('train', train, envir = .GlobalEnv)




# Classification: train ---------------------------------------------------
#
## prepare data
#
# downsample data to get 5% of y = 1
if (sum(train$y == 1) / nrow(train) < 0.05) {
  set.seed(12345)
  train <- bind_rows(train %>% filter(y == 0) %>% sample_n(sum(train$y == 1) * 19),
                     train %>% filter(y == 1) %>% sample_n(sum(train$y == 1) *  1)) %>%
    sample_n(nrow(.)) %>%
    data.frame()
  set.seed(Sys.time())
}
#
# remove unnecesary columns
train <- train %>% SRfunctions::SR_remove_column_with_unique_value(., silent = T)
#
# remove near zero variables
nzv <- caret::nearZeroVar(train, freqCut = 20, saveMetrics = T) %>%
  tibble::rownames_to_column("variable")
train <- train %>% select(nzv$variable[!nzv$nzv])
#
# show stats of y
train %>% count(y) %>% mutate(n_percent = n/sum(n))
dim(train)
#
# clean up
rm(nzv)
#
# create folds
no_folds <- 5
train <- train %>%
  mutate(index = 1:nrow(.)) %>%
  select(index, y, everything())
folds5 <- SRxgboost_create_folds(df = train, foldcolumn = "index", k = no_folds)
train <- train %>%
  select(-index)
#
#
## compare sample methods
# only numeric features are allowed: ubENN, ubNCL, ubOSS, ubCNN, ubTomek        TODO !!!
#
path_output <- "output_temp/"
comparison <- SRxgboost_compare_sample_methods(df_train = train,
                                               # y_name = "Churn",              TODO !!!
                                               df_test = NULL,
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
  expect_equal(length(list.files(paste0(path_output, "compare sample methods/"))), 5)
})
#
#
## clean up
#
rm(folds5, no_folds, comparison)




# Classification: train / test --------------------------------------------
#
## prepare data
#
# downsample data to get 5% of y = 1
if (sum(train$y == 1) / nrow(train) < 0.05) {
  set.seed(12345)
  train <- bind_rows(train %>% filter(y == 0) %>% sample_n(sum(train$y == 1) * 19),
                     train %>% filter(y == 1) %>% sample_n(sum(train$y == 1) *  1)) %>%
    sample_n(nrow(.)) %>%
    data.frame()
  set.seed(Sys.time())
}
#
# remove unnecesary columns
train <- train %>% SRfunctions::SR_remove_column_with_unique_value(., silent = T)
#
# remove near zero variables
nzv <- caret::nearZeroVar(train, freqCut = 20, saveMetrics = T) %>%
  tibble::rownames_to_column("variable")
train <- train %>% select(nzv$variable[!nzv$nzv])
#
# split train and test
set.seed(12345)
inTrain <- caret::createDataPartition(y = train$y, p = 0.8, list = FALSE) %>% as.vector()
set.seed(Sys.time())
test <- train[-inTrain, ]
train <- train[inTrain, ]
#
# show stats of y
train %>% count(y) %>% mutate(n_percent = n/sum(n))
dim(train)
#
# clean up
rm(nzv)
#
# create folds
no_folds <- 5
train <- train %>%
  mutate(index = 1:nrow(.)) %>%
  select(index, y, everything())
folds5 <- SRxgboost_create_folds(df = train, foldcolumn = "index", k = no_folds)
train <- train %>%
  select(-index)
#
#
## compare sample methods
# only numeric features are allowed: ubENN, ubNCL, ubOSS, ubCNN, ubTomek        TODO !!!
#
path_output <- "output_temp/"
comparison <- SRxgboost_compare_sample_methods(df_train = train,              # TODO !!! remove 31 warnings: Unequal factor levels: coercing to character
                                               # y_name = "Churn",              TODO !!!
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
  expect_equal(length(list.files(paste0(path_output, "compare sample methods/"))), 5)
})
#
#
## clean up
#
rm(folds5, no_folds, comparison, test, inTrain)




# Classification: clean up ----------------------------------------------------
#
rm(churn, train)




# Clean up ----------------------------------------------------------------
#
unlink(path_output, recursive = TRUE)
rm(path_output, path_to_data, this_file)

