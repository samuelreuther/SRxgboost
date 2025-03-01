# Setup -------------------------------------------------------------------
#
# empty memory and cache
rm(list = ls()); gc()
# load libraries
library(dplyr); library(testthat)
# set paths
path_output <- "temp/"
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




# Test --------------------------------------------------------------------
#
# prepare data
set.seed(12345)
df1 <- data.frame(a = stats::runif(1000),
                  b = sample(c("a", "b", "c", "d", "e"), 1000, replace = TRUE),
                  c = sample(seq.Date(from = as.Date("2010-01-01"),
                                      to = as.Date("2020-12-31"),
                                      by = "1 day"), 1000, replace = TRUE))
set.seed(123456)
df2 <- data.frame(a = stats::runif(1000),
                  b = sample(c("a", "b", "c", "d", "e"), 1000, replace = TRUE),
                  c = sample(seq.Date(from = as.Date("2010-01-01"),
                                      to = as.Date("2020-12-31"),
                                      by = "1 day"), 1000, replace = TRUE))
set.seed(Sys.time())
#
# run SRxgboost_covariate_drift_XGB
lauf <- "covariate_drift.csv"
assign('lauf', lauf, envir = .GlobalEnv)
SRxgboost_covariate_drift_XGB(df1 = df1, df2 = df2, pdp_parallel = TRUE)
#
#
## tests
#
# no. of files
test_that("files in path_output/lauf", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/"))), 8)
})
test_that("files in path_output/lauf/All Models", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/All Models"))), 10)
})
test_that("files in path_output/lauf/Best Model", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Best Model"))), 85)
})
test_that("files in path_output/lauf/Data", {
  expect_equal(length(list.files(paste0(path_output, gsub(".csv", "", lauf), "/Data"))), 22)
})




# Clean up ----------------------------------------------------------------
#
SRxgboost_cleanup()
unlink(path_output, recursive = TRUE)
rm(df1, df2, path_output, path_to_data, this_file)

