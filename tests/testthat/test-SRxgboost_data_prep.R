## setup
path_output <- "output_temp/"
assign('path_output', path_output, envir = .GlobalEnv)


## read data
# regression
house <- rio::import(paste0("D:/Samuel/Analysen in R/Analysen/99 ML Algorithm Benchmark/",
                            "Regression/Kaggle - house prices/data/train.csv"))
house_train <- house %>% dplyr::select(-Id)
id_unique_train <- house$Id
# classification
churn <- rio::import(paste0("D:/Samuel/Analysen in R/Analysen/99 ML Algorithm Benchmark/",
                            "Classification/Telco Customer Churn/Telco-Customer-Churn.csv"))


## regression / no_folds
lauf <- "SRxgboost_test"
assign('lauf', lauf, envir = .GlobalEnv)
# prepare data and test
test_that("regression / no_folds", {
  expect_equal(class(SRxgboost_data_prep(yname = "SalePrice",
                                         data_train = house_train,
                                         no_folds = 5,
                                         objective = "regression")),
               "NULL")})
# clean up
SRxgboost_cleanup()


## regression / eval_index
lauf <- "SRxgboost_test"
assign('lauf', lauf, envir = .GlobalEnv)
# create eval_index
eval_index <- which(house_train$MSSubClass > 90)
# prepare data and test
test_that("regression / eval_index", {
  expect_equal(class(SRxgboost_data_prep(yname = "SalePrice",
                                         data_train = house_train,
                                         eval_index = eval_index,
                                         objective = "regression")),
               "NULL")})
# clean up
SRxgboost_cleanup()


## regression / folds
lauf <- "SRxgboost_test"
assign('lauf', lauf, envir = .GlobalEnv)
# create folds
house_train$group <- rep(1:(nrow(house_train) / 10), each = 10)
folds <- SRxgboost_create_folds(df = house_train, foldcolumn = "group", k = 5)
house_train <- house_train %>% dplyr::select(-group)
# prepare data and test
test_that("regression / folds", {
  expect_equal(class(SRxgboost_data_prep(yname = "SalePrice",
                                         data_train = house_train,
                                         folds = folds,
                                         objective = "regression")),
               "NULL")})
# clean up
SRxgboost_cleanup()
