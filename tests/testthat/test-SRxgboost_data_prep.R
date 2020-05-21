## setup
path_output <- "output_temp/"
lauf <- "SRxgboost_test"


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
SRxgboost_data_prep(yname = "SalePrice", data_train = house_train, no_folds = 5,
                    objective = "regression")
test_that("regression / no_folds: no ob objects", {
  expect_equal(nrow(SR_memory_usage()),
               22)
})


## regression / eval_index
eval_index <- which(house_train$MSSubClass > 90)
SRxgboost_data_prep(yname = "SalePrice", data_train = house_train, eval_index = eval_index,
                    objective = "regression")
test_that("regression / eval_index: no ob objects", {
  expect_equal(nrow(SR_memory_usage()),
               22)
})


## regression / folds
# create folds
house_train$group <- rep(1:(nrow(house_train) / 10), each = 10)
folds <- SRxgboost_create_folds(df = house_train, foldcolumn = "group", k = 5)
house_train <- house_train %>% dplyr::select(-group)
# prepare data
SRxgboost_data_prep(yname = "SalePrice", data_train = house_train, folds = folds,
                    objective = "regression")
test_that("regression / folds: no ob objects", {
  expect_equal(nrow(SR_memory_usage()),
               22)
})
