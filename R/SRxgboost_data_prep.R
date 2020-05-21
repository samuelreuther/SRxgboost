#' SRxgboost_data_prep
#'
#' Prepare data for SRxgboost modeling.
#' Creates data for xgboost modeling function SRxgboost_run (y, partitions,
#' train, test).
#' Creates a sparse matrix and dmatrix.
#'
#' @param yname character
#' @param data_train data.frame
#' @param data_test data.frame
#' @param no_folds integer
#' @param folds list, output from xxx
#' @param eval_index vector
#' @param objective character
#' @param label_encoding boolean
#' @param check_covariate_drift boolean
#'
#' @return assigns output into global workspace
#'
#' @examples
#'
#' @export
SRxgboost_data_prep <- function(yname,
                                data_train,
                                data_test = NULL,
                                no_folds = NULL, folds = NULL, eval_index = NULL,
                                objective = NULL,
                                label_encoding = TRUE,
                                check_covariate_drift = TRUE) {
  ### checks
  if (!exists("path_output")) {
    cat("Error: 'path_output' is missing. \n")
    return(invisible(NULL))
  }
  #
  #
  #
  ### convert to data.frame
  data_train <- data.frame(data_train)
  if (!is.null(data_test)) data_test <- data.frame(data_test)
  #
  #
  #
  ### check covariate drift
  path_output_ <- paste0(path_output, gsub(".csv", "", lauf), "/Data Drift/")
  if (!is.null(data_test) & !dir.exists(path_output_) & check_covariate_drift) {
    # create output dir
    dir.create(path_output_, showWarnings = FALSE, recursive = TRUE)
    # calculate_drift
    drift_covariate <- calculate_drift(data_train, data_test, name = path_output_)
  }
  rm(path_output_)
  #
  #
  #
  ### determine if objective is not given: regression, binary or multiclass   TODO multiclass !!!
  if (is.null(objective)) {
    objective <- ifelse(length(data_train[, yname]) == 2, "binary",
                        ifelse(class(data_train[, yname]) == "numeric",
                               "regression", "multiclass"))
  }
  #
  # # create random_noise variable to determine useful features (variable importance)
  # data_train$random_noise <- rnorm(nrow(data_train), mean = 0, sd = 1)
  # if (!is.null(data_test)) data_test$random_noise <- rnorm(nrow(data_test), mean = 0, sd = 1)
  #
  #
  #
  ### remove rows in train, where is.na(y)
  data_train <- data_train[!is.na(data_train[yname]), ]
  # data_train <- data_train %>% filter(!is.na(matches(yname)))
  #
  #
  #
  ### create y
  if (objective == "regression") {
    # "regression"
    y <- data_train[, yname]
    if (!is.null(data_test) & !is.na(match(yname, names(data_test)))) {
      y_test <- data_test[, yname]
    }
  } else if (objective == "binary") {
    # "binary" must be 0 and 1
    # if (sum(is.factor(data_train[, yname]) |
    #         (unique(data_train[, yname])[1] == 1 & unique(data_train[, yname])[2] == 2) |
    #         (unique(data_train[, yname])[1] == 2 & unique(data_train[, yname])[2] == 1)) > 0) {
    #   data_train[, yname] <- as.numeric(data_train[, yname]) - 1
    #   y <- as.numeric(data_train[, yname])
    # } else {
    y <- data_train[, yname]
    # check if y contains only 0/1s
    if (sum(y == 0) + sum(y == 1) != length(y)) {
      stop("XGB_data_prep: y must contain only 0/1s!")
    }
    if (!is.null(data_test) & !is.na(match(yname, names(data_test)))) {
      y_test <- data_test[, yname]
      if (sum(y_test == 0) + sum(y_test == 1) != length(y_test)) {
        stop("XGB_data_prep: y_test must contain only 0/1s!")
      }
    }
    # }
  } else {
    # "multiclass"
    # TODO !!!
  }
  #
  #
  #
  ### create train and test data frames
  datenModell <- data.table::copy(as.data.frame(data_train))
  if (is.null(data_test)) {
    datenModelltest <- data.table::copy(data_train)
  } else {
    datenModelltest <- data.table::copy(as.data.frame(data_test))
  }
  #
  #
  #
  ### data cleaning
  # date
  datenModell <- SRfunctions::SR_feat_eng_date(datenModell,
                                               only_date_to_numeric = FALSE)
  datenModelltest <- SRfunctions::SR_feat_eng_date(datenModelltest,
                                                   only_date_to_numeric = FALSE)
  # factors
  datenModell <- SRfunctions::SR_feat_eng_factors(datenModell,
                                                  label_encoding = FALSE,
                                                  make_na_explicit = TRUE,
                                                  combine_rare_levels = TRUE, prop = 0.005)
  datenModelltest <- SRfunctions::SR_feat_eng_factors(datenModelltest,
                                                      label_encoding = FALSE,
                                                      make_na_explicit = TRUE,
                                                      combine_rare_levels = TRUE,
                                                      use_other_df = datenModell)
  if (label_encoding) {
    datenModell <- SRfunctions::SR_feat_eng_factors(datenModell,
                                                    label_encoding = TRUE,
                                                    order_weekday_month = FALSE,
                                                    make_na_explicit = FALSE,
                                                    combine_rare_levels = FALSE)
    datenModelltest <- SRfunctions::SR_feat_eng_factors(datenModelltest,
                                                        label_encoding = TRUE,
                                                        order_weekday_month = FALSE,
                                                        make_na_explicit = FALSE,
                                                        combine_rare_levels = FALSE,
                                                        use_other_df = datenModell)
  }
  # logical
  datenModell <- datenModell %>%
    dplyr::mutate_if(is.logical, ~dplyr::if_else(., 1, 0))
  datenModelltest <- datenModelltest %>%
    dplyr::mutate_if(is.logical, ~dplyr::if_else(., 1, 0))
  # numeric
  datenModelltest <- SRfunctions::SR_feat_eng_numeric(datenModelltest,
                                                      trim_outliers = TRUE,
                                                      replace_NA_median = TRUE,
                                                      exception = yname,
                                                      use_other_df = datenModell)
  datenModell <- SRfunctions::SR_feat_eng_numeric(datenModell,
                                                  trim_outliers = TRUE,
                                                  replace_NA_median = TRUE,
                                                  exception = yname)
  # datenModell <- SR_feat_eng_numeric(datenModell, replace_NA_special_value = TRUE)
  # datenModelltest <- SR_feat_eng_numeric(datenModelltest, replace_NA_special_value = TRUE)
  # remove columns with unique value
  datenModell <- SRfunctions::SR_remove_column_with_unique_value(datenModell,
                                                                 silent = TRUE)
  suppressWarnings(datenModelltest <- datenModelltest %>%
                     dplyr::select(dplyr::one_of(names(datenModell))))
  # if train has more columns (because of NAs)!
  missings <- setdiff(names(datenModell), names(datenModelltest))               # Find names of missing columns
  if (length(missings) > 0) {
    datenModelltest[missings] <- 0                                              # Add them, filled with '0's
    suppressWarnings(datenModelltest <- datenModelltest %>%
                       dplyr::select(dplyr::one_of(names(datenModell))))
  }
  #
  #
  #
  ### create eval partitions
  if (!is.null(eval_index)) {
    # if exists eval_index
    inTrain <- setdiff(1:nrow(datenModell), eval_index)
  } else if (!is.null(folds)) {
    # if exists folds
    inTrain <- setdiff(1:nrow(datenModell), folds[[1]])
  } else {
    # else CV
    # avoid in binary classification the error that y = 1 is not present in test data
    seed <- 12345
    for (tries in 1:10) {
      set.seed(seed + tries - 1)
      inTrain <- caret::createDataPartition(y = y, p = 1 - 1/no_folds, list = FALSE)
      if (min(y[inTrain]) != max(y[inTrain]) &
          min(y[-inTrain]) != max(y[-inTrain])) {
        set.seed(Sys.time())
        break()
      }
    }; rm(seed, tries)
  }
  assign('eval_index', inTrain, envir = .GlobalEnv)
  train_eval <- datenModell[inTrain, ]
  test_eval <- datenModell[-inTrain, ]
  if (objective == "regression") {
    y_train_eval <- train_eval[, yname]
    y_test_eval <- test_eval[, yname]
  } else if (objective == "binary") {
    if (is.factor(datenModell[, yname])) {
      y_train_eval <- as.numeric(train_eval[, yname]) - 1
      y_test_eval <- as.numeric(test_eval[, yname]) - 1
    } else {
      y_train_eval <- train_eval[, yname]
      y_test_eval <- test_eval[, yname]
    }
    # check if y = 1 exists in test set (problem at unbalanced data)
    if (sum(y_test_eval == 1) == 0 | sum(y_test_eval == 1) == 0 ) {
      stop("30_Model_XGBOOST: y == 1 does not exist in y_train_eval or y_test_eval !")
    }
  } else {
    # "multiclass"
    # TODO !!!
  }
  # rm(inTrain)
  #
  #
  #
  ### remove y from x
  datenModell <- datenModell %>%
    dplyr::select(-dplyr::one_of(yname))
  datenModelltest <- datenModelltest %>%
    dplyr::select(dplyr::one_of(names(datenModell)))
  train_eval <- train_eval %>%
    dplyr::select(dplyr::one_of(names(datenModell)))
  test_eval <- test_eval %>%
    dplyr::select(dplyr::one_of(names(datenModell)))
  #
  #
  #
  ### convert integer variables to numeric (avoid rare error)
  for (var in names(datenModell)) {
    if (class(datenModell[, var]) == "integer") {
      datenModell[, var] <- as.numeric(datenModell[, var])
    }
  }; rm(var)
  for (var in names(datenModelltest)) {
    if (class(datenModelltest[, var]) == "integer") {
      datenModelltest[, var] <- as.numeric(datenModelltest[, var])
    }
  }; rm(var)
  #
  #
  #
  ### check if all variable names and variable classes are identical
  if (!all.equal(names(datenModell), names(datenModelltest))) {
    stop("Names of datenModell and datenModelltest are not equal!")
  }
  if (!all.equal(sapply(datenModell, class), sapply(datenModelltest, class))) {
    stop("Classes of variables of datenModell and datenModelltest are not equal!")
  }
  #
  #
  #
  ### turn train and test into matrices for dummy variables
  options(na.action = 'na.pass')  # global option !!!
  #
  train_mat <- Matrix::sparse.model.matrix(y~. - 1,
                                           data = datenModell)
  d_train <- xgboost::xgb.DMatrix(data = train_mat,
                                  label = y)
  #
  test_mat <- Matrix::sparse.model.matrix(~. - 1,
                                          data = datenModelltest)
  d_test <- xgboost::xgb.DMatrix(data = test_mat)
  #
  train_eval_mat <- cbind(y_train_eval, train_eval)
  train_eval_mat <- Matrix::sparse.model.matrix(y_train_eval~. - 1,
                                                data = train_eval_mat)
  d_train_eval <- xgboost::xgb.DMatrix(data = train_eval_mat,
                                       label = y_train_eval)
  #
  test_eval_mat <- cbind(y_test_eval, test_eval)
  test_eval_mat <- Matrix::sparse.model.matrix(y_test_eval~. - 1,
                                               data = test_eval_mat)
  d_test_eval <- xgboost::xgb.DMatrix(data = test_eval_mat,
                                      label = y_test_eval)
  #
  #
  #
  ### return results to environment
  assign('d_train_eval', d_train_eval, envir = .GlobalEnv)
  assign('d_test_eval', d_test_eval, envir = .GlobalEnv)
  assign('datenModell', datenModell, envir = .GlobalEnv)
  assign('datenModell_train', train_eval, envir = .GlobalEnv)
  assign('datenModell_eval', test_eval, envir = .GlobalEnv)
  assign('datenModelltest', datenModelltest, envir = .GlobalEnv)
  assign('d_train', d_train, envir = .GlobalEnv)
  assign('d_test', d_test, envir = .GlobalEnv)
  assign('y', y, envir = .GlobalEnv)
  try(assign('y_test', y_test, envir = .GlobalEnv), TRUE)
  assign('y_train_eval', y_train_eval, envir = .GlobalEnv)
  assign('y_test_eval', y_test_eval, envir = .GlobalEnv)
  assign('train_mat', train_mat, envir = .GlobalEnv)
  assign('test_mat', test_mat, envir = .GlobalEnv)
  assign('train_eval_mat', train_eval_mat, envir = .GlobalEnv)
  assign('test_eval_mat', test_eval_mat, envir = .GlobalEnv)
  #
  # clean up
  invisible(gc())
  #
  # return results
  return(invisible(NULL))
}
