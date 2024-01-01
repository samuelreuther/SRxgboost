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
#' @param objective character: one of "regression", "classification", "multilabel"
#' @param weights table of weights (multilabel classification with "weighted_precision" only)
#' @param label_encoding boolean
#' @param add_random_variables boolean
#' @param check_covariate_drift boolean
#'
#' @return assigns output into global workspace
#'
#' @export
SRxgboost_data_prep <- function(yname,
                                data_train,
                                data_test = NULL,
                                no_folds = NULL, folds = NULL, eval_index = NULL,
                                nthreads = NULL,
                                objective = NULL,
                                weights = NULL,
                                label_encoding = TRUE,
                                add_random_variables = FALSE,
                                check_covariate_drift = FALSE) {
  ### checks
  # check path_output exists
  if (!exists("path_output")) {
    cat("Error: 'path_output' is missing. \n")
    return(invisible(NULL))
  }
  # check lauf ends with ".csv"
  if (!grepl('.csv$', lauf)) lauf <- paste0(lauf, ".csv")
  #
  ### general options
  if (is.null(nthreads)) nthreads <- parallel::detectCores()
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
    drift_covariate <- SRxgboost_calculate_drift(data_train, data_test,
                                                 name = path_output_)
  }
  rm(path_output_)
  #
  #
  #
  ### determine if objective is not given: regression, binary classification or multilabel classification
  if (is.null(objective)) {
    objective <- dplyr::case_when(length(unique(data_train[, yname])) == 2  ~ "classification",
                                  length(unique(data_train[, yname])) <= 10 ~ "multilabel", # ??? !!!
                                  class(data_train[, yname]) == "numeric"   ~ "regression")
  }
  assign('objective', objective, envir = .GlobalEnv)
  #
  # # create random_noise variable to determine useful features (variable importance)
  # data_train$random_noise <- rnorm(nrow(data_train), mean = 0, sd = 1)
  # if (!is.null(data_test)) data_test$random_noise <- rnorm(nrow(data_test), mean = 0, sd = 1)
  #
  #
  #
  ### remove rows in train, where is.na(y)
  data_train <- data_train[!is.na(data_train[yname]), ]
  # data_train <- data_train %>% dplyr::filter(!is.na(matches(yname)))
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
  } else if (objective == "classification") {
    # "classification" must be 0 and 1
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
  } else if (objective == "multilabel") {
    y <- data_train[, yname]
    # check if y contains only numbers 0-9
    if (sum(unique(y) %in% 0:9) != length(unique(y))) {
      stop("XGB_data_prep: y must contain only numbers from 0 to 9!")
    }
    # check if y contains the number 1
    if (sum(y == 0) == 0) {
      stop("XGB_data_prep: y must include 0 as lowest label!")
    }
    # prepare y_test
    if (!is.null(data_test) & !is.na(match(yname, names(data_test)))) {
      y_test <- data_test[, yname]
      # check if y_test contains only numbers 0-9
      if (sum(unique(y_test) %in% 0:9) != length(unique(y_test))) {
        stop("XGB_data_prep: y_test must contain only numbers from 0 to 9!")
      }
    }
  } else {
    stop("XGB_data_prep: objective must be one of: 'regression', 'classification', 'multilabel'!")
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
  # add randomly generated variables
  if (add_random_variables) {
    set.seed(12345)
    datenModell <- datenModell %>%
      dplyr::mutate(random_numeric = round(stats::runif(nrow(.)), 3),
                    random_binary = ifelse(stats::runif(nrow(.)) >= 0.5, TRUE, FALSE),
                    random_category = sample(letters[1:10], size = nrow(.), replace = TRUE))
    set.seed(246810)
    datenModelltest <- datenModelltest %>%
      dplyr::mutate(random_numeric = round(stats::runif(nrow(.)), 3),
                    random_binary = ifelse(stats::runif(nrow(.)) >= 0.5, TRUE, FALSE),
                    random_category = sample(letters[1:10], size = nrow(.), replace = TRUE))
    set.seed(Sys.time())
  }
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
  missings <- missings[yname != missings]
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
    assign('eval_index', eval_index, envir = .GlobalEnv)
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
      inTrain <- caret::createDataPartition(y = y, p = 1 - 1/no_folds, list = FALSE) %>%
        as.vector()
      if (min(y[inTrain]) != max(y[inTrain]) &
          min(y[-inTrain]) != max(y[-inTrain])) {
        set.seed(Sys.time())
        break()
      }
    }; rm(seed, tries)
  }
  # assign('inTrain', inTrain, envir = .GlobalEnv)
  train_eval <- datenModell[inTrain, ]
  test_eval <- datenModell[-inTrain, ]
  index_train_eval <- id_unique_train[inTrain]
  index_test_eval <- id_unique_train[-inTrain]
  if (objective == "regression") {
    y_train_eval <- train_eval[, yname]
    y_test_eval <- test_eval[, yname]
  } else if (objective == "classification") {
    if (is.factor(datenModell[, yname])) {
      y_train_eval <- as.numeric(train_eval[, yname]) - 1
      y_test_eval <- as.numeric(test_eval[, yname]) - 1
    } else {
      y_train_eval <- train_eval[, yname]
      y_test_eval <- test_eval[, yname]
    }
    # check if y = 1 exists in test set (problem at unbalanced data)
    if (sum(y_test_eval == 1) == 0 | sum(y_test_eval == 1) == 0 ) {
      stop("Error: y == 1 does not exist in y_train_eval or y_test_eval !")
    }
  } else if (objective == "multilabel") {
    if (is.factor(datenModell[, yname])) {
      y_train_eval <- as.numeric(train_eval[, yname]) - 1
      y_test_eval <- as.numeric(test_eval[, yname]) - 1
    } else {
      y_train_eval <- train_eval[, yname]
      y_test_eval <- test_eval[, yname]
    }
    # check if all labels exists in train and test set (problem at unbalanced data)   NECESSARY???
    # if (sum(unique(y_test_eval)[order(unique(y_test_eval))] !=
    #         unique(y_train_eval)[order(unique(y_train_eval))]) != 0) {
    #   stop("Error: labels of y_test_eval are not equal to y_train_eval!")
    # }
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
  options(na.action = 'na.pass')  # global option
  #
  if (is.null(weights)) {
    train_mat <- Matrix::sparse.model.matrix(y~. - 1,
                                             data = datenModell)
    d_train <- xgboost::xgb.DMatrix(data = train_mat,
                                    label = y,
                                    nthread = nthreads)
    #
    test_mat <- Matrix::sparse.model.matrix(~. - 1,
                                            data = datenModelltest)
    d_test <- xgboost::xgb.DMatrix(data = test_mat,
                                   nthread = nthreads)
    #
    train_eval_mat <- cbind(y_train_eval, train_eval)
    train_eval_mat <- Matrix::sparse.model.matrix(y_train_eval~. - 1,
                                                  data = train_eval_mat)
    d_train_eval <- xgboost::xgb.DMatrix(data = train_eval_mat,
                                         label = y_train_eval,
                                         nthread = nthreads)
    #
    test_eval_mat <- cbind(y_test_eval, test_eval)
    test_eval_mat <- Matrix::sparse.model.matrix(y_test_eval~. - 1,
                                                 data = test_eval_mat)
    d_test_eval <- xgboost::xgb.DMatrix(data = test_eval_mat,
                                        label = y_test_eval,
                                        nthread = nthreads)
  } else {
    # add weight
    train_mat <- Matrix::sparse.model.matrix(y~. - 1,
                                             data = datenModell)
    d_train <- xgboost::xgb.DMatrix(data = train_mat,
                                    label = y,
                                    weight = weights[y + 1],
                                    nthread = nthreads)
    #
    test_mat <- Matrix::sparse.model.matrix(~. - 1,
                                            data = datenModelltest)
    d_test <- xgboost::xgb.DMatrix(data = test_mat,
                                   nthread = nthreads)
    #
    train_eval_mat <- cbind(y_train_eval, train_eval)
    train_eval_mat <- Matrix::sparse.model.matrix(y_train_eval~. - 1,
                                                  data = train_eval_mat)
    d_train_eval <- xgboost::xgb.DMatrix(data = train_eval_mat,
                                         label = y_train_eval,
                                         weight = weights[y_train_eval + 1],
                                         nthread = nthreads)
    #
    test_eval_mat <- cbind(y_test_eval, test_eval)
    test_eval_mat <- Matrix::sparse.model.matrix(y_test_eval~. - 1,
                                                 data = test_eval_mat)
    d_test_eval <- xgboost::xgb.DMatrix(data = test_eval_mat,
                                        label = y_test_eval,
                                        weight = weights[y_test_eval + 1],
                                        nthread = nthreads)
  }
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
  assign('index_train_eval', index_train_eval, envir = .GlobalEnv)
  assign('index_test_eval', index_test_eval, envir = .GlobalEnv)
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
