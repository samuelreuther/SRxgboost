#' SRxgboost_run
#'
#' Main function to run XGBOOST models (regression, binary classification,
#' multiclass classification)
#' Implemented metrics:
#' "reg:squarederror"  "rmse", "auc", "rmsle", "mae", "mape"
#' "reg:logistic"      "error", "auc"
#' "binary:logistic"   "error", "logloss", "auc", "roc", "qwk_score", "f1_score",
#'                     "mcc_score"
#' "multi:softprob"    "merror", "logloss"
#' "multi:softmax"     "merror", "auc"
#' "rank:pairwise"     "ndcg"
#'
#' @param nround integer
#' @param eta numeric
#' @param obj character
#' @param metric character
#' @param runs integer
#' @param nfold integer
#' @param folds list, output of SRxgboost_create_folds()
#' @param trees integer
#' @param dart numeric
#' @param tree_method character
#' @param verbose integer 0 to 3
#' @param test_param boolean
#' @param shap boolean
#' @param continue_threshold numeric
#' @param run_final_model boolean
#' @param best_params character
#' @param max_overfit numeric between for example -1 (no restriction) or -0.5
#' @param feat_sel boolean
#' @param Feat_Sel data.frame
#' @param Feat_Sel_Vars list
#' @param Selected_Features boolean
#'
#' @return several files in folder
#'
#' @export
SRxgboost_run <- function(nround, eta, obj, metric, runs,
                          nfold = NULL, folds = NULL,
                          trees = 1, dart = 0, tree_method = "auto", verbose = 0,
                          test_param = FALSE, shap = TRUE, continue_threshold = 0.1,
                          run_final_model = TRUE, best_params = NULL, max_overfit = -1,
                          feat_sel = FALSE, Feat_Sel = NULL, Feat_Sel_Vars = NULL,
                          Selected_Features = FALSE) {
  #
  ### Suppress all warnings
  old_warn <- getOption("warn")
  options(warn = -1)
  #
  ### general options
  tree_method <- tree_method   # "exact" "auto"
  #
  #
  #
  ### Read or create output files
  #
  # Check and maybe create output folder
  # ifelse(!dir.exists(paste0(path_output, gsub(".csv", "/", lauf))),
  dir.create(paste0(path_output), showWarnings = FALSE, recursive = TRUE)
  dir.create(paste0(path_output, gsub(".csv", "/", lauf)), FALSE)
  #
  # Create data.frame for results and save as file
  if (!file.exists(paste0(path_output, gsub(".csv", "/", lauf), "Summary.csv")) &
      !feat_sel) {
    #
    # create folder for Data and Models
    dir.create(paste0(path_output, gsub(".csv", "/", lauf), "Data/"))
    dir.create(paste0(path_output, gsub(".csv", "/", lauf), "All Models/"))
    #
    SummaryCV <- data.frame(date = character(0), depth = numeric(0), eta = numeric(0),
                            nround = numeric(0), min_child_weight = numeric(0),
                            gamma = numeric(0), subsample = numeric(0),
                            colsample_bytree = numeric(0), train = numeric(0),
                            test = numeric(0), runtime = numeric(0), eval_1fold = numeric(0),
                            train_sd = numeric(0), test_sd = numeric(0),
                            rate_drop = numeric(0), skip_drop = numeric(0),
                            Feat_Sel = character(0), Feat_Sel_Vars = character(0),
                            metric = character(0))
    utils::write.table(SummaryCV, paste0(path_output, gsub(".csv", "/", lauf), "Summary.csv"),
                       row.names = FALSE, col.names = TRUE, append = FALSE, sep = ";", dec = ",")
    #
    # Create data.frame for OOFforecasts and y and save as file
    # if validation on eval_index OR !run_final_model
    if ((!is.null(nfold) & !is.null(folds)) | !run_final_model) {
      if (exists("id_unique_train")) {
        OOFforecast <- data.frame(ID = id_unique_train[eval_index])
        y_OOF <- data.frame(y = y_test_eval)
      } else {
        OOFforecast <- data.frame(ID = c(1:nrow(datenModell[eval_index, ])))
        y_OOF <- data.frame(y = y_test_eval)
      }
    } else {
      # CV OOF
      if (exists("id_unique_train")) {
        OOFforecast <- data.frame(ID = id_unique_train)
        y_OOF <- data.frame(y)
      } else {
        browser()   # looks like an error?
      }
    }
    saveRDS(OOFforecast,
            paste0(path_output, gsub(".csv", "/", lauf), "Data/OOFforecast.rds"))
    saveRDS(y_OOF, paste0(path_output, gsub(".csv", "/", lauf), "Data/y_OOF.rds"))
    #
    # Create data.frame for TESTforecasts and save as file
    # if (run_final_model) {   # still save forecasts on test data
    if (exists("id_unique_test")) {
      TESTforecast <- data.frame(ID = id_unique_test)
    } else {
      TESTforecast <- data.frame(ID = c(1:nrow(datenModelltest)))
    }
    saveRDS(TESTforecast, paste0(path_output, gsub(".csv", "/", lauf), "Data/TESTforecast.rds"))
    # }
    #
    # make a copy of the actual R script
    file.copy(this_file, paste0(path_output, gsub(".csv", "/", lauf), "R_Script.R"),
              overwrite = TRUE)
    #
    # save everything (data)
    SRxgboost_save_everything(lauf)
  } else {
    # Read in existing run results
    SummaryCV <- utils::read.table(paste0(path_output, gsub(".csv", "/", lauf),
                                          "Summary.csv"),
                                   header = TRUE, sep = ";", dec = ",")
    assign('SummaryCV', SummaryCV, envir = .GlobalEnv)
  }
  #
  #
  #
  ### Loop on 1/kfold or eval_index to check if model parameters are promising,
  # continue best models
  #
  if (feat_sel) runs <- nrow(SummaryCV) + 1
  #
  for (i in (nrow(SummaryCV) + 1):runs) {
    #
    # stop if more than "runs" already done
    if (i > runs) break
    #
    # Read in existing run results
    SummaryCV <- utils::read.table(paste0(path_output, gsub(".csv", "/", lauf),
                                          "Summary.csv"),
                                   header = TRUE, sep = ";", dec = ",")
    assign('SummaryCV', SummaryCV, envir = .GlobalEnv)
    #
    #
    #
    ### Define modell parameters
    nround_test <- ifelse(test_param, min(3000, nround), nround)
    eta_test <- ifelse(test_param, 0.5, eta)
    if (i == 1 & nrow(SummaryCV) == 0) {
      # Run 1 with standard-modell parameters
      depth <- 6                                         # (Default: 6)
      min_child_weight <- 1                              # (Default: 1)
      gamma <- 0                                         # (Default: 0)
      subsample <- 1                                     # (Default: 1)
      colsample_bytree <- 1                              # (Default: 1)
      booster <- ifelse(dart < 1, "gbtree", "dart")      # (Default: "gbtree")
      rate_drop <- ifelse(booster == "gbtree", 0, 0.1)   # (Default: 0)
      skip_drop <- ifelse(booster == "gbtree", 0, 0.5)   # (Default: 0)
    } else if ((i == 2 & !is.null(best_params)) | feat_sel) {
      if (!feat_sel) {
        bp <- utils::read.table(paste0(path_output, gsub(".csv", "/", best_params),
                                "Summary.csv"), header = TRUE, sep = ";", dec = ",")
        if (metric %in% c("auc", "qwk_score", "f1_score", "mcc_score")) {
          bp <- bp %>% dplyr::arrange(dplyr::desc(test), dplyr::desc(eval_1fold))
        } else {
          bp <- bp %>% dplyr::arrange(test, eval_1fold)
        }
      } else {
        bp <- SummaryCV
      }
      depth <- bp$depth[1]
      min_child_weight <- bp$min_child_weight[1]
      gamma <- bp$gamma[1]
      subsample <- bp$subsample[1]
      colsample_bytree <- bp$colsample_bytree[1]
      booster <- ifelse(bp$rate_drop[1] == 0, "gbtree", "dart")
      rate_drop <- bp$rate_drop[1]
      skip_drop <- bp$skip_drop[1]
    } else {
      # Random Search for Modell-Parameter
      set.seed(Sys.time())
      depth <- round(stats::runif(1, 2, 15), 0)
      min_child_weight <- round(stats::rexp(1, rate = 0.1), 0)  # 0.1 => 20  0.2 => 12  0.3 => 10  0.7 => 4
      gamma <- round(stats::rexp(1, rate = 0.5), 1)             # 0.3 => 7   0.5 => 5   0.7 => 3   1.0 => 2.5
      subsample <- sample(c(seq(0.2, 0.45, 0.05),
                            seq(0.5, 0.68, 0.02),
                            seq(0.7, 0.99, 0.01),
                            rep(1, 3)), 1)
      colsample_bytree <- round(stats::runif(1, 0.1, 1), 2)
      booster <- sample(c("gbtree", "dart"), 1, prob = c(1 - dart, dart))
      rate_drop <- ifelse(booster == "dart", sample(c(0.02, 0.05, 0.1, 0.2), 1), 0)
      skip_drop <- ifelse(booster == "dart", sample(c(0.5), 1), 0)   # 0.7, 0.5, 0.3
    }
    # Exponential distribution:
    # rate = 0.01; n = 1000; hist(stats::rexp(n, rate = rate), 20)
    # summary(stats::rexp(n, rate = rate)); rm(rate, n)
    #
    # Initialize temp
    if (exists("temp")) rm(temp)
    temp <- vector(length = 6)
    temp[1] <- depth
    temp[2] <- eta_test
    temp[3] <- min_child_weight
    temp[4] <- gamma
    temp[5] <- subsample
    temp[6] <- colsample_bytree
    temp[7] <- rate_drop
    temp[8] <- skip_drop
    #
    # Goto next, if setup already exists (if TRUE go to next i)
    if (length(which(duplicated(rbind(SummaryCV[, c(2,5:8,15:16)], temp[c(1,3:8)])))) != 0 &
        !feat_sel) next
    #
    # Clean cache and start measuring time for run(i)
    invisible(gc())
    start <- Sys.time()
    #
    # Print and save parameters for run(i)
    if (exists("temp")) rm(temp)
    temp <- data.frame(matrix(nrow = 1, ncol = 12, byrow = TRUE))
    print(paste0(i, " ", start,
                 " e=", ifelse(test_param, paste0(eta_test, "/", eta), eta),
                 " n=", ifelse(test_param, paste0(nround_test, "/", nround), nround),
                 " d=", depth, " mcw=", min_child_weight, " g=", gamma,
                 " s=", subsample, " c=", colsample_bytree,
                 dplyr::if_else(booster == "dart",
                                paste0(" r_dr=", rate_drop, " s_dr=", skip_drop), "")))
    temp[1] <- as.character(start)
    temp[2] <- depth
    temp[3] <- eta_test
    temp[4] <- nround_test
    temp[5] <- min_child_weight
    temp[6] <- gamma
    temp[7] <- subsample
    temp[8] <- colsample_bytree
    temp[c(9:18)] <- NA
    temp[15] <- rate_drop
    temp[16] <- skip_drop
    #
    # Custom metrics
    custom_metrics <- c("rmsle", "mape", "mae", "qwk_score", "f1_score", "mcc_score")
    # Maximise metric
    metrics_maximize <- ifelse(metric %in% c("auc", "qwk_score", "f1_score",
                                             "mcc_score"), TRUE, FALSE)
    # functions to be used by XGBOOST during optimization
    rmsle <- function(preds, d_train) {
      labels <- xgboost::getinfo(d_train, "label")
      preds[preds < -1] <- -1               # avoids error of log(x < -1) = -Inf
      err <- Metrics::rmsle(labels, preds)
      return(list(metric = "rmsle", value = err))
    }
    mape <- function(preds, d_train) {
      labels <- xgboost::getinfo(d_train, "label")
      err <- sum(abs(preds / labels - 1)) / length(labels)
      return(list(metric = "mape", value = err))
    }
    mae <- function(preds, d_train) {
      labels <- xgboost::getinfo(d_train, "label")
      err <- Metrics::mae(labels, preds)
      return(list(metric = "mae", value = err))
    }
    qwk_score <- function(preds, d_train) {
      labels <- xgboost::getinfo(d_train, "label")
      preds <- round(preds, 0)
      err <- Metrics::ScoreQuadraticWeightedKappa(
        labels, preds, min.rating = min(y), max.rating = max(y))
      return(list(metric = "qwk", value = err))
    }
    f1_score <- function(pred, d_train) {
      labels <- xgboost::getinfo(d_train, "label")
      pred <- ROCR::prediction(pred, labels)
      f <- ROCR::performance(pred, "f")
      err <- max(f@y.values[[1]], na.rm = TRUE)
      opt_cutoff = f@x.values[[1]][which.max(f@y.values[[1]])]
      return(list(metric = "f1", value = err, opt_cutoff = opt_cutoff))
    }
    mcc_score <- function(pred, d_train) {
      labels <- xgboost::getinfo(d_train, "label")
      pred <- ROCR::prediction(pred, labels)
      mcc <- ROCR::performance(pred, "mat")
      err <- max(mcc@y.values[[1]], na.rm = TRUE)
      opt_cutoff = mcc@x.values[[1]][which.max(mcc@y.values[[1]])]
      return(list(metric = "mcc", value = err, opt_cutoff = opt_cutoff))
    }
    #
    #
    #
    ### Feat_Sel_Vars_best
    if (Selected_Features) {
      train_eval_mat <- train_eval_mat
      train_eval_mat <- train_eval_mat[, Feat_Sel_Vars_best]
      d_train_eval <- xgboost::xgb.DMatrix(data = train_eval_mat, label = y_train_eval)
      #
      test_eval_mat <- test_eval_mat
      test_eval_mat <- test_eval_mat[, Feat_Sel_Vars_best]
      d_test_eval <- xgboost::xgb.DMatrix(data = test_eval_mat, label = y_test_eval)
      #
      train_mat <- train_mat
      train_mat <- train_mat[, Feat_Sel_Vars_best]
      d_train <- xgboost::xgb.DMatrix(data = train_mat, label = y)
      #
      test_mat <- test_mat
      test_mat <- test_mat[, Feat_Sel_Vars_best]
      d_test <- xgboost::xgb.DMatrix(data = test_mat)
    }
    #
    #
    #
    ### Test model parameter on fraction of data (1/kfold or eval_index)
    if (exists("bst")) rm(bst)
    set.seed(12345)
    try(rm(params), TRUE)
    if (obj %in% c("multi:softmax", "multi:softprob")) {
      params <- list(num_class = length(unique(y)))
    } else {
      params <- list()
    }
    if (!metric %in% custom_metrics) {
      bst <- xgboost::xgb.train(objective = obj, params = params,
                                eval_metric = metric, maximize = metrics_maximize,
                                eta = eta_test, max_depth = depth,
                                min_child_weight = min_child_weight, gamma = gamma,
                                subsample = subsample, colsample_bytree = colsample_bytree,
                                num_parallel_tree = trees,
                                tree_method = tree_method, booster = booster,
                                rate_drop = rate_drop, skip_drop = skip_drop,
                                early_stopping_rounds = min(max(0.05 * nround_test, 5), 30),
                                missing = NA, data = d_train_eval, nround = nround_test,
                                verbose = verbose,
                                print_every_n = ifelse(verbose == 0, nround, nround_test / 50),
                                watchlist = list(train = d_train_eval, eval = d_test_eval),
                                callbacks = list(xgboost::cb.evaluation.log()))
    } else {
      bst <- xgboost::xgb.train(objective = obj, params = params,
                                feval = get(metric), maximize = metrics_maximize ,
                                eta = eta_test, max_depth = depth,
                                min_child_weight = min_child_weight, gamma = gamma,
                                subsample = subsample, colsample_bytree = colsample_bytree,
                                num_parallel_tree = trees,
                                tree_method = tree_method, booster = booster,
                                rate_drop = rate_drop, skip_drop = skip_drop,
                                early_stopping_rounds = min(max(0.05 * nround_test, 5), 30),
                                missing = NA, data = d_train_eval, nround = nround_test,
                                verbose = verbose,
                                print_every_n = ifelse(verbose == 0, nround, nround_test / 50),
                                watchlist = list(train = d_train_eval, eval = d_test_eval),
                                callbacks = list(xgboost::cb.evaluation.log()))
    }
    #
    # get evaluation log
    evaluation_log <- data.frame(bst$evaluation_log)
    if (max_overfit > 0) max_overfit <- -max_overfit
    if (metrics_maximize) {
      evaluation_log$overfit <- (1 - evaluation_log[, 2]) / (1 - evaluation_log[, 3]) - 1
    } else {
      evaluation_log$overfit <- evaluation_log[, 2] / evaluation_log[, 3] - 1
    }
    evaluation_log <- evaluation_log %>%
      dplyr::mutate(valid = overfit >= max_overfit)
    evaluation_log_ <- evaluation_log %>%
      dplyr::filter(valid == TRUE)
    if (nrow(evaluation_log_) == 0) evaluation_log_ <- evaluation_log[1, ]
    temp[4] <- ifelse(metrics_maximize == TRUE,
                      evaluation_log_$iter[which.max(evaluation_log_[, 3])],
                      evaluation_log_$iter[which.min(evaluation_log_[, 3])])
    eval_1fold <- temp[12] <- evaluation_log[temp[[4]], 3]
    cat(paste0("   Best score eval: ", round(temp[12], 4), " at nrounds: ", temp[4], "\n"))
    #
    # determine "good" model
    threshold <- continue_threshold # 0.1
    wait_no_models <- dplyr::if_else(is.null(nfold) & is.null(folds), 1, 10)   # if no CV: 1 else 10
    condition_best <-
      ifelse(metrics_maximize == TRUE,
             ifelse(nrow(SummaryCV) < wait_no_models,
                    FALSE,
                    eval_1fold > stats::quantile(SummaryCV$eval_1fold, 1 - threshold)[[1]]),
             ifelse(nrow(SummaryCV) < wait_no_models,
                    FALSE,
                    eval_1fold < stats::quantile(SummaryCV$eval_1fold, threshold)[[1]]))
    #
    # save model, error rate, TESTforecast, OOFforecast, shapley if this is a "good" model
    if (((i == 1 & nrow(SummaryCV) == 0) |
         condition_best |
         (i == 2 & !is.null(best_params))) |
        (feat_sel)) {
      # save model
      xgboost::xgb.save(bst, paste0(path_output, gsub(".csv", "/", lauf), "All Models/",
                                    gsub(":", ".", as.character(start)), "_1fold.model"))
      saveRDS(bst, paste0(path_output, gsub(".csv", "/", lauf), "All Models/",
                          gsub(":", ".", as.character(start)), "_1fold.model.rds"))
      #
      # save error rate, TESTforecast and OOFforecast if this is the final model
      if (!run_final_model) {
        # get error rate => save below
        if (metrics_maximize) {
          loss <- (1 - evaluation_log[bst$best_iteration, 3]) / (1 - as.numeric(temp[10])) - 1
        } else {
          loss <- evaluation_log[bst$best_iteration, 3] / as.numeric(temp[12]) - 1
        }
        error_rate <- evaluation_log %>%
          dplyr::select(iter, 2, 3)
        #
        # Save TESTforecast
        try({
          TESTforecast <- readRDS(paste0(path_output, gsub(".csv", "/", lauf),
                                         "Data/TESTforecast.rds"))
          TESTforecast <- cbind(TESTforecast, stats::predict(bst, d_test, missing = NA))  # test_mat
          data.table::setnames(TESTforecast, names(TESTforecast)[ncol(TESTforecast)],
                               gsub(":", ".", as.character(temp[1])))
          saveRDS(TESTforecast, paste0(path_output, gsub(".csv", "/", lauf),
                                       "Data/TESTforecast.rds"))
          rm(TESTforecast)
        })
        #
        # Save OOFforecast (this is only OOF on 1/kfold or eval_index)
        try({
          OOFforecast <- readRDS(paste0(path_output, gsub(".csv", "/", lauf),
                                        "Data/OOFforecast.rds"))
          OOFforecast <- cbind(OOFforecast, stats::predict(bst, d_test_eval, missing = NA))
          # OOFforecast <- cbind(OOFforecast, stats::predict(bst, d_train[eval_index], missing = NA))
          data.table::setnames(OOFforecast, names(OOFforecast)[ncol(OOFforecast)],
                               gsub(":", ".", as.character(temp[1])))
          saveRDS(OOFforecast, paste0(path_output, gsub(".csv", "/", lauf),
                                      "Data/OOFforecast.rds"))
          rm(OOFforecast)
        })
      }
      #
      #
      # save Shapley values
      if (shap) {
        shapley <- stats::predict(bst, d_test_eval, missing = NA, predcontrib = TRUE)
        saveRDS(shapley, paste0(path_output, gsub(".csv", "/", lauf), "All Models/",
                                gsub(":", ".", as.character(start)), "_Shap_train_eval.rds"))
        # xgb.plot.shap(test_eval_mat, shap_contrib = shapley, features= "FSAlter",
        #               span_loess = 0.1)
        # xgb.plot.shap(test_eval_mat, shap_contrib = shapley, model = bst, top_n = 3,
        #               span_loess = 0.3)
        #
        # get forecast values
        OOFforecast <- stats::predict(bst, d_test_eval, missing = NA)
        saveRDS(OOFforecast, paste0(path_output, gsub(".csv", "/", lauf), "All Models/",
                                    gsub(":", ".", as.character(start)),
                                    "_Shap_prediction.rds"))
        #
        # check if rowSums of shapley == model prediction   ok
        # OOFforecast[1:5]
        # rowSums(shapley[1:5, 1:(ncol(shapley))])
        #
        # clean up data
        temp_data <- datenModell_eval
        for (column in names(temp_data)) {
          # decode LabelEnc
          if (gsub("_LabelEnc", "", column) %in% factor_encoding$feature) {
            temp_data[, column] <- factor(temp_data[, column])
            levels(temp_data[, column]) <- factor_encoding$levels[factor_encoding$feature ==
                                                                    gsub("_LabelEnc", "", column)]
          }
          # decode Date
          if (sum(is.na(as.Date(as.character(temp_data[, column]), format = "%Y%m%d"))) == 0) {
            temp_data[, column] <- as.Date(as.character(temp_data[, column]), format = "%Y%m%d")
          }
        }; rm(column)
        saveRDS(temp_data, paste0(path_output, gsub(".csv", "/", lauf), "All Models/",
                                  gsub(":", ".", as.character(start)),
                                  "_Shap_datenModell_eval.rds"))
        #
        # get some high and low forecasts
        set.seed(12345)
        if (length(OOFforecast) >= 300) {
          selection <- c(which(OOFforecast == min(OOFforecast))[1],
                         try(sample(which(OOFforecast   >  stats::quantile(OOFforecast, 0.00) &
                                            OOFforecast <= stats::quantile(OOFforecast, 0.01)), 1), TRUE),
                         try(sample(which(OOFforecast   >  stats::quantile(OOFforecast, 0.04) &
                                            OOFforecast <= stats::quantile(OOFforecast, 0.05)), 1), TRUE),
                         try(sample(which(OOFforecast   >  stats::quantile(OOFforecast, 0.09) &
                                            OOFforecast <= stats::quantile(OOFforecast, 0.10)), 1), TRUE),
                         try(sample(which(OOFforecast   >  stats::quantile(OOFforecast, 0.19) &
                                            OOFforecast <= stats::quantile(OOFforecast, 0.20)), 1), TRUE),
                         try(sample(which(OOFforecast   >= stats::quantile(OOFforecast, 0.80) &
                                            OOFforecast <  stats::quantile(OOFforecast, 0.81)), 1), TRUE),
                         try(sample(which(OOFforecast   >= stats::quantile(OOFforecast, 0.90) &
                                            OOFforecast <  stats::quantile(OOFforecast, 0.91)), 1), TRUE),
                         try(sample(which(OOFforecast   >= stats::quantile(OOFforecast, 0.95) &
                                            OOFforecast <  stats::quantile(OOFforecast, 0.96)), 1), TRUE),
                         try(sample(which(OOFforecast   >= stats::quantile(OOFforecast, 0.99) &
                                            OOFforecast <  stats::quantile(OOFforecast, 1.00)), 1), TRUE),
                         which(OOFforecast   == max(OOFforecast))[1])
          selection <- selection[!grepl("Error", selection)]
        } else {
          high <- sample(which(OOFforecast >= stats::quantile(OOFforecast, 0.8)), 3)
          set.seed(12345)
          low <- sample(which(OOFforecast <= stats::quantile(OOFforecast, 0.2)), 3)
          selection <- c(low, high)[order(c(OOFforecast[low], OOFforecast[high]))]
          rm(high, low)
        }
        selection <- as.numeric(selection)
        set.seed(Sys.time())
        #
        # plot shapley contribution
        p <- list()
        for (plot_i in 1:length(selection)) {
          row <- selection[plot_i]
          p[[plot_i]] <- shapley[row, ] %>%
            t() %>%
            data.frame() %>%
            dplyr::select(-BIAS) %>%
            data.table::setnames(paste0(colnames(.), ": ",
                                        temp_data[row, colnames(.)] %>%
                                          dplyr::mutate_if(is.Date, as.character) %>%
                                          reshape2::melt(id = NULL) %>%
                                          dplyr::pull(value))) %>%
            data.table::setnames(gsub("_LabelEnc", "", names(.))) %>%
            reshape2::melt(id = NULL) %>%
            dplyr::arrange(-abs(value)) %>%
            dplyr::slice(1:min(5, nrow(.))) %>%
            ggplot2::ggplot(ggplot2::aes(x = stats::reorder(variable, abs(value)), y = value)) +
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::labs(x = "", y = "",
                          subtitle = paste0("pred = ", round(OOFforecast[row], 3),
                                            " (mean pred = ", round(mean(OOFforecast), 3),
                                            ", pred no. ", row, ")")) +
            ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
            ggplot2::theme(text = ggplot2::element_text(size = 8)) +
            ggplot2:coord_flip()
        }; rm(plot_i, row)
        p <- do.call(gridExtra::arrangeGrob, c(p, ncol = 2, as.table = FALSE))
        # p <- do.call(grid.arrange, c(p, ncol = 2, as.table = FALSE))
        ggplot2:ggsave(paste0(path_output, gsub(".csv", "/", lauf), "All Models/",
                              gsub(":", ".", as.character(start)), "_Shap_plot.png"),
                       plot = p, width = 9.92, height = 5.3)
        #
        # clean up
        rm(OOFforecast, selection, temp_data, shapley, p)
      }
    } # end "good" model
    #
    # clean up
    rm(bst, evaluation_log_); invisible(gc())
    #
    #
    #
    ### Run CV model
    if ((run_final_model & ((i == 1 & nrow(SummaryCV) == 0) |
                            condition_best |
                            (i == 2 & !is.null(best_params)))) |
        (run_final_model & feat_sel)) {
      if (test_param) temp[3] <- eta
      set.seed(12345)
      if (!metric %in% custom_metrics) {
        bst <- xgboost::xgb.cv(objective = obj, params = params,
                               eval_metric = metric, maximize = metrics_maximize,
                               eta = eta, max_depth = depth,
                               min_child_weight = min_child_weight, gamma = gamma,
                               subsample = subsample, colsample_bytree = colsample_bytree,
                               num_parallel_tree = trees, stratified = TRUE,
                               tree_method = tree_method, booster = booster,
                               rate_drop = rate_drop, skip_drop = skip_drop,
                               early_stopping_rounds = min(max(0.05 * nround_test, 5), 30),
                               missing = NA, data = d_train, nround = nround,
                               nfold = nfold, folds = folds, verbose = verbose,
                               print_every_n = ifelse(verbose == 0, nround, nround_test / 50),
                               prediction = TRUE)
      } else {
        bst <- xgboost::xgb.cv(objective = obj, params = params,
                               feval = get(metric), maximize = metrics_maximize,
                               eta = eta, max_depth = depth,
                               min_child_weight = min_child_weight, gamma = gamma,
                               subsample = subsample, colsample_bytree = colsample_bytree,
                               num_parallel_tree = trees, stratified = TRUE,
                               tree_method = tree_method, booster = booster,
                               rate_drop = rate_drop, skip_drop = skip_drop,
                               early_stopping_rounds = min(max(0.05 * nround_test, 5), 30),
                               missing = NA, data = d_train, nround = nround,
                               nfold = nfold, folds = folds, verbose = verbose,
                               print_every_n = ifelse(verbose == 0, nround, nround_test / 50),
                               prediction = TRUE)
      }
      invisible(gc())
      #
      # get results
      evaluation_log <- data.frame(bst$evaluation_log)
      if (metrics_maximize) {
        evaluation_log$overfit <- (1 - evaluation_log[, 2]) / (1 - evaluation_log[, 4]) - 1
      } else {
        evaluation_log$overfit <- evaluation_log[, 2] / evaluation_log[, 4] - 1
      }
      evaluation_log <- evaluation_log %>%
        dplyr::mutate(valid = overfit >= max_overfit)
      evaluation_log_ <- evaluation_log %>%
        dplyr::filter(valid == TRUE)
      if (nrow(evaluation_log_) == 0) evaluation_log_ <- evaluation_log[1, ]
      temp[4] <- ifelse(metrics_maximize == TRUE,
                        evaluation_log_$iter[which.max(evaluation_log_[, 4])],
                        evaluation_log_$iter[which.min(evaluation_log_[, 4])])
      # temp[4] <- bst$best_iteration
      temp[9] <- as.numeric(evaluation_log[temp[[4]], 2])
      temp[13] <- as.numeric(evaluation_log[temp[[4]], 3])
      temp[10] <- as.numeric(evaluation_log[temp[[4]], 4])
      temp[14] <- as.numeric(evaluation_log[temp[[4]], 5])
      cat(paste0("   Best score CV:   ", round(temp[10], 4),
                 " (train: ", round(temp[9], 4), ")", " at nrounds: ", temp[4], "\n"))
      #
      # Save OOFforecast
      if (!feat_sel) {
        OOFforecast <- readRDS(paste0(path_output, gsub(".csv", "/", lauf), "Data/OOFforecast.rds"))
        # OOFforecast <- read.table(paste0(path_output, gsub(".csv", "/", lauf),
        #                                         gsub(".csv", "", lauf), "_OOFforecast.csv"),
        #                           header = TRUE, sep = ";", dec = ",")
        OOFforecast <- cbind(OOFforecast, bst$pred)
        data.table::setnames(OOFforecast, names(OOFforecast)[ncol(OOFforecast)],
                 gsub(":", ".", as.character(temp[1])))
        saveRDS(OOFforecast, paste0(path_output, gsub(".csv", "/", lauf), "Data/OOFforecast.rds"))
        # write.table(OOFforecast, paste0(path_output, gsub(".csv", "/", lauf),
        #                                        gsub(".csv", "", lauf), "_OOFforecast.csv"),
        #             row.names = FALSE, col.names = TRUE, append = FALSE, sep = ";", dec = ",")
      }
      #
      # get error rate => save below
      if (metrics_maximize) {
        loss <- (1 - evaluation_log[bst$best_iteration, 4]) / (1 - as.numeric(temp[10])) - 1
      } else {
        loss <- evaluation_log[bst$best_iteration, 4] / as.numeric(temp[10]) - 1
      }
      error_rate <- evaluation_log %>%
        dplyr::select(iter, 2, 4)
      rm(bst, evaluation_log_)
      invisible(gc())
    }   # end "Run CV model"
    #
    #
    #
    # Run final model if (i = 1) or condition_best = TRUE and run_final_model
    if ((run_final_model & ((i == 1 & nrow(SummaryCV) == 0) |
                            condition_best |
                            (i == 2 & !is.null(best_params)))) |
        (run_final_model & feat_sel)) {
      #
      set.seed(12345)
      if (!metric %in% custom_metrics) {
        bst <- xgboost::xgboost(objective = obj, params = params, maximize = metrics_maximize,
                                eval_metric = metric, eta = eta, max_depth = depth,
                                min_child_weight = min_child_weight, gamma = gamma,
                                subsample = subsample, colsample_bytree = colsample_bytree,
                                num_parallel_tree = trees,
                                tree_method = tree_method, booster = booster,
                                rate_drop = rate_drop, skip_drop = skip_drop,
                                missing = NA, data = d_train, nround = temp[[4]], verbose = verbose,
                                print_every_n = ifelse(verbose == 0, nround, nround_test / 50))
      } else {
        bst <- xgboost::xgboost(objective = obj, params = params,
                                feval = get(metric), maximize = metrics_maximize,
                                eta = eta, max_depth = depth,
                                min_child_weight = min_child_weight, gamma = gamma,
                                subsample = subsample, colsample_bytree = colsample_bytree,
                                num_parallel_tree = trees,
                                tree_method = tree_method, booster = booster,
                                rate_drop = rate_drop, skip_drop = skip_drop,
                                missing = NA, data = d_train, nround = temp[[4]], verbose = verbose,
                                print_every_n = ifelse(verbose == 0, nround, nround_test / 50))
      }
      invisible(gc())
      #
      # Save TESTforecast
      TESTforecast <- readRDS(paste0(path_output, gsub(".csv", "/", lauf),
                                     "Data/TESTforecast.rds"))
      TESTforecast <- cbind(TESTforecast, stats::predict(bst, d_test, missing = NA))  # test_mat
      data.table::setnames(TESTforecast, names(TESTforecast)[ncol(TESTforecast)],
                           gsub(":", ".", as.character(temp[1])))
      saveRDS(TESTforecast, paste0(path_output, gsub(".csv", "/", lauf),
                                   "Data/TESTforecast.rds"))
      #
      # Save model
      xgboost::xgb.save(bst, paste0(path_output, gsub(".csv", "/", lauf), "All Models/",
                                    gsub(":", ".", as.character(start)), ".model"))
      saveRDS(bst, paste0(path_output, gsub(".csv", "/", lauf), "All Models/",
                          gsub(":", ".", as.character(start)), ".model.rds"))
      assign("bst", bst, envir = .GlobalEnv)
      invisible(gc())
    } # end "Run final model"
    #
    #
    #
    ### Results
    #
    # Save results as SummaryCV
    temp[11] <- round(as.numeric(difftime(Sys.time(), start, units = "mins")), 3)
    if (feat_sel) {
      temp[17] <- Feat_Sel
      temp[18] <- paste(Feat_Sel_Vars, collapse = ", ")
    } # else {
    #   temp[15] <- NA
    #   temp[16] <- NA
    # }
    temp[19] <- metric
    utils::write.table(temp, paste0(path_output, gsub(".csv", "/", lauf), "Summary.csv"),
                row.names = FALSE, col.names = FALSE, append = TRUE, sep = ";", dec = ",")
    #
    # Re-Read results
    SummaryCV <- SRxgboost_get_summary_CV(lauf)
    #
    # save log and plot of error rate
    if (exists("error_rate")) {
      saveRDS(evaluation_log,
              paste0(path_output, gsub(".csv", "/", lauf), "All Models/",
                     gsub(":", ".", as.character(start)), "_Evaluation_log.rds"))
      error_rate %>%
        reshape2::melt(id = "iter") %>%
        ggplot2::ggplot(ggplot2::aes(x = iter, y = value, colour = variable)) +
        ggplot2::geom_line() +
        {if (max_overfit != -1) ggplot2::geom_vline(xintercept = temp[[4]],
                                                    linetype = "dashed")} +
        {if (max_overfit != -1) ggplot2::geom_hline(yintercept = evaluation_log[temp[[4]], 3],
                                                    linetype = "dashed")} +
        ggplot2:labs(title = "Entwicklung der Fehlerrate", x = "iteration", y = metric, colour = "",
             subtitle = paste0("max_overfit = ", max_overfit * 100, "% ",
                               "(loss = ", min(round(loss * 100, 1), 0), "%)")) +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
        theme(legend.position = "top")
      ggplot2:ggsave(paste0(path_output, gsub(".csv", "/", lauf), "All Models/",
                    gsub(":", ".", as.character(start)), "_Error_rate.png"),
             width = 9.92, height = 5.3)  # 4.67
      rm(evaluation_log, error_rate, loss)
    }
    #
    #
    #
    ### Plot output
    #
    # calculate benchmark
    benchmark <- NA
    if (metric %in% c("error", "merror")) benchmark <- 1 - max(prop.table(table(y)))
    if (metric %in% c("auc")) benchmark <- 0.5
    if (metric %in% c("rmse")) benchmark <- Metrics::rmse(y, mean(y))  # sqrt(var(y))
    if (metric %in% c("rmsle")) benchmark <- Metrics::rmsle(y, mean(y))
    if (metric %in% c("msle")) benchmark <- Metrics::msle(y, mean(y))
    if (metric %in% c("mpe")) benchmark <- sum(mean(y) / y - 1) / length(y)
    if (metric %in% c("mape")) benchmark <- sum(abs(mean(y) / y - 1)) / length(y)
    if (metric %in% c("mae")) benchmark <- Metrics::mae(y, mean(y))
    if (metric %in% c("logloss")) benchmark <- Metrics::logLoss(y, mean(y))
    if (metric %in% c("mlogloss")) benchmark <- 0 # MLmetrics::MultiLogLoss(y, mean(y)) # funzt so ueberhaupt nicht!
    if (metric %in% c("qwk_score")) benchmark <-
      Metrics::ScoreQuadraticWeightedKappa(y, mean(y),
                                           min.rating = min(y), max.rating = max(y))
    if (metric %in% c("f1_score")) benchmark <- Metrics::f1(y, mean(y))
    if (metric %in% c("mcc_score")) benchmark <- 0
    # ROCR::performance(prediction(pred = rep(mean(y), length(y)), labels = y), measure = "mat")@x.values[[1]]
    #
    # Grafic output of runs
    suppressWarnings(
      if (sum(!is.na(SummaryCV$test)) > 0) {
        if (obj %in% c("binary:logistic", "multi:softmax", "multi:softprob")) {
          a <- ggplot2::qplot(index, eval_1fold, data = SummaryCV, main = "CV error",
                              ylab = metric, xlab = "Run", ylim = c(0, NA)) +
            ggplot2::geom_point(ggplot2::aes(y = SummaryCV$test), colour = "red",
                                size = 3.5) +
            ggplot2::geom_text(ggplot2::aes(y = SummaryCV$test *
                                              ifelse(metrics_maximize, 0.92, 1.08),
                                            label = round(SummaryCV$test,3)),
                               vjust = 0.4, angle = 90) +
            # geom_text(ggplot2::aes(y = SummaryCV$test, label = round(SummaryCV$test,3)),
            #           hjust = -0.1, vjust = 0.3, angle = 90) +
            ggplot2::geom_line(ggplot2::aes(y = SummaryCV$best_test), colour = "red") +
            ggplot2::geom_line(ggplot2::aes(y = benchmark), colour = "black")
        } else {
          a <- ggplot2::qplot(index, eval_1fold, data = SummaryCV, main = "CV error",
                              ylab = metric, xlab = "Run", ylim = c(0, NA)) +
            ggplot2::geom_point(ggplot2::aes(y = SummaryCV$test), colour = "red", size = 3.5) +
            ggplot2::geom_text(ggplot2::aes(y = SummaryCV$test *
                                              ifelse(metrics_maximize, 0.92, 1.08),
                                            label = round(SummaryCV$test,3)),
                               vjust = 0.4, angle = 90) +
            # geom_text(ggplot2::aes(y = SummaryCV$test, label = round(SummaryCV$test,3)),
            #           hjust = -0.1, vjust = 0.3, angle = 90) +
            ggplot2::geom_line(ggplot2::aes(y = SummaryCV$best_test), colour = "red") +
            ggplot2::geom_line(ggplot2::aes(y = benchmark), colour = "black")
        }
      } else {
        if (obj %in% c("binary:logistic", "multi:softmax", "multi:softprob")) {
          a <- ggplot2::qplot(index, eval_1fold, data = SummaryCV, main = "Test error",
                              ylab = metric, xlab = "Run", ylim = c(0, NA)) +
            ggplot2::geom_line(ggplot2::aes(y = benchmark), colour = "black")
        } else {
          a <- ggplot2::qplot(index, eval_1fold, data = SummaryCV, main = "Test error",
                              ylab = metric, xlab = "Run", ylim = c(0, NA)) +
            ggplot2::geom_line(ggplot2::aes(y = benchmark), colour = "black") +
            ggplot2::geom_line(ggplot2::aes(y = SummaryCV$best_test), colour = "red")
        }
      }   # end grafic
    )
    suppressMessages(try(print(a)))
    #
    # Save all other graphics if (i = 1) or condition_best = TRUE
    if ((run_final_model & ((i == 1 & nrow(SummaryCV) <= 1) |
                            condition_best |
                            (i == 2 & !is.null(best_params)))) |
        (run_final_model & feat_sel) |
        i == runs) {
      #
      # Plot train vs. test
      if (sum(!is.na(SummaryCV$test)) > 0) {
        b <- ggplot2::qplot(train, test, color = nround, data = SummaryCV,
                            main = "test vs train", asp = 1,
                            xlim = c(0,NA), ylim = c(0,NA)) +
          ggplot2::geom_abline(intercept = 0, slope = 1)
      }
      if (sum(!is.na(SummaryCV$test)) > 0) {
        suppressMessages({
          c <- gridExtra::arrangeGrob(a, b, layout_matrix = rbind(c(1,1,2)))   # grid.arrange without drawing
        })
      } else {
        c <- a
      }
      suppressMessages(
        ggplot2:ggsave(filename = paste0(path_output, gsub(".csv", "/", lauf), "Summary.png"),
                       plot = c, width = 9.92, height = 5.3))  # 4.67)
      #
      # Plot parameter check up: without/with interaction
      if (nrow(SummaryCV) >= 10) {
        temp_table <- SummaryCV[!is.na(SummaryCV$eval_1fold), c(2,5:8,12,15:16)]
        try( {
          mars <- earth::earth(eval_1fold~., data = temp_table, degree = 1, penalty = 2)
          grDevices::png(filename = paste0(path_output, gsub(".csv", "/", lauf),
                                           "Model parameter.png"),
                         width = 1200, height = 900)
          plotmo::plotmo(mars, nrug = "density", trace = -1, cex = 2,
                         caption = paste0("Model parameter (R2 = ",
                                          round(mars$rsq, 3), ")"))
          grDevices::dev.off()
        }, TRUE)
        try({
          mars <- earth::earth(eval_1fold~., data = temp_table, degree = 2, penalty = 3)
          grDevices::png(filename = paste0(path_output, gsub(".csv", "/", lauf),
                                           "Model parameter2.png"),
                         width = 1200, height = 900)
          plotmo::plotmo(mars, nrug = "density", trace = -1, cex = 2,
                         caption = paste0("Model parameter with interaction (R2 = ",
                                          round(mars$rsq, 3), ")"))
          grDevices::dev.off()
        }, TRUE)
      }
      #
      # Plot runtime and nrounds: for eval_1fold and full model
      try(rm(a,b),TRUE)
      if (nrow(SummaryCV[is.na(SummaryCV$test), ]) >= 1) {
        a <- ggplot2::qplot(nround, runtime, color = depth,
                            data = SummaryCV[is.na(SummaryCV$test), ],
                            xlim = c(0,NA), ylim = c(0,NA),
                            main = "Runtime and nrounds (eval_1fold)",
                            ylab = "Runtime (mins)")
      }
      if (nrow(SummaryCV[!is.na(SummaryCV$test), ]) >= 1) {
        b <- ggplot2::qplot(nround, runtime, color = depth,
                            data = SummaryCV[!is.na(SummaryCV$test), ],
                            xlim = c(0,NA), ylim = c(0,NA),
                            main = "Runtime and nrounds (full model)",
                            ylab = "Runtime (mins)")
      }
      try({
        rm(c)
        if (exists("a") & exists("b")) c <-
            gridExtra::arrangeGrob(a, b, nrow = 1, ncol = 2)   # grid.arrange without drawing
        if (exists("a") & !exists("b")) c <- a
        if (!exists("a") & exists("b")) c <- b
        # if (!exists("a") & !exists("b")) {
        if (exists("c")) {
          suppressMessages(
            ggplot2:ggsave(filename = paste0(path_output, gsub(".csv", "/", lauf),
                                             "Runtime and nrounds.png"), plot = c,
                           width = 9.92, height = 5.3))  # 4.67))
        }
      } , TRUE)
      #
      # Plot eval_1fold vs. CV error (test)
      SummaryCV_temp <- SummaryCV %>%
        dplyr::filter(!is.na(test))
      if (nrow(SummaryCV_temp) > 0) {
        try(rm(p), TRUE)
        min <- min(c(SummaryCV_temp$test, SummaryCV_temp$eval_1fold), na.rm = TRUE)
        max <- max(c(SummaryCV_temp$test, SummaryCV_temp$eval_1fold), na.rm = TRUE)
        p <- ggplot2::ggplot(SummaryCV_temp, ggplot2::aes(x = test, y = eval_1fold)) +
          ggplot2::geom_point() +
          ggplot2::geom_smooth(method = "loess", formula = "y ~ x", span = smooth_span * 2) +
          ggplot2::scale_x_continuous(limits = c(min, max)) +
          ggplot2::scale_y_continuous(limits = c(min, max)) +
          ggplot2::geom_abline(intercept = 0, slope = 1, colour = "red", linetype = "dashed")
        ggplot2:ggsave(paste0(path_output, gsub(".csv", "/", lauf), "Eval_1fold vs. test.png"),
                       plot = p, width = 9.92, height = 5.3)  # 4.67
      }
      #
      ### Clean up
      invisible(gc())
    }
  }   # end run(i)
  SRxgboost_get_summary_CV(lauf)
  set.seed(Sys.time())
  print(paste0(Sys.time()))
  #
  # Enable warnings again
  options(warn = old_warn)
  #
}   # end function
