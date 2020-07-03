#' SRxgboost_make_ensemble
#'
#' Make an ensemble model and predictions from previous runs.
#'
#' @param name character: folder name for results
#' @param lauf character: 1 or several "lauf"-names of previous runs, e.g. c("lauf1", "lauf2")
#' @param top_rank integer: 1 integer for each lauf, e.g. c(3, 5)
#'
#' @return forecast and model metrics for ensemble (OOF and TEST if available)
#'         as rds-file and as data.frame in memory.
#'
#' @export
SRxgboost_make_ensemble <- function(name,
                                    lauf,
                                    top_rank = 3) {
  # check if lauf ends with ".csv"
  for (i in seq_along(lauf)) {
    if (!grepl('.csv$', lauf[i])) lauf[i] <- paste0(lauf[i], ".csv")
  }; rm(i)
  #
  # check if name does not end with ".csv"
  if (grepl('.csv$', name)) name <- gsub(".csv$", "", name)
  #
  # delete old plots
  if (dir.exists(paste0(path_output, name))) {
    unlink(paste0(path_output, name, "/*"), recursive = TRUE)
  }
  #
  # create Ensemble folder
  dir.create(paste0(path_output, name), showWarnings = FALSE)
  #
  #
  #
  ### load results of runs
  #
  for (i in seq_along(lauf)) {
    SRxgboost_get_OOFforecast_TESTforecast(lauf = lauf[i],
                                           top_rank = top_rank[i])
    if (i == 1) {
      OOFforecast_temp <- OOFforecast %>%
        stats::setNames(c("id", paste0(gsub(".csv", "", lauf[i]), "__", names(.)[2:ncol(.)])))
      TESTforecast_temp <- TESTforecast %>%
        stats::setNames(c("id", paste0(gsub(".csv", "", lauf[i]), "__", names(.)[2:ncol(.)])))
      y_OOF_temp <- y_OOF
      if (exists("y_test")) y_test_temp <- y_test
    } else {
      OOFforecast_temp <-
        dplyr::left_join(OOFforecast_temp,
                         OOFforecast %>%
                           stats::setNames(c("id", paste0(gsub(".csv", "", lauf[i]),
                                                          "__", names(.)[2:ncol(.)]))),
                         by = "id")
      TESTforecast_temp <-
        dplyr::left_join(TESTforecast_temp,
                         TESTforecast %>%
                           stats::setNames(c("id", paste0(gsub(".csv", "", lauf[i]),
                                                          "__", names(.)[2:ncol(.)]))),
                         by = "id")
    }
  }; rm(i)
  OOFforecast <- OOFforecast_temp
  TESTforecast <- TESTforecast_temp
  y_OOF <- y_OOF_temp
  if (exists("y_test")) y_test <- y_test_temp
  rm(OOFforecast_temp, TESTforecast_temp, y_OOF_temp)
  if (exists("y_test")) rm(y_test_temp)
  invisible(gc())
  #
  # check number of columns returned and adapt top_rank
  top_rank <- sum(top_rank)                                                # recalculate in case of several runs
  if (ncol(OOFforecast) < top_rank + 1) top_rank <- ncol(OOFforecast) - 1  # less models than top_rank
  # if (ncol(OOFforecast) > top_rank + 1) top_rank <- ncol(OOFforecast) - 1  # "multi:softprob" models
  #
  #
  #
  ### make ensemble
  #
  if (objective %in% c("regression", "classification")) {
    # mean
    OOFforecast <- OOFforecast %>%
      dplyr::mutate(ensemble_mean = rowMeans(dplyr::select(., 2:(top_rank + 1))))
    TESTforecast <- TESTforecast %>%
      dplyr::mutate(ensemble_mean = rowMeans(dplyr::select(., 2:(top_rank + 1))))
    # median
    OOFforecast$ensemble_median <- apply(dplyr::select(OOFforecast, 2:(top_rank + 1)), 1, median)
    TESTforecast$ensemble_median <- apply(dplyr::select(TESTforecast, 2:(top_rank + 1)), 1, median)
    #
    # add y to forecasts
    OOFforecast <- dplyr::bind_cols(OOFforecast, y = y_OOF)
    if (exists("y_test")) TESTforecast <- dplyr::bind_cols(TESTforecast, y = y_test)
    #
    # GLM-weighted mean (be aware of overfitting !!!)
    glm <- stats::glm(y ~ ., data = OOFforecast %>% dplyr::select(., 2:(top_rank + 1), y),
                      family = ifelse(objective == "regression", "gaussian", "binomial"))
    weights <- data.frame(summary(glm)$coefficient) %>%
      tibble::rownames_to_column(., var = "variable") %>%
      dplyr::select(variable, estimate = Estimate) %>%
      dplyr::filter(variable != "(Intercept)") %>%
      dplyr::mutate(weight = round(abs(estimate) / sum(abs(estimate)), 4))
    cat("GLM weights: ", weights$weight, "\n")
    OOFforecast$ensemble_wmean_glm <- apply(dplyr::select(OOFforecast, 2:(top_rank + 1)), 1,
                                        function(x) stats::weighted.mean(x, w = weights$weight))
    TESTforecast$ensemble_wmean_glm <- apply(dplyr::select(TESTforecast, 2:(top_rank + 1)), 1,
                                         function(x) stats::weighted.mean(x, w = weights$weight))
    rm(glm, weights)
    #
    # accuracy/cor-weighted mean
    if (objective == "regression") {
      accuracy <- apply(OOFforecast %>% dplyr::select(., 2:(top_rank + 1)), 2,
                        function(x) Metrics::rmse(OOFforecast$y, x))
      accuracy <- 2 - accuracy / min(accuracy)
    } else {
      accuracy <- apply(OOFforecast[, 2:(top_rank + 1)], 2,
                        function(x) as.numeric(pROC::roc(response = OOFforecast$y,
                                                         predictor = x, algorithm = 2,
                                                         levels = c(0, 1),
                                                         direction = "<")$auc))
      accuracy <- 2 - accuracy / max(accuracy)
    }
    cor <- cor(OOFforecast %>% dplyr::select(., 2:(top_rank + 1)),
               use = "pairwise.complete.obs")[which.max(accuracy), ]
    weights <- accuracy ^ 20 / cor ^ 6
    weights[which.max(accuracy)] <- max(weights)
    weights <- round(weights / sum(weights), 4)
    cat("accuracy/cor-weights: ", weights, "\n",
        "accuracy: ", round(accuracy, 2), "   cor: ", round(cor, 2), "\n")
    OOFforecast$ensemble_wmean_cor <- apply(dplyr::select(OOFforecast, 2:(top_rank + 1)), 1,
                                            function(x) stats::weighted.mean(x, w = weights))
    TESTforecast$ensemble_wmean_cor <- apply(dplyr::select(TESTforecast, 2:(top_rank + 1)), 1,
                                             function(x) stats::weighted.mean(x, w = weights))
    rm(accuracy, cor, weights)
    #
    # Train caret ensemble models   xgbTree works best, but check overfitting!!!
    if (objective == "regression") {
      train <- OOFforecast %>% dplyr::select(., 2:(top_rank + 1), y)
      set.seed(12345)
      trcontrol <- caret::trainControl(method = "cv", number = 5, search = "grid",
                                       savePredictions = 'final')
      #
      # glm
      set.seed(12345)
      stack <- caret::train(y ~ ., method = "glm", data = train,
                            trControl = trcontrol, tuneLength = 3) # ; stack
      OOFforecast$ensemble_glm <- data.frame(stack$pred) %>%
        dplyr::arrange(rowIndex) %>%
        dplyr::pull(pred)
      TESTforecast$ensemble_glm <- stats::predict(stack, TESTforecast[, 2:(top_rank + 1)])
      rm(stack)
      #
      # ranger
      set.seed(12345)
      stack <- caret::train(y ~ ., method = "ranger", data = train,             # rf, ranger, extraTrees,
                            trControl = trcontrol, tuneLength = 3) # ; stack
      OOFforecast$ensemble_rf <- data.frame(stack$pred) %>%
        dplyr::arrange(rowIndex) %>%
        dplyr::pull(pred)
      TESTforecast$ensemble_rf <- stats::predict(stack, TESTforecast[, 2:(top_rank + 1)])
      rm(stack)
      #
      # xgb
      gridxgb <- expand.grid(eta = c(0.1),
                             nrounds = c(10, 30, 50, 100, 200),   # 10, 30, 100
                             max_depth = 1:4,
                             min_child_weight = c(1),
                             colsample_bytree = c(0.8),   # 0.5, 0.75, 1
                             gamma = 0,
                             subsample = 1)
      set.seed(12345)
      stack <- caret::train(y ~ ., method = "xgbTree", data = train,
                            tuneGrid = gridxgb, trControl = trcontrol) # ; stack
      # stack <- caret::train(y ~ ., method = "xgbTree", data = train,
      #                       trControl = trcontrol, tuneLength = 10) # tuneLength not working !!!
      OOFforecast$ensemble_xgb <- data.frame(stack$pred) %>%
        dplyr::arrange(rowIndex) %>%
        dplyr::pull(pred)
      TESTforecast$ensemble_xgb <- stats::predict(stack, TESTforecast[, 2:(top_rank + 1)])
      rm(stack)
      rm(train, trcontrol)
      set.seed(Sys.time())
    } else {
      # "classification"
      train <- OOFforecast %>%
        dplyr::select(., 2:(top_rank + 1), y) %>%
        dplyr::mutate(y = factor(dplyr::if_else(y >= 0.5, "ja", "nein")))
      set.seed(12345)
      trcontrol <- caret::trainControl(method = "cv", number = 5, search = "grid",
                                       savePredictions = 'final',
                                       classProbs = T, summaryFunction = twoClassSummary)
      #
      # glm
      set.seed(12345)
      stack <- caret::train(y ~ ., method = "glm", data = train, metric = "ROC",
                            trControl = trcontrol, tuneLength = 3) # ; stack
      OOFforecast$ensemble_glm <- data.frame(stack$pred) %>%
        dplyr::arrange(rowIndex) %>%
        dplyr::pull(ja)
      TESTforecast$ensemble_glm <- stats::predict(stack, TESTforecast[, 2:(top_rank + 1)],
                                                  type = "prob")$ja
      rm(stack)
      #
      # ranger
      set.seed(12345)
      stack <- caret::train(y ~ ., method = "ranger", data = train, metric = "ROC",  # rf, ranger, extraTrees,
                            trControl = trcontrol, tuneLength = 3) # ; stack
      OOFforecast$ensemble_rf <- data.frame(stack$pred) %>%
        dplyr::arrange(rowIndex) %>%
        dplyr::pull(ja)
      TESTforecast$ensemble_rf <- stats::predict(stack, TESTforecast[, 2:(top_rank + 1)],
                                                 type = "prob")$ja
      rm(stack)
      #
      # xgb
      gridxgb <- expand.grid(eta = c(0.1),
                             nrounds = c(10, 30, 50, 100, 200),   # 10, 30, 100
                             max_depth = 1:4,
                             min_child_weight = c(1),
                             colsample_bytree = c(0.8),   # 0.5, 0.75, 1
                             gamma = 0,
                             subsample = 1)
      set.seed(12345)
      stack <- caret::train(y ~ ., method = "xgbTree", data = train, metric = "ROC",
                            tuneGrid = gridxgb, trControl = trcontrol) # ; stack
      # stack <- caret::train(y ~ ., method = "xgbTree", data = train,
      #                       trControl = trcontrol, tuneLength = 10) # tuneLength not working !!!
      OOFforecast$ensemble_xgb <- data.frame(stack$pred) %>%
        dplyr::arrange(rowIndex) %>%
        dplyr::pull(ja)
      TESTforecast$ensemble_xgb <- stats::predict(stack, TESTforecast[, 2:(top_rank + 1)],
                                                  type = "prob")$ja
      rm(stack)
      rm(train, trcontrol)
      set.seed(Sys.time())
    }
    #
    # reorder columns, so that y is last column
    OOFforecast <- OOFforecast %>% dplyr::select(-y, dplyr::everything(), y)
    if (exists("y_test")) {
      TESTforecast <- TESTforecast %>% dplyr::select(-y, dplyr::everything(), y)
    }
    #
    # initialise metrics
    OOF_metrics <- data.frame(row.names = names(OOFforecast)[2:(ncol(OOFforecast) - 1)]) %>%
      tibble::rownames_to_column(var = "model")
    if (exists("y_test")) TEST_metrics <- OOF_metrics
  } else {
    # "multilabel"
    if (ncol(OOFforecast) > top_rank + 1) {
      # "multi:softprob"
      classes <- (ncol(OOFforecast) - 1) / top_rank - 1
      # classes <- grep("class", names(OOFforecast))[1] - 2
      #
      # mean
      for (i in 2:(2 + classes - 1)) {
        OOFforecast <- OOFforecast %>%
          dplyr::mutate(!!paste0("ensemble_mean", "_X", i - 1) :=
                          rowMeans(dplyr::select(., dplyr::ends_with(paste0("_X", i - 1)))))
        TESTforecast <- TESTforecast %>%
          dplyr::mutate(!!paste0("ensemble_mean", "_X", i - 1) :=
                          rowMeans(dplyr::select(., dplyr::ends_with(paste0("_X", i - 1)))))
      }; rm(i)
      OOFforecast <- OOFforecast %>%
        dplyr::mutate(ensemble_mean_class =
                        as.numeric(apply(dplyr::select(OOFforecast,
                                                       dplyr::starts_with("ensemble_mean_")),
                                         MARGIN = 1, which.max)) - 1)
      TESTforecast <- TESTforecast %>%
        dplyr::mutate(ensemble_mean_class =
                        as.numeric(apply(dplyr::select(TESTforecast,
                                                       dplyr::starts_with("ensemble_mean_")),
                                         MARGIN = 1, which.max)) - 1)
      #
      # median
      for (i in 2:(2 + classes - 1)) {
        OOFforecast[, paste0("ensemble_median", "_X", i - 1)] <-
          apply(dplyr::select(OOFforecast, dplyr::ends_with(paste0("_X", i - 1))), 1, median)
        TESTforecast[, paste0("ensemble_median", "_X", i - 1)] <-
          apply(dplyr::select(TESTforecast, dplyr::ends_with(paste0("_X", i - 1))), 1, median)
      }; rm(i)
      OOFforecast <- OOFforecast %>%
        dplyr::mutate(ensemble_median_class =
                        as.numeric(apply(dplyr::select(OOFforecast,
                                                       dplyr::starts_with("ensemble_median_")),
                                         MARGIN = 1, which.max)) - 1)
      TESTforecast <- TESTforecast %>%
        dplyr::mutate(ensemble_median_class =
                        as.numeric(apply(dplyr::select(TESTforecast,
                                                       dplyr::starts_with("ensemble_median_")),
                                         MARGIN = 1, which.max)) - 1)
      #
      # add y to forecasts
      OOFforecast <- dplyr::bind_cols(OOFforecast, y = y_OOF)
      if (exists("y_test")) TESTforecast <- dplyr::bind_cols(TESTforecast, y = y_test)
      #
      # GLM-weighted mean (be aware of overfitting !!!)
      glm <- OOFforecast %>%
        dplyr::select(y, 1:(1 + top_rank * (classes + 1))) %>%
        dplyr::select(y, dplyr::contains("__class")) %>%
        nnet::multinom(y ~ ., data = .)
      weights <- data.frame(t(summary(glm)$coefficient)) %>%
        tibble::rownames_to_column(var = "variable") %>%
        dplyr::mutate(estimate = rowMeans(dplyr::select(., 2:ncol(.)))) %>%
        dplyr::filter(variable != "(Intercept)") %>%
        dplyr::mutate(weight = round(abs(estimate) / sum(abs(estimate)), 4))
      cat("GLM weights: ", weights$weight, "\n")
      for (i in 2:(2 + classes - 1)) {
        OOFforecast[, paste0("ensemble_wmean_glm", "_X", i - 1)] <-
          apply(dplyr::select(OOFforecast[, 1:(1 + top_rank * (classes + 1))],
                              dplyr::ends_with(paste0("_X", i - 1))), 1,
                function(x) stats::weighted.mean(x, w = weights$weight))
        TESTforecast[, paste0("ensemble_wmean_glm", "_X", i - 1)] <-
          apply(dplyr::select(TESTforecast[, 1:(1 + top_rank * (classes + 1))],
                              dplyr::ends_with(paste0("_X", i - 1))), 1,
                function(x) stats::weighted.mean(x, w = weights$weight))
      }; rm(i)
      OOFforecast <- OOFforecast %>%
        dplyr::mutate(ensemble_wmean_glm_class =
                        as.numeric(apply(dplyr::select(OOFforecast,
                                                       dplyr::starts_with("ensemble_wmean_glm_")),
                                         MARGIN = 1, which.max)) - 1)
      TESTforecast <- TESTforecast %>%
        dplyr::mutate(ensemble_wmean_glm_class =
                        as.numeric(apply(dplyr::select(TESTforecast,
                                                       dplyr::starts_with("ensemble_wmean_glm_")),
                                         MARGIN = 1, which.max)) - 1)
      rm(glm, weights)
      #
      # accuracy/cor-weighted mean
      accuracy <- apply(OOFforecast %>%
                          dplyr::select(1:(1 + top_rank * (classes + 1))) %>%
                          dplyr::select(dplyr::contains("__class")), 2,
                        function(x) caret::confusionMatrix(table(x, OOFforecast$y),
                                                           positive = "1")$overall[1])
      accuracy <- accuracy / max(accuracy)
      cor <- cor(OOFforecast %>%
                   dplyr::select(1:(1 + top_rank * (classes + 1))) %>%
                   dplyr::select(dplyr::contains("__class")),
                 use = "pairwise.complete.obs")[which.max(accuracy), ]
      weights <- accuracy ^ 20 / cor ^ 6
      weights[which.max(accuracy)] <- max(weights)
      weights <- round(weights / sum(weights), 4)
      cat("accuracy/cor-weights: ", weights, "\n",
          "accuracy: ", round(accuracy, 2), "   cor: ", round(cor, 2), "\n")
      for (i in 2:(2 + classes - 1)) {
        OOFforecast[, paste0("ensemble_wmean_cor", "_X", i - 1)] <-
          apply(dplyr::select(OOFforecast[, 1:(1 + top_rank * (classes + 1))],
                              dplyr::ends_with(paste0("_X", i - 1))), 1,
                function(x) stats::weighted.mean(x, w = weights))
        TESTforecast[, paste0("ensemble_wmean_cor", "_X", i - 1)] <-
          apply(dplyr::select(TESTforecast[, 1:(1 + top_rank * (classes + 1))],
                              dplyr::ends_with(paste0("_X", i - 1))), 1,
                function(x) stats::weighted.mean(x, w = weights))
      }; rm(i)
      OOFforecast <- OOFforecast %>%
        dplyr::mutate(ensemble_wmean_cor_class =
                        as.numeric(apply(dplyr::select(OOFforecast,
                                                       dplyr::starts_with("ensemble_wmean_cor_")),
                                         MARGIN = 1, which.max)) - 1)
      TESTforecast <- TESTforecast %>%
        dplyr::mutate(ensemble_wmean_cor_class =
                        as.numeric(apply(dplyr::select(TESTforecast,
                                                       dplyr::starts_with("ensemble_wmean_cor_")),
                                         MARGIN = 1, which.max)) - 1)
      rm(accuracy, cor, weights)
      #
      # browser()
      # # Train caret ensemble models   xgbTree works best, but check overfitting!!!
      # train <- OOFforecast %>% dplyr::select(., 2:(top_rank + 1), y)
      # set.seed(12345)
      # trcontrol <- caret::trainControl(method = "cv", number = 5, search = "grid",
      #                                  savePredictions = 'final')
      # #
      # # glm
      # set.seed(12345)
      # stack <- caret::train(y ~ ., method = "glm", data = train,
      #                       trControl = trcontrol, tuneLength = 3) # ; stack
      # OOFforecast$ensemble_glm <- data.frame(stack$pred) %>%
      #   dplyr::arrange(rowIndex) %>%
      #   dplyr::pull(pred)
      # TESTforecast$ensemble_glm <- stats::predict(stack, TESTforecast[, 2:(top_rank + 1)])
      # rm(stack)
      # #
      # # ranger
      # set.seed(12345)
      # stack <- caret::train(y ~ ., method = "ranger", data = train,             # rf, ranger, extraTrees,
      #                       trControl = trcontrol, tuneLength = 3) # ; stack
      # OOFforecast$ensemble_rf <- data.frame(stack$pred) %>%
      #   dplyr::arrange(rowIndex) %>%
      #   dplyr::pull(pred)
      # TESTforecast$ensemble_rf <- stats::predict(stack, TESTforecast[, 2:(top_rank + 1)])
      # rm(stack)
      # #
      # # xgb
      # gridxgb <- expand.grid(eta = c(0.1),
      #                        nrounds = c(10, 30, 50, 100, 200),   # 10, 30, 100
      #                        max_depth = 1:4,
      #                        min_child_weight = c(1),
      #                        colsample_bytree = c(0.8),   # 0.5, 0.75, 1
      #                        gamma = 0,
      #                        subsample = 1)
      # set.seed(12345)
      # stack <- caret::train(y ~ ., method = "xgbTree", data = train,
      #                       tuneGrid = gridxgb, trControl = trcontrol) # ; stack
      # # stack <- caret::train(y ~ ., method = "xgbTree", data = train,
      # #                       trControl = trcontrol, tuneLength = 10) # tuneLength not working !!!
      # OOFforecast$ensemble_xgb <- data.frame(stack$pred) %>%
      #   dplyr::arrange(rowIndex) %>%
      #   dplyr::pull(pred)
      # TESTforecast$ensemble_xgb <- stats::predict(stack, TESTforecast[, 2:(top_rank + 1)])
      # rm(stack)
      # rm(train, trcontrol)
      # set.seed(Sys.time())
      #
      # reorder columns, so that y is last column
      OOFforecast <- OOFforecast %>% dplyr::select(-y, dplyr::everything(), y)
      if (exists("y_test")) {
        TESTforecast <- TESTforecast %>% dplyr::select(-y, dplyr::everything(), y)
      }
      #
      # initialise metrics
      OOF_metrics <- data.frame(row.names = names(OOFforecast)[2:(ncol(OOFforecast) - 1)] %>%
                                  gsub("_X.?$", "", .) %>%
                                  unique() %>%
                                  .[!grepl("_class", .)]) %>%
        tibble::rownames_to_column(var = "model")
      if (exists("y_test")) TEST_metrics <- OOF_metrics
    } else {
      # "multi:softmax"   TODO !!!
      browser()
      # ...
    }
  }
  #
  #
  #
  ### calculate metrics and plot results
  #
  # regression
  if (objective == "regression") {
    ## OOF
    #
    # calculate metrics
    OOF_metrics$RMSE <- apply(OOFforecast[, 2:(ncol(OOFforecast) - 1)], 2,
                              function(x) Metrics::rmse(OOFforecast$y, x))
    OOF_metrics$MAE <- apply(OOFforecast[, 2:(ncol(OOFforecast) - 1)], 2,
                             function(x) Metrics::mae(OOFforecast$y, x))
    OOF_metrics$MAPE <- apply(OOFforecast[, 2:(ncol(OOFforecast) - 1)], 2,
                              function(x) sum(abs(x / OOFforecast$y - 1)) /
                                length(OOFforecast$y))
    OOF_metrics$R2 <- apply(OOFforecast[, 2:(ncol(OOFforecast) - 1)], 2,
                            function(x) stats::cor(OOFforecast$y, x,
                                                   use = "pairwise.complete.obs")^2)
    OOF_metrics <- OOF_metrics %>%
      dplyr::arrange(-RMSE) %>%
      dplyr::mutate(model = factor(model, levels = .$model))
    saveRDS(OOF_metrics, paste0(path_output, name, "/OOF_metrics.rds"))
    assign('OOF_metrics', OOF_metrics, envir = .GlobalEnv)
    #
    # add column with best ensemble forecasts in OOF
    best_ensemble_OOF <- OOF_metrics %>%
      dplyr::arrange(RMSE) %>%
      dplyr::filter(grepl("ensemble", model)) %>%
      dplyr::slice(1) %>%
      dplyr::pull(model) %>%
      as.character()
    OOFforecast <- OOFforecast %>%
      dplyr::mutate(ensemble_best = .[, best_ensemble_OOF])
    saveRDS(OOFforecast, paste0(path_output, name, "/OOFforecast.rds"))
    assign('OOFforecast', OOFforecast, envir = .GlobalEnv)
    #
    # add column with best ensemble forecasts in TEST
    TESTforecast <- TESTforecast %>%
      dplyr::mutate(ensemble_best = .[, best_ensemble_OOF])
    saveRDS(TESTforecast, paste0(path_output, name, "/TESTforecast.rds"))
    assign('TESTforecast', TESTforecast, envir = .GlobalEnv)
    #
    # plot comparison of model metrics
    p <- OOF_metrics %>%
      reshape2::melt(id = "model") %>%
      ggplot2::ggplot(ggplot2::aes(x = model, y = value, fill = variable)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::geom_text(ggplot2::aes(label = round(value, 3)),
                         position = ggplot2::position_stack(0.5)) +
      ggplot2::facet_wrap(variable~., scales = "free_x") +
      ggplot2::labs(x = "", y = "", title = "OOF Comparison of model metrics") +
      ggplot2::guides(fill = FALSE) +
      ggplot2::coord_flip()
    print(p)
    ggplot2::ggsave(paste0(path_output, name, "/OOF Comparison of model metrics.png"),
                    plot = p, width = 9.92, height = 5.3)  # 4.67
    rm(p)
    #
    # Plot y vs. best model ensemble prediction
    p <- OOFforecast %>%
      ggplot2::ggplot(ggplot2::aes(x = y, y = ensemble_best)) +
      ggplot2::geom_point() +
      ggplot2::geom_abline(intercept = 0, slope = 1,
                           colour = "red", linetype = "dashed") +
      ggplot2::labs(y = "model prediction", title = "y vs. model prediction OOF",
                    subtitle = paste0("RMSE:    ", round(Metrics::rmse(OOFforecast$y,
                                                                       OOFforecast$ensemble_best), 3) %>%
                                        format(., nsmall = 3),
                                      "\nMAE:      ",
                                      round(Metrics::mae(OOFforecast$y,
                                                         OOFforecast$ensemble_best), 3) %>%
                                        format(., nsmall = 3),
                                      "\nMAPE:    ",
                                      round(sum(abs(OOFforecast$ensemble_best /
                                                      OOFforecast$y - 1)) /
                                              length(OOFforecast$y), 3) %>%
                                        format(., nsmall = 3),
                                      "\nR2:          ",
                                      round(stats::cor(OOFforecast$y,
                                                       OOFforecast$ensemble_best,
                                                       use = "pairwise.complete.obs")^2, 3) %>%
                                        format(., nsmall = 3))) # ,
    # "\nAUC:       ", round(pROC::auc(OOFforecast$y,
    #                                  OOFforecast$ensemble_best), 3)))
    print(p)
    ggplot2::ggsave(paste0(path_output, name, "/OOF Accuracy y vs. model prediction.png"),
                    plot = p, width = 9.92, height = 5.3)  # 4.67
    rm(p)
    #
    # Lift Chart
    OOFforecast$group <- cut(OOFforecast$ensemble_best,
                             breaks = unique(stats::quantile(OOFforecast$ensemble_best,
                                                             probs = seq(0, 1.01, by = 1/20),
                                                             na.rm = TRUE)),
                             ordered_result = TRUE, dig.lab = 10,
                             include.lowest = TRUE, labels = FALSE)
    temp <- OOFforecast %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(Actual = mean(y), Predicted = mean(ensemble_best))
    OOFforecast$group <- NULL
    temp <- reshape2::melt(temp, id.vars = "group")
    ggplot2::ggplot(temp, ggplot2::aes(x = group, y = value, colour = variable)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
      ggplot2::labs(title = "OOF Lift Chart", y = "y", x = "Sorted Predictions", colour = "")
    ggplot2::ggsave(paste0(path_output, name, "/OOF Lift Chart.png"),
                    width = 9.92, height = 5.3)  # 4.67
    rm(temp)
    #
    #
    ## TEST
    #
    if (exists("y_test")) {
      # calculate metrics
      TEST_metrics$RMSE <- apply(TESTforecast[, 2:(ncol(TESTforecast) - 2)], 2,
                                 function(x) Metrics::rmse(TESTforecast$y, x))
      TEST_metrics$MAE <- apply(TESTforecast[, 2:(ncol(TESTforecast) - 2)], 2,
                                function(x) Metrics::mae(TESTforecast$y, x))
      TEST_metrics$MAPE <- apply(TESTforecast[, 2:(ncol(TESTforecast) - 2)], 2,
                                 function(x) sum(abs(x / TESTforecast$y - 1)) /
                                   length(TESTforecast$y))
      TEST_metrics$R2 <- apply(TESTforecast[, 2:(ncol(TESTforecast) - 2)], 2,
                               function(x) stats::cor(TESTforecast$y, x,
                                                      use = "pairwise.complete.obs")^2)
      TEST_metrics <- TEST_metrics %>%
        dplyr::mutate(model = factor(model, levels = levels(OOF_metrics$model))) %>%
        dplyr::arrange(model)
      saveRDS(TEST_metrics, paste0(path_output, name, "/TEST_metrics.rds"))
      assign('TEST_metrics', TEST_metrics, envir = .GlobalEnv)
      #
      # plot comparison of model metrics
      p <- TEST_metrics %>%
        reshape2::melt(id = "model") %>%
        ggplot2::ggplot(ggplot2::aes(x = model, y = value, fill = variable)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::geom_text(ggplot2::aes(label = round(value, 3)),
                           position = ggplot2::position_stack(0.5)) +
        ggplot2::facet_wrap(variable~., scales = "free_x") +
        ggplot2::labs(x = "", y = "", title = "TEST Comparison of model metrics") +
        ggplot2::guides(fill = FALSE) +
        ggplot2::coord_flip()
      print(p)
      ggplot2::ggsave(paste0(path_output, name, "/TEST Comparison of model metrics.png"),
                      plot = p, width = 9.92, height = 5.3)  # 4.67
      rm(p)
      #
      # Plot y vs. best model ensemble prediction
      p <- TESTforecast %>%
        ggplot2::ggplot(ggplot2::aes(x = y, y = ensemble_best)) +
        ggplot2::geom_point() +
        ggplot2::geom_abline(intercept = 0, slope = 1,
                             colour = "red", linetype = "dashed") +
        ggplot2::labs(y = "model prediction", title = "y vs. model prediction TEST",
                      subtitle = paste0("RMSE:    ", round(Metrics::rmse(TESTforecast$y,
                                                                         TESTforecast$ensemble_best), 3) %>%
                                          format(., nsmall = 3),
                                        "\nMAE:      ",
                                        round(Metrics::mae(TESTforecast$y,
                                                           TESTforecast$ensemble_best), 3) %>%
                                          format(., nsmall = 3),
                                        "\nMAPE:    ",
                                        round(sum(abs(TESTforecast$ensemble_best /
                                                        TESTforecast$y - 1)) /
                                                length(TESTforecast$y), 3) %>%
                                          format(., nsmall = 3),
                                        "\nR2:          ",
                                        round(stats::cor(TESTforecast$y,
                                                         TESTforecast$ensemble_best,
                                                         use = "pairwise.complete.obs")^2, 3) %>%
                                          format(., nsmall = 3))) # ,
      # "\nAUC:       ", round(pROC::auc(TESTforecast$y,
      #                                  TESTforecast$ensemble_best), 3)))
      print(p)
      ggplot2::ggsave(paste0(path_output, name, "/TEST Accuracy y vs. model prediction.png"),
                      plot = p, width = 9.92, height = 5.3)  # 4.67
      rm(p)
      #
      # Lift Chart
      TESTforecast$group <- cut(TESTforecast$ensemble_best,
                                breaks = unique(stats::quantile(TESTforecast$ensemble_best,
                                                                probs = seq(0, 1.01, by = 1/20),
                                                                na.rm = TRUE)),
                                ordered_result = TRUE, dig.lab = 10,
                                include.lowest = TRUE, labels = FALSE)
      temp <- TESTforecast %>%
        dplyr::group_by(group) %>%
        dplyr::summarise(Actual = mean(y), Predicted = mean(ensemble_best))
      TESTforecast$group <- NULL
      temp <- reshape2::melt(temp, id.vars = "group")
      ggplot2::ggplot(temp, ggplot2::aes(x = group, y = value, colour = variable)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
        ggplot2::labs(title = "TEST Lift Chart", y = "y", x = "Sorted Predictions", colour = "")
      ggplot2::ggsave(paste0(path_output, name, "/TEST Lift Chart.png"),
                      width = 9.92, height = 5.3)  # 4.67
      rm(temp)
    }
  } else if (objective == "classification") {
    # define mcc function
    # ROCR currently supports only evaluation of binary classification tasks !!!
    mcc_ <- function(pred, labels, cutoff = NULL) {
      temp <- data.frame(pred = pred, labels = labels)
      if (length(pred) > 100000) {   # NAs produced by integer overflow > 500'000
        set.seed(12345)
        temp <- temp %>% dplyr::sample_n(100000)
        set.seed(Sys.time())
      }
      prediction <- ROCR::prediction(temp$pred, temp$labels)
      mcc <- ROCR::performance(prediction, "mat")
      if (is.null(cutoff)) {
        # select optimal cutoff
        err <- max(mcc@y.values[[1]], na.rm = TRUE)
        opt_cutoff = mcc@x.values[[1]][which.max(mcc@y.values[[1]])]
      } else {
        # take mcc at given cutoff
        err <- mcc@y.values[[1]][[min(which(mcc@x.values[[1]] <= cutoff))]]
        opt_cutoff = cutoff
      }
      return(list(mcc = err, opt_cutoff = opt_cutoff))
    }
    #
    #
    ## OOF
    #
    # calculate metrics
    OOF_metrics$AUC <- apply(OOFforecast[, 2:(ncol(OOFforecast) - 1)], 2,
                             function(x) as.numeric(pROC::roc(response = OOFforecast$y,
                                                              predictor = x, algorithm = 2,
                                                              levels = c(0, 1),
                                                              direction = "<")$auc))
    OOF_metrics$MCC <- apply(OOFforecast[, 2:(ncol(OOFforecast) - 1)], 2,
                             function(x) mcc_(x, OOFforecast$y)$mcc)
    OOF_metrics$Accuracy <- apply(OOFforecast[, 2:(ncol(OOFforecast) - 1)], 2,
                                  function(x) caret::confusionMatrix(table(
                                    ifelse(x > 0.5, 1, 0), OOFforecast$y),
                                    positive = "1")$overall[1])
    OOF_metrics$Sensitivity_TPR <- apply(OOFforecast[, 2:(ncol(OOFforecast) - 1)], 2,
                                         function(x) caret::confusionMatrix(table(
                                           ifelse(x > 0.5, 1, 0), OOFforecast$y),
                                           positive = "1")$byClass[1])
    OOF_metrics <- OOF_metrics %>%
      dplyr::arrange(AUC) %>%
      dplyr::mutate(model = factor(model, levels = .$model))
    saveRDS(OOF_metrics, paste0(path_output, name, "/OOF_metrics.rds"))
    assign('OOF_metrics', OOF_metrics, envir = .GlobalEnv)
    #
    # add column with best ensemble forecasts in OOF
    best_ensemble_OOF <- OOF_metrics %>%
      dplyr::arrange(-AUC) %>%
      dplyr::filter(grepl("ensemble", model)) %>%
      dplyr::slice(1) %>%
      dplyr::pull(model) %>%
      as.character()
    OOFforecast <- OOFforecast %>%
      dplyr::mutate(ensemble_best = .[, best_ensemble_OOF])
    saveRDS(OOFforecast, paste0(path_output, name, "/OOFforecast.rds"))
    assign('OOFforecast', OOFforecast, envir = .GlobalEnv)
    #
    # add column with best ensemble forecasts in TEST
    TESTforecast <- TESTforecast %>%
      dplyr::mutate(ensemble_best = .[, best_ensemble_OOF])
    saveRDS(TESTforecast, paste0(path_output, name, "/TESTforecast.rds"))
    assign('TESTforecast', TESTforecast, envir = .GlobalEnv)
    #
    # plot comparison of model metrics
    p <- OOF_metrics %>%
      reshape2::melt(id = "model") %>%
      ggplot2::ggplot(ggplot2::aes(x = model, y = value, fill = variable)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::geom_text(ggplot2::aes(label = round(value, 3)),
                         position = ggplot2::position_stack(0.5)) +
      ggplot2::facet_wrap(variable~., scales = "free_x") +
      ggplot2::labs(x = "", y = "", title = "OOF Comparison of model metrics") +
      ggplot2::guides(fill = FALSE) +
      ggplot2::coord_flip()
    print(p)
    ggplot2::ggsave(paste0(path_output, name, "/OOF Comparison of model metrics.png"),
                    plot = p, width = 9.92, height = 5.3)  # 4.67
    rm(p)
    #
    # calculate ROC-curve
    ROC <- pROC::roc(response = OOFforecast$y,
                     predictor = OOFforecast$ensemble_best, algorithm = 2,
                     levels = c(0, 1), direction = "<")
    # ROC <- pROC::roc(response = OOFforecast$y,
    #                  predictor = OOFforecast$ensemble_best, algorithm = 2, quiet = TRUE)
    #
    # Accuracy and Cut off
    temp <- data.frame(threshold = ROC$thresholds,
                       true_positive_rate = ROC$sensitivities,
                       true_negative_rate = ROC$specificities)
    temp$balanced_accuracy <- (temp$true_positive_rate + temp$true_negative_rate) / 2
    temp$accuracy <-
      (temp$true_positive_rate * sum(OOFforecast$y == 1) +
         temp$true_negative_rate * sum(OOFforecast$y == 0)) /
      (sum(OOFforecast$y == 1) + sum(OOFforecast$y == 0))
    temp <- temp %>% dplyr::filter(!threshold %in% c(-Inf, Inf))   # remove -Inf, Inf thresholds NEW !!!
    # calculate cut off
    cut_off_ba <- round(temp$threshold[which.max(temp$balanced_accuracy)], 3)
    cut_off_ba_max <- max(temp$balanced_accuracy)
    cut_off_a <- round(temp$threshold[which.max(temp$accuracy)], 3)
    if (cut_off_a == 1) cut_off_a <- 0.99
    if (cut_off_a == 0) cut_off_a <- 0.01
    cut_off_a_max <- max(temp$accuracy)
    # plot accuracy
    temp$threshold[temp$threshold < 0] <- 0
    temp$threshold[temp$threshold > 1] <- 1
    temp_long <- reshape2::melt(temp, id = "threshold")
    ggplot2::ggplot(temp_long, ggplot2::aes(x = threshold, y = value, colour = variable)) +
      ggplot2::geom_line() +
      ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_ba),
                          linetype = "dashed", colour = "turquoise3") +
      ggplot2::annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
                        label = paste0("cut off ba = ", cut_off_ba)) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_a),
                          linetype = "dashed", colour = "darkorchid1") +
      ggplot2::annotate("text", x = cut_off_a + 0.07, y = 0.05, colour = "darkorchid1",
                        label = paste0("cut off a = ", cut_off_a)) +
      ggplot2::scale_x_continuous(limits = c(0, 1),
                                  breaks = scales::pretty_breaks(6)) +
      ggplot2::scale_y_continuous(limits = c(0, 1),
                                  breaks = scales::pretty_breaks(6)) +
      ggplot2::labs(title = "OOF Accuracy and cut off", x = "Threshold",
                    y = "Accuracy", colour = "") +
      ggplot2::theme(legend.position = "top")
    ggplot2::ggsave(paste0(path_output, name, "/OOF Accuracy and Cut off.png"),
                    width = 9.92, height = 5.3)
    #
    # Cut off and Costs
    temp$costs_1_to_1 <-
      temp$true_positive_rate * sum(OOFforecast$y == 1) * 1 +
      temp$true_negative_rate * sum(OOFforecast$y == 0) * 1
    temp$costs_1_to_3 <-
      temp$true_positive_rate * sum(OOFforecast$y == 1) * 1 +
      temp$true_negative_rate * sum(OOFforecast$y == 0) * 3
    temp$costs_3_to_1 <-
      temp$true_positive_rate * sum(OOFforecast$y == 1) * 3 +
      temp$true_negative_rate * sum(OOFforecast$y == 0) * 1
    #
    temp %>%
      dplyr::select(-true_positive_rate, -true_negative_rate, -balanced_accuracy,
                    -accuracy) %>%
      reshape2::melt(id = "threshold") %>%
      ggplot2::ggplot(ggplot2::aes(x = threshold, y = value, colour = variable)) +
      ggplot2::geom_line() +
      ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_ba),
                          linetype = "dashed", colour = "turquoise3") +
      ggplot2::annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
                        label = paste0("cut off ba = ", cut_off_ba)) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_a),
                          linetype = "dashed", colour = "darkorchid1") +
      ggplot2::annotate("text", x = cut_off_a + 0.07, y = 0.05, colour = "darkorchid1",
                        label = paste0("cut off a = ", cut_off_a)) +
      ggplot2::scale_x_continuous(limits = c(0, 1),
                                  breaks = scales::pretty_breaks(6)) +
      ggplot2::scale_y_continuous(limits = c(0, NA),
                                  breaks = scales::pretty_breaks(6)) +
      ggplot2::labs(title = "OOF Cut off and costs", x = "Threshold",
                    y = "Costs", colour = "") +
      ggplot2::theme(legend.position = "top")
    ggplot2::ggsave(paste0(path_output, name, "/OOF Cut off and Costs.png"),
                    width = 9.92, height = 5.3)
    #
    # save ROC data
    utils::write.table(temp, paste0(path_output, name, "/OOF Cut off, accuracy and costs.csv"),
                       row.names = FALSE, sep = ";")
    #
    # confusion matrix
    if (!cut_off_a %in% c(-Inf, Inf, 0, 1)) {
      confusion_matrix <-
        caret::confusionMatrix(table(ifelse(OOFforecast$ensemble_best > cut_off_a, 1, 0),
                                     OOFforecast$y),
                               positive = "1")
      print(confusion_matrix)
      sink(paste0(path_output, name, "/OOF Confusion matrix.txt"), append = FALSE)
      print(confusion_matrix)
      sink()
      # plot confusion matrix
      # suppressWarnings(
      SRfunctions::SR_mosaicplot(var1 = factor(OOFforecast$y, levels = c("0", "1")),   # !!!
                                 var2 = factor(ifelse(OOFforecast$ensemble_best <= cut_off_a, 0, 1),
                                               levels = c("0", "1")))
      # )
      # suppressWarnings(SRfunctions::SR_mosaicplot(df = OOFforecast, cutoff = cut_off_a))
      ggplot2::ggsave(paste0(path_output, name, "/OOF Confusion Matrix.png"),
                      width = 9.92, height = 5.3)  # 4.67
    }
    #
    # print ROC-curve
    try({
      temp <- data.frame(tpr = ROC$sensitivities,
                         fpr = 1 - ROC$specificities)
      mcc <- mcc_(OOFforecast$ensemble_best, OOFforecast$y)
      p <- ggplot2::ggplot(temp, ggplot2::aes(x = fpr, y = tpr)) +
        ggplot2::geom_line() +
        ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray", size = 1,
                             linetype = "dashed") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(5),
                                    limits = c(0, 1)) +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5),
                                    limits = c(0, 1)) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_ba),
                            linetype = "dashed", colour = "turquoise3") +
        ggplot2::annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
                          label = paste0("cut off ba = ", cut_off_ba)) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_a),
                            linetype = "dashed", colour = "darkorchid1") +
        ggplot2::annotate("text", x = cut_off_a + 0.07, y = 0.05, colour = "darkorchid1",
                          label = paste0("cut off a = ", cut_off_a)) +
        ggplot2::labs(title = "OOF ROC",
                      x = "Rate of false positives", y = "Rate of true positives",
                      subtitle = paste0("AUC:                  ",
                                        round(as.numeric(ROC$auc), 3) %>%
                                          format(., nsmall = 3),
                                        "\nMCC:                  ",
                                        paste0(round(mcc$mcc, 3),
                                               " (cutoff = ",
                                               round(mcc$opt_cutoff, 3) %>%
                                                 format(., nsmall = 3), ")"),
                                        "\nAccuracy:           ",
                                        round(as.numeric(confusion_matrix$overall[1]), 3) %>%
                                          format(., nsmall = 3),
                                        "\nSensitivity/TPR:  ",
                                        round(as.numeric(confusion_matrix$byClass[1]), 3) %>%
                                          format(., nsmall = 3),
                                        "\nSpecificity/TNR:  ",
                                        round(as.numeric(confusion_matrix$byClass[2]), 3) %>%
                                          format(., nsmall = 3)))
      print(p)
      ggplot2::ggsave(paste0(path_output, name, "/OOF ROC.png"),
                      plot = p, width = 9.92, height = 5.3)  # 4.67
    })
    rm(temp)
    #
    # print class distribution (model-forecast)
    # relation <- sum(OOFforecast$y == 1) / sum(OOFforecast$y == 0)
    # if (relation < 0.2 | relation > 5) {
    ggplot2::ggplot(OOFforecast, ggplot2::aes(x = ensemble_best, fill = as.factor(y))) +
      ggplot2::geom_histogram(bins = 30) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_ba),
                          linetype = "dashed", colour = "turquoise3") +
      ggplot2::annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
                        label = paste0("cut off ba = ", cut_off_ba)) +
      ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_a),
                          linetype = "dashed", colour = "darkorchid1") +
      ggplot2::annotate("text", x = cut_off_a + 0.07, y = 0, colour = "darkorchid1",
                        label = paste0("cut off a = ", cut_off_a)) +
      ggplot2::labs(title = "OOF Class distributions", x = "Probability (y = 1)",
                    y = "Count", fill = "y") +
      ggplot2::facet_grid(y~., scales = "free_y")
    # } else {
    #   ggplot(OOFforecast, ggplot2::aes(x = ensemble_best, fill = as.factor(y))) +
    #     geom_histogram(bins = 30, alpha = 0.4, position="dodge") +
    #     geom_vline(ggplot2::aes(xintercept = cut_off_ba), linetype = "dashed", colour = "turquoise3") +
    #     annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
    #              label = paste0("cut off ba = ", cut_off_ba)) +
    #     geom_vline(ggplot2::aes(xintercept = cut_off_a), linetype = "dashed", colour = "darkorchid1") +
    #     annotate("text", x = cut_off_a + 0.07, y = 0, colour = "darkorchid1",
    #              label = paste0("cut off a = ", cut_off_a)) +
    #     labs(title = "Class distributions", x = "Probability (y = 1)", y = "Count", fill = "y")
    # }
    ggplot2::ggsave(paste0(path_output, name, "/OOF Class distributions.png"),
                    width = 9.92, height = 5.3)
    #
    # Lift Chart
    OOFforecast$group <- cut(OOFforecast$ensemble_best,
                             breaks = unique(stats::quantile(OOFforecast$ensemble_best,
                                                             probs = seq(0, 1.01, by = 1/20),
                                                             na.rm = TRUE)),
                             ordered_result = TRUE, dig.lab = 10,
                             include.lowest = TRUE, labels = FALSE)
    temp <- OOFforecast %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(Actual = mean(y), Predicted = mean(ensemble_best))
    OOFforecast$group <- NULL
    temp <- reshape2::melt(temp, id.vars = "group")
    ggplot2::ggplot(temp, ggplot2::aes(x = group, y = value, colour = variable)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
      ggplot2::labs(title = "OOF Lift Chart", y = "y", x = "Sorted Predictions", colour = "")
    ggplot2::ggsave(paste0(path_output, name, "/OOF Lift Chart.png"),
                    width = 9.92, height = 5.3)  # 4.67
    rm(temp)
    #
    #
    ## TEST
    #
    if (exists("y_test")) {
      TEST_metrics$AUC <- apply(TESTforecast[, 2:(ncol(TESTforecast) - 2)], 2,
                                function(x) as.numeric(pROC::roc(response = TESTforecast$y,
                                                                 predictor = x, algorithm = 2,
                                                                 levels = c(0, 1),
                                                                 direction = "<")$auc))
      TEST_metrics$MCC <- apply(TESTforecast[, 2:(ncol(TESTforecast) - 2)], 2,
                                function(x) mcc_(x, TESTforecast$y,
                                                 cutoff = mcc$opt_cutoff)$mcc)
      TEST_metrics$Accuracy <- apply(TESTforecast[, 2:(ncol(TESTforecast) - 2)], 2,
                                     function(x) caret::confusionMatrix(table(
                                       ifelse(x > cut_off_a, 1, 0), TESTforecast$y),
                                       positive = "1")$overall[1])
      TEST_metrics$Sensitivity_TPR <- apply(TESTforecast[, 2:(ncol(TESTforecast) - 2)], 2,
                                            function(x) caret::confusionMatrix(table(
                                              ifelse(x > cut_off_a, 1, 0), TESTforecast$y),
                                              positive = "1")$byClass[1])
      TEST_metrics <- TEST_metrics %>%
        dplyr::mutate(model = factor(model, levels = levels(OOF_metrics$model))) %>%
        dplyr::arrange(model)
      saveRDS(TEST_metrics, paste0(path_output, name, "/TEST_metrics.rds"))
      assign('TEST_metrics', TEST_metrics, envir = .GlobalEnv)
      #
      # plot comparison of model metrics
      p <- TEST_metrics %>%
        reshape2::melt(id = "model") %>%
        ggplot2::ggplot(ggplot2::aes(x = model, y = value, fill = variable)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::geom_text(ggplot2::aes(label = round(value, 3)),
                           position = ggplot2::position_stack(0.5)) +
        ggplot2::facet_wrap(variable~., scales = "free_x") +
        ggplot2::labs(x = "", y = "", title = "TEST Comparison of model metrics") +
        ggplot2::guides(fill = FALSE) +
        ggplot2::coord_flip()
      print(p)
      ggplot2::ggsave(paste0(path_output, name, "/TEST Comparison of model metrics.png"),
                      plot = p, width = 9.92, height = 5.3)  # 4.67
      rm(p)
      #
      # calculate ROC-curve
      ROC <- pROC::roc(response = TESTforecast$y,
                       predictor = TESTforecast$ensemble_best, algorithm = 2,
                       levels = c(0, 1), direction = "<")
      # ROC <- pROC::roc(response = TESTforecast$y,
      #                  predictor = TESTforecast$ensemble_best, algorithm = 2, quiet = TRUE)
      #
      # Accuracy and Cut off
      temp <- data.frame(threshold = ROC$thresholds,
                         true_positive_rate = ROC$sensitivities,
                         true_negative_rate = ROC$specificities)
      temp$balanced_accuracy <- (temp$true_positive_rate + temp$true_negative_rate) / 2
      temp$accuracy <-
        (temp$true_positive_rate * sum(TESTforecast$y == 1) +
           temp$true_negative_rate * sum(TESTforecast$y == 0)) /
        (sum(TESTforecast$y == 1) + sum(TESTforecast$y == 0))
      temp <- temp %>% dplyr::filter(!threshold %in% c(-Inf, Inf))   # remove -Inf, Inf thresholds NEW !!!
      #
      # save ROC data
      utils::write.table(temp, paste0(path_output, name, "/TEST Cut off, accuracy and costs.csv"),
                         row.names = FALSE, sep = ";")
      #
      # confusion matrix
      if (!cut_off_a %in% c(-Inf, Inf, 0, 1)) {
        confusion_matrix <-
          caret::confusionMatrix(table(ifelse(TESTforecast$ensemble_best > cut_off_a, 1, 0),
                                       TESTforecast$y),
                                 positive = "1")
        print(confusion_matrix)
        sink(paste0(path_output, name, "/TEST Confusion matrix.txt"), append = FALSE)
        print(confusion_matrix)
        sink()
        # plot confusion matrix
        # suppressWarnings(
        SRfunctions::SR_mosaicplot(var1 = factor(TESTforecast$y, levels = c("0", "1")),   # !!!
                                   var2 = factor(ifelse(TESTforecast$ensemble_best <= cut_off_a, 0, 1),
                                                 levels = c("0", "1")))
        # )
        # suppressWarnings(SRfunctions::SR_mosaicplot(df = TESTforecast, cutoff = cut_off_a))
        ggplot2::ggsave(paste0(path_output, name, "/TEST Confusion Matrix.png"),
                        width = 9.92, height = 5.3)  # 4.67
      }
      #
      # print ROC-curve
      try({
        temp <- data.frame(tpr = ROC$sensitivities,
                           fpr = 1 - ROC$specificities)
        mcc <- mcc_(TESTforecast$ensemble_best, TESTforecast$y, cutoff = mcc$opt_cutoff)
        p <- ggplot2::ggplot(temp, ggplot2::aes(x = fpr, y = tpr)) +
          ggplot2::geom_line() +
          ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray", size = 1,
                               linetype = "dashed") +
          ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(5),
                                      limits = c(0, 1)) +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5),
                                      limits = c(0, 1)) +
          ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_ba),
                              linetype = "dashed", colour = "turquoise3") +
          ggplot2::annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
                            label = paste0("cut off ba = ", cut_off_ba)) +
          ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_a),
                              linetype = "dashed", colour = "darkorchid1") +
          ggplot2::annotate("text", x = cut_off_a + 0.07, y = 0.05, colour = "darkorchid1",
                            label = paste0("cut off a = ", cut_off_a)) +
          ggplot2::labs(title = "ROC",
                        x = "Rate of false positives", y = "Rate of true positives",
                        subtitle = paste0("AUC:                  ",
                                          round(as.numeric(ROC$auc), 3) %>%
                                            format(., nsmall = 3),
                                          "\nMCC:                  ",
                                          paste0(round(mcc$mcc, 3),
                                                 " (cutoff = ",
                                                 round(mcc$opt_cutoff, 3) %>%
                                                   format(., nsmall = 3), ")"),
                                          "\nAccuracy:           ",
                                          round(as.numeric(confusion_matrix$overall[1]), 3) %>%
                                            format(., nsmall = 3),
                                          "\nSensitivity/TPR:  ",
                                          round(as.numeric(confusion_matrix$byClass[1]), 3) %>%
                                            format(., nsmall = 3),
                                          "\nSpecificity/TNR:  ",
                                          round(as.numeric(confusion_matrix$byClass[2]), 3) %>%
                                            format(., nsmall = 3)))
        print(p)
        ggplot2::ggsave(paste0(path_output, name, "/TEST ROC.png"),
                        plot = p, width = 9.92, height = 5.3)  # 4.67
      })
      rm(temp)
      #
      # print class distribution (model-forecast)
      # relation <- sum(TESTforecast$y == 1) / sum(TESTforecast$y == 0)
      # if (relation < 0.2 | relation > 5) {
      ggplot2::ggplot(TESTforecast, ggplot2::aes(x = ensemble_best, fill = as.factor(y))) +
        ggplot2::geom_histogram(bins = 30) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_ba),
                            linetype = "dashed", colour = "turquoise3") +
        ggplot2::annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
                          label = paste0("cut off ba = ", cut_off_ba)) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_a),
                            linetype = "dashed", colour = "darkorchid1") +
        ggplot2::annotate("text", x = cut_off_a + 0.07, y = 0, colour = "darkorchid1",
                          label = paste0("cut off a = ", cut_off_a)) +
        ggplot2::labs(title = "TEST Class distributions", x = "Probability (y = 1)",
                      y = "Count", fill = "y") +
        ggplot2::facet_grid(y~., scales = "free_y")
      # } else {
      #   ggplot(TESTforecast, ggplot2::aes(x = ensemble_best, fill = as.factor(y))) +
      #     geom_histogram(bins = 30, alpha = 0.4, position="dodge") +
      #     geom_vline(ggplot2::aes(xintercept = cut_off_ba), linetype = "dashed", colour = "turquoise3") +
      #     annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
      #              label = paste0("cut off ba = ", cut_off_ba)) +
      #     geom_vline(ggplot2::aes(xintercept = cut_off_a), linetype = "dashed", colour = "darkorchid1") +
      #     annotate("text", x = cut_off_a + 0.07, y = 0, colour = "darkorchid1",
      #              label = paste0("cut off a = ", cut_off_a)) +
      #     labs(title = "Class distributions", x = "Probability (y = 1)", y = "Count", fill = "y")
      # }
      ggplot2::ggsave(paste0(path_output, name, "/TEST Class distributions.png"),
                      width = 9.92, height = 5.3)
      #
      # Lift Chart
      TESTforecast$group <- cut(TESTforecast$ensemble_best,
                                breaks = unique(stats::quantile(TESTforecast$ensemble_best,
                                                                probs = seq(0, 1.01, by = 1/20),
                                                                na.rm = TRUE)),
                                ordered_result = TRUE, dig.lab = 10,
                                include.lowest = TRUE, labels = FALSE)
      temp <- TESTforecast %>%
        dplyr::group_by(group) %>%
        dplyr::summarise(Actual = mean(y), Predicted = mean(ensemble_best))
      TESTforecast$group <- NULL
      temp <- reshape2::melt(temp, id.vars = "group")
      ggplot2::ggplot(temp, ggplot2::aes(x = group, y = value, colour = variable)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
        ggplot2::labs(title = "TEST Lift Chart", y = "y", x = "Sorted Predictions", colour = "")
      ggplot2::ggsave(paste0(path_output, name, "/TEST Lift Chart.png"),
                      width = 9.92, height = 5.3)  # 4.67
      rm(temp)
    }
  } else if (objective == "multilabel") {
    ## OOF
    #
    # calculate metrics
    mAUC <- list()
    for (i in 1:nrow(OOF_metrics)) {
      # suppressMessages(
      mAUC[[i]] <- pROC::multiclass.roc(OOFforecast$y,
                                        OOFforecast[, (i * (classes + 1) - (classes - 1)):(i * (classes + 1))] %>%
                                          # OOFforecast[, (i * (classes + 1) - 5):(i * (classes + 1))] %>%
                                          stats::setNames(0:(classes - 1)))$auc %>%
        as.numeric()
      # )
    }; rm(i)
    OOF_metrics$mAUC <- unlist(mAUC)
    rm(mAUC)
    #
    OOFforecast_temp <- OOFforecast %>%
      dplyr::select(dplyr::contains("_class"), y) %>%
      dplyr::mutate(y = factor(y)) %>%
      dplyr::mutate_all(~factor(., levels = levels(y)))
    OOF_metrics$Accuracy <-
      apply(OOFforecast_temp[, 1:(ncol(OOFforecast_temp) - 1)], 2,
            function(x) caret::confusionMatrix(factor(x, levels = levels(OOFforecast_temp$y)),
                                               OOFforecast_temp$y)$overall[1])
    rm(OOFforecast_temp)
    #
    OOF_metrics <- OOF_metrics %>%
      dplyr::arrange(mAUC) %>%
      dplyr::mutate(model = factor(model, levels = .$model))
    saveRDS(OOF_metrics, paste0(path_output, name, "/OOF_metrics.rds"))
    assign('OOF_metrics', OOF_metrics, envir = .GlobalEnv)
    #
    # add column with best ensemble forecasts in OOF
    best_ensemble_OOF <- OOF_metrics %>%
      dplyr::arrange(-mAUC) %>%
      dplyr::filter(grepl("ensemble", model)) %>%
      dplyr::slice(1) %>%
      dplyr::pull(model) %>%
      as.character()
    OOFforecast <- OOFforecast %>%
      dplyr::bind_cols(
        OOFforecast %>%
          dplyr::select(dplyr::contains(best_ensemble_OOF)) %>%
          stats::setNames(paste0("ensemble_best",
                                 substr(names(.), nchar(names(.)) - 2, nchar(names(.))))) %>%
          dplyr::rename(ensemble_best_class = ensemble_bestass))
    saveRDS(OOFforecast, paste0(path_output, name, "/OOFforecast.rds"))
    assign('OOFforecast', OOFforecast, envir = .GlobalEnv)
    #
    # add column with best ensemble forecasts in TEST
    TESTforecast <- TESTforecast %>%
      dplyr::bind_cols(
        TESTforecast %>%
          dplyr::select(dplyr::contains(best_ensemble_OOF)) %>%
          stats::setNames(paste0("ensemble_best",
                                 substr(names(.), nchar(names(.)) - 2, nchar(names(.))))) %>%
          dplyr::rename(ensemble_best_class = ensemble_bestass))
    saveRDS(TESTforecast, paste0(path_output, name, "/TESTforecast.rds"))
    assign('TESTforecast', TESTforecast, envir = .GlobalEnv)
    #
    # plot comparison of model metrics
    p <- OOF_metrics %>%
      reshape2::melt(id = "model") %>%
      ggplot2::ggplot(ggplot2::aes(x = model, y = value, fill = variable)) +
      ggplot2::geom_bar(stat = "identity", position = "dodge") +
      ggplot2::geom_text(ggplot2::aes(label = round(value, 3)),
                         position = ggplot2::position_stack(0.5)) +
      ggplot2::facet_wrap(variable~., scales = "free_x") +
      ggplot2::labs(x = "", y = "", title = "OOF Comparison of model metrics") +
      ggplot2::guides(fill = FALSE) +
      ggplot2::coord_flip()
    print(p)
    ggplot2::ggsave(paste0(path_output, name, "/OOF Comparison of model metrics.png"),
                    plot = p, width = 9.92, height = 5.3)  # 4.67
    rm(p)
    #
    # calculate ROC-curve
    suppressMessages(
      ROC <- pROC::multiclass.roc(OOFforecast$y,
                                  OOFforecast$ensemble_best_class,
                                  levels = levels(factor(OOFforecast$y))) # direction = "<"
    )
    # additionally compute ROC-curve for "multi:softprob"
    # WARNING: AUC is better/correcter with "multi:softprob" !!!
    if (classes > 2) {
      suppressMessages(
        ROC_prob <- pROC::multiclass.roc(OOFforecast$y,
                                         OOFforecast %>%
                                           dplyr::select(dplyr::contains("ensemble_best_X")) %>%
                                           stats::setNames(0:(ncol(.) - 1)))
      )
    }
    #
    # confusion matrix
    confusion_matrix <- caret::confusionMatrix(factor(OOFforecast$ensemble_best_class,
                                                      levels = levels(factor(OOFforecast$y))),
                                               factor(OOFforecast$y))
    print(confusion_matrix)
    sink(paste0(path_output, name, "/OOF Confusion matrix.txt"), append = FALSE)
    print(confusion_matrix)
    print(confusion_matrix[["byClass"]])
    sink()
    # plot confusion matrix
    SRfunctions::SR_mosaicplot(var1 = factor(OOFforecast$y),
                               var2 = factor(OOFforecast$ensemble_best_class,
                                             levels = levels(factor(OOFforecast$y))))
    ggplot2::ggsave(paste0(path_output, name, "/OOF Confusion Matrix.png"),
                    width = 9.92, height = 5.3)  # 4.67
    #
    # calculate mcc
    # ROCR currently supports only evaluation of binary classification tasks !!!
    # mcc <- mcc(OOFforecast$ensemble_best, OOFforecast$y)
    #
    # get binary ROC curves
    ROC_bin <- data.frame()
    if (exists("ROC_prob")) {
      for (i in 1:length(ROC_prob[['rocs']])) {
        suppressMessages({
          ROC_bin <- dplyr::bind_rows(ROC_bin,
                                      data.frame(binary_ROC = toString(ROC_prob[['rocs']][[i]][[1]][["levels"]]),
                                                 binary_AUC = as.numeric(pROC::auc(ROC_prob[["rocs"]][[i]][[1]][["response"]],
                                                                                   ROC_prob[["rocs"]][[i]][[1]][["predictor"]])),
                                                 # binary_AUC = as.numeric(pROC::auc(ROC_prob[["rocs"]][[i]][[1]][["response"]],
                                                 #                                   ifelse(ROC_prob[["rocs"]][[i]][[1]][["predictor"]] > 0.5, 1, 0))),
                                                 thresholds = ROC_prob[['rocs']][[i]][[1]][["thresholds"]],
                                                 tpr = ROC_prob[['rocs']][[i]][[1]][["sensitivities"]],
                                                 fpr = 1 - ROC_prob[['rocs']][[i]][[1]][["specificities"]],
                                                 no = length(ROC_prob[['rocs']][[i]][[1]][["response"]]),
                                                 no_y_1 = length(ROC_prob[['rocs']][[i]][[1]][["controls"]]),
                                                 stringsAsFactors = FALSE))
        })
      }
    } else {
      # WARNING: depends on ROC with "multi:softmax" which is worse than "multi:softprob"
      for (i in 1:length(ROC[['rocs']])) {
        suppressMessages({
          ROC_bin <- dplyr::bind_rows(ROC_bin,
                                      data.frame(binary_ROC = toString(ROC[['rocs']][[i]][["levels"]]),
                                                 binary_AUC = as.numeric(pROC::auc(ROC[["rocs"]][[i]][["response"]],
                                                                                   ROC[["rocs"]][[i]][["predictor"]])), # direction = "<" ???
                                                 thresholds = ROC[['rocs']][[i]][["thresholds"]],
                                                 tpr = ROC[['rocs']][[i]][["sensitivities"]],
                                                 fpr = 1 - ROC[['rocs']][[i]][["specificities"]],
                                                 no = length(ROC[['rocs']][[i]][["response"]]),
                                                 no_y_1 = length(ROC[['rocs']][[i]][["controls"]]),
                                                 stringsAsFactors = FALSE))
        })
      }
    }; rm(i)
    ROC_bin <- ROC_bin %>%
      dplyr::mutate(binary = paste0(binary_ROC,
                                    " (AUC = ", round(binary_AUC, 2) %>% format(., nsmall = 2),
                                    ", data = ", no, ")"))
    #
    # print ROC-curve
    try({
      p <- ggplot2::ggplot(ROC_bin, ggplot2::aes(x = fpr, y = tpr,
                                                 colour = stats::reorder(binary, -binary_AUC))) +
        ggplot2::geom_line() +
        ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray", size = 1,
                             linetype = "dashed") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(5),
                                    limits = c(0, 1)) +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5),
                                    limits = c(0, 1)) +
        ggplot2::labs(title = "OOF ROC", colour = "Binary AUC",
                      x = "Rate of false positives", y = "Rate of true positives",
                      subtitle = paste0("AUC:                  ",
                                        round(ifelse(exists("ROC_prob"),
                                                     as.numeric(ROC_prob$auc),
                                                     as.numeric(ROC$auc)), 3) %>%
                                          format(., nsmall = 3),
                                        # "\nMCC:                  ",
                                        # paste0(round(mcc$mcc, 3),
                                        #        " (cutoff = ",
                                        #        round(mcc$opt_cutoff, 3), ")"),
                                        "\nAccuracy:           ",
                                        round(as.numeric(confusion_matrix$overall[1]), 3) %>%
                                          format(., nsmall = 3),
                                        "\nPrecision:           ",
                                        toString(round(as.numeric(confusion_matrix$byClass[, 5]), 2) %>%
                                                   format(., nsmall = 2)),
                                        "\nSensitivity/TPR:  ",
                                        toString(round(as.numeric(confusion_matrix$byClass[, 1]), 2) %>%
                                                   format(., nsmall = 2)),
                                        "\nSpecificity/TNR:  ",
                                        toString(round(as.numeric(confusion_matrix$byClass[, 2]), 2) %>%
                                                   format(., nsmall = 2)))) +
        ggplot2::theme(legend.text = ggplot2::element_text(size = 6))
      print(p)
      ggplot2::ggsave(paste0(path_output, name, "/OOF ROC.png"),
                      plot = p, width = 9.92, height = 5.3)  # 4.67
    })
    #
    # Lift Chart
    # add baseline of random group
    set.seed(12345)
    OOFforecast <- OOFforecast %>%
      dplyr::mutate(random = sample(OOFforecast$y, nrow(OOFforecast), replace = FALSE),
                    model_correct = dplyr::if_else(y == ensemble_best_class, 1, 0),
                    baseline_correct = dplyr::if_else(y == random, 1, 0))
    set.seed(Sys.time())
    #
    temp <- dplyr::full_join(
      # calculate precision for model
      OOFforecast %>%
        dplyr::count(y, model_correct) %>%
        dplyr::group_by(y) %>%
        dplyr::mutate(model_precision = n / sum(n)) %>%
        dplyr::filter(model_correct == 1) %>%
        dplyr::select(y, model = model_precision),
      # calculate precision for baseline
      OOFforecast %>%
        dplyr::count(y, baseline_correct) %>%
        dplyr::group_by(y) %>%
        dplyr::mutate(baseline_precision = n / sum(n)) %>%
        dplyr::filter(baseline_correct == 1) %>%
        dplyr::select(y, baseline = baseline_precision),
      by = "y") %>%
      dplyr::ungroup() %>%
      dplyr::mutate_at(dplyr::vars(model, baseline), ~tidyr::replace_na(., 0)) %>%
      dplyr::mutate(y = as.character(y),
                    lift_factor = dplyr::if_else(baseline != 0, model / baseline, 0))
    #
    OOFforecast <- OOFforecast %>% select(-random, -model_correct, -baseline_correct)
    temp %>%
      dplyr::select(-lift_factor) %>%
      reshape2::melt(id.vars = "y") %>%
      ggplot2::ggplot(ggplot2::aes(x = y, y = value, colour = variable, group = variable)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::annotate(geom = "text", x = temp$y, y = temp$model,
                        label = paste0("lift =\n", round(temp$lift_factor, 1) %>%
                                         format(., nsmall = 1)),
                        hjust = "center", vjust = "top", size = 3) +
      # ggplot2::geom_label(data = temp,
      #                     ggplot2::aes(x = y, y = model, label = round(lift_factor, 1)))
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6), limits = c(0, 1)) +
      ggplot2::labs(title = "OOF Lift Chart", y = "Precision", x = "Class", colour = "")
    ggplot2::ggsave(paste0(path_output, name, "/OOF Lift Chart.png"),
                    width = 9.92, height = 5.3)  # 4.67
    rm(temp)
    #
    #
    ## TEST
    #
    if (exists("y_test")) {
      # calculate metrics
      mAUC <- list()
      for (i in 1:(nrow(OOF_metrics))) {
        # suppressMessages(
        mAUC[[i]] <- pROC::multiclass.roc(TESTforecast$y,
                                          TESTforecast[, (i * (classes + 1) - (classes - 1)):(i * (classes + 1))] %>%
                                          # TESTforecast[, (i * (classes + 1) - 5):(i * (classes + 1))] %>%
                                            stats::setNames(0:(classes - 1)))$auc %>%
          as.numeric()
        # )
      }; rm(i)
      TEST_metrics$mAUC <- unlist(mAUC)
      rm(mAUC)
      #
      TESTforecast_temp <- TESTforecast %>%
        dplyr::select(dplyr::contains("_class"), y) %>%
        dplyr::mutate(y = factor(y)) %>%
        dplyr::mutate_all(~factor(., levels = levels(y)))
      TEST_metrics$Accuracy <-
        apply(TESTforecast_temp[, 1:(ncol(TESTforecast_temp) - 2)], 2,
              function(x) caret::confusionMatrix(factor(x, levels = levels(TESTforecast_temp$y)),
                                                 TESTforecast_temp$y)$overall[1])
      rm(TESTforecast_temp)
      #
      TEST_metrics <- TEST_metrics %>%
        dplyr::mutate(model = factor(model, levels = levels(OOF_metrics$model))) %>%
        dplyr::arrange(model)
      saveRDS(TEST_metrics, paste0(path_output, name, "/TEST_metrics.rds"))
      assign('TEST_metrics', TEST_metrics, envir = .GlobalEnv)
      #
      # plot comparison of model metrics
      p <- TEST_metrics %>%
        reshape2::melt(id = "model") %>%
        ggplot2::ggplot(ggplot2::aes(x = model, y = value, fill = variable)) +
        ggplot2::geom_bar(stat = "identity", position = "dodge") +
        ggplot2::geom_text(ggplot2::aes(label = round(value, 3)),
                           position = ggplot2::position_stack(0.5)) +
        ggplot2::facet_wrap(variable~., scales = "free_x") +
        ggplot2::labs(x = "", y = "", title = "TEST Comparison of model metrics") +
        ggplot2::guides(fill = FALSE) +
        ggplot2::coord_flip()
      print(p)
      ggplot2::ggsave(paste0(path_output, name, "/TEST Comparison of model metrics.png"),
                      plot = p, width = 9.92, height = 5.3)  # 4.67
      rm(p)
      #
      # calculate ROC-curve
      suppressMessages(
        ROC <- pROC::multiclass.roc(TESTforecast$y,
                                    TESTforecast$ensemble_best_class,
                                    levels = levels(factor(TESTforecast$y))) # direction = "<"
      )
      # additionally compute ROC-curve for "multi:softprob"
      # WARNING: AUC is better/correcter with "multi:softprob" !!!
      if (classes > 2) {
        suppressMessages(
          ROC_prob <- pROC::multiclass.roc(TESTforecast$y,
                                           TESTforecast %>%
                                             dplyr::select(dplyr::contains("ensemble_best_X")) %>%
                                             stats::setNames(0:(ncol(.) - 1)))
        )
      }
      #
      # confusion matrix
      confusion_matrix <- caret::confusionMatrix(factor(TESTforecast$ensemble_best_class,
                                                        levels = levels(factor(TESTforecast$y))),
                                                 factor(TESTforecast$y))
      print(confusion_matrix)
      sink(paste0(path_output, name, "/TEST Confusion matrix.txt"), append = FALSE)
      print(confusion_matrix)
      print(confusion_matrix[["byClass"]])
      sink()
      # plot confusion matrix
      SRfunctions::SR_mosaicplot(var1 = factor(TESTforecast$y),
                                 var2 = factor(TESTforecast$ensemble_best_class,
                                               levels = levels(factor(TESTforecast$y))))
      ggplot2::ggsave(paste0(path_output, name, "/TEST Confusion Matrix.png"),
                      width = 9.92, height = 5.3)  # 4.67
      #
      # calculate mcc
      # ROCR currently supports only evaluation of binary classification tasks !!!
      # mcc <- mcc(TESTforecast$ensemble_best, TESTforecast$y)
      #
      # get binary ROC curves
      ROC_bin <- data.frame()
      if (exists("ROC_prob")) {
        for (i in 1:length(ROC_prob[['rocs']])) {
          suppressMessages({
            ROC_bin <- dplyr::bind_rows(ROC_bin,
                                        data.frame(binary_ROC = toString(ROC_prob[['rocs']][[i]][[1]][["levels"]]),
                                                   binary_AUC = as.numeric(pROC::auc(ROC_prob[["rocs"]][[i]][[1]][["response"]],
                                                                                     ROC_prob[["rocs"]][[i]][[1]][["predictor"]])),
                                                   # binary_AUC = as.numeric(pROC::auc(ROC_prob[["rocs"]][[i]][[1]][["response"]],
                                                   #                                   ifelse(ROC_prob[["rocs"]][[i]][[1]][["predictor"]] > 0.5, 1, 0))),
                                                   thresholds = ROC_prob[['rocs']][[i]][[1]][["thresholds"]],
                                                   tpr = ROC_prob[['rocs']][[i]][[1]][["sensitivities"]],
                                                   fpr = 1 - ROC_prob[['rocs']][[i]][[1]][["specificities"]],
                                                   no = length(ROC_prob[['rocs']][[i]][[1]][["response"]]),
                                                   no_y_1 = length(ROC_prob[['rocs']][[i]][[1]][["controls"]]),
                                                   stringsAsFactors = FALSE))
          })
        }
      } else {
        # WARNING: depends on ROC with "multi:softmax" which is worse than "multi:softprob"
        for (i in 1:length(ROC[['rocs']])) {
          suppressMessages({
            ROC_bin <- dplyr::bind_rows(ROC_bin,
                                        data.frame(binary_ROC = toString(ROC[['rocs']][[i]][["levels"]]),
                                                   binary_AUC = as.numeric(pROC::auc(ROC[["rocs"]][[i]][["response"]],
                                                                                     ROC[["rocs"]][[i]][["predictor"]])), # direction = "<" ???
                                                   thresholds = ROC[['rocs']][[i]][["thresholds"]],
                                                   tpr = ROC[['rocs']][[i]][["sensitivities"]],
                                                   fpr = 1 - ROC[['rocs']][[i]][["specificities"]],
                                                   no = length(ROC[['rocs']][[i]][["response"]]),
                                                   no_y_1 = length(ROC[['rocs']][[i]][["controls"]]),
                                                   stringsAsFactors = FALSE))
          })
        }
      }; rm(i)
      ROC_bin <- ROC_bin %>%
        dplyr::mutate(binary = paste0(binary_ROC,
                                      " (AUC = ", round(binary_AUC, 2) %>% format(., nsmall = 2),
                                      ", data = ", no, ")"))
      #
      # print ROC-curve
      try({
        p <- ggplot2::ggplot(ROC_bin, ggplot2::aes(x = fpr, y = tpr,
                                                   colour = stats::reorder(binary, -binary_AUC))) +
          ggplot2::geom_line() +
          ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray", size = 1,
                               linetype = "dashed") +
          ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(5),
                                      limits = c(0, 1)) +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5),
                                      limits = c(0, 1)) +
          ggplot2::labs(title = "TEST ROC", colour = "Binary AUC",
                        x = "Rate of false positives", y = "Rate of true positives",
                        subtitle = paste0("AUC:                  ",
                                          round(ifelse(exists("ROC_prob"),
                                                       as.numeric(ROC_prob$auc),
                                                       as.numeric(ROC$auc)), 3) %>%
                                            format(., nsmall = 3),
                                          # "\nMCC:                  ",
                                          # paste0(round(mcc$mcc, 3),
                                          #        " (cutoff = ",
                                          #        round(mcc$opt_cutoff, 3), ")"),
                                          "\nAccuracy:           ",
                                          round(as.numeric(confusion_matrix$overall[1]), 3) %>%
                                            format(., nsmall = 3),
                                          "\nPrecision:           ",
                                          toString(round(as.numeric(confusion_matrix$byClass[, 5]), 2) %>%
                                                     format(., nsmall = 2)),
                                          "\nSensitivity/TPR:  ",
                                          toString(round(as.numeric(confusion_matrix$byClass[, 1]), 2) %>%
                                                     format(., nsmall = 2)),
                                          "\nSpecificity/TNR:  ",
                                          toString(round(as.numeric(confusion_matrix$byClass[, 2]), 2) %>%
                                                     format(., nsmall = 2)))) +
          ggplot2::theme(legend.text = ggplot2::element_text(size = 6))
        print(p)
        ggplot2::ggsave(paste0(path_output, name, "/TEST ROC.png"),
                        plot = p, width = 9.92, height = 5.3)  # 4.67
      })
      #
      # Lift Chart
      # add baseline of random group
      set.seed(12345)
      TESTforecast <- TESTforecast %>%
        dplyr::mutate(random = sample(TESTforecast$y, nrow(TESTforecast), replace = FALSE),
                      model_correct = dplyr::if_else(y == ensemble_best_class, 1, 0),
                      baseline_correct = dplyr::if_else(y == random, 1, 0))
      set.seed(Sys.time())
      #
      temp <- dplyr::full_join(
        # calculate precision for model
        TESTforecast %>%
          dplyr::count(y, model_correct) %>%
          dplyr::group_by(y) %>%
          dplyr::mutate(model_precision = n / sum(n)) %>%
          dplyr::filter(model_correct == 1) %>%
          dplyr::select(y, model = model_precision),
        # calculate precision for baseline
        TESTforecast %>%
          dplyr::count(y, baseline_correct) %>%
          dplyr::group_by(y) %>%
          dplyr::mutate(baseline_precision = n / sum(n)) %>%
          dplyr::filter(baseline_correct == 1) %>%
          dplyr::select(y, baseline = baseline_precision),
        by = "y") %>%
        dplyr::ungroup() %>%
        dplyr::mutate_at(dplyr::vars(model, baseline), ~tidyr::replace_na(., 0)) %>%
        dplyr::mutate(y = as.character(y),
                      lift_factor = dplyr::if_else(baseline != 0, model / baseline, 0))
      #
      TESTforecast <- TESTforecast %>% select(-random, -model_correct, -baseline_correct)
      temp %>%
        dplyr::select(-lift_factor) %>%
        reshape2::melt(id.vars = "y") %>%
        ggplot2::ggplot(ggplot2::aes(x = y, y = value, colour = variable, group = variable)) +
        ggplot2::geom_point() +
        ggplot2::geom_line() +
        ggplot2::annotate(geom = "text", x = temp$y, y = temp$model,
                          label = paste0("lift =\n", round(temp$lift_factor, 1) %>%
                                           format(., nsmall = 1)),
                          hjust = "center", vjust = "top", size = 3) +
        # ggplot2::geom_label(data = temp,
        #                     ggplot2::aes(x = y, y = model, label = round(lift_factor, 1)))
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6), limits = c(0, 1)) +
        ggplot2::labs(title = "TEST Lift Chart", y = "Precision", x = "Class", colour = "")
      ggplot2::ggsave(paste0(path_output, name, "/TEST Lift Chart.png"),
                      width = 9.92, height = 5.3)  # 4.67
      rm(temp)
    }
  }
  #
  # return NULL
  return(invisible(NULL))
}

