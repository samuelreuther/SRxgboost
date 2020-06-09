#' SRxgboost_plots
#'
#' Plots model results for a selected model.
#'
#' @param lauf character
#' @param rank integer
#' @param plots boolean
#' @param min_rel_Gain numeric
#' @param sample integer
#' @param pdp_sample integer
#' @param pdp_cuts integer
#' @param pdp_parallel boolean
#' @param n_core character
#' @param silent boolean
#'
#' @return several files in folder
#'
#' @export
SRxgboost_plots <- function(lauf, rank = 1,
                            plots = TRUE,
                            min_rel_Gain = 0.01,
                            sample = 100000, pdp_sample = 20000, pdp_cuts = 50,
                            pdp_parallel = FALSE, n_core = "max",
                            silent = FALSE) {
  #
  # delete old plots
  if (plots & dir.exists(paste0(path_output, gsub(".csv", "/", lauf), "Best Model/"))) {
    unlink(paste0(path_output, gsub(".csv", "/", lauf), "Best Model/*"),
           recursive = TRUE)
  }
  #
  # create XGB_plots folder
  dir.create(paste0(path_output, gsub(".csv", "/", lauf), "Best Model/"),
             showWarnings = FALSE)
  #
  # Get summary
  SRxgboost_get_summary_CV(lauf)
  assign('SummaryCV', SummaryCV, envir = .GlobalEnv)
  if (!silent) print(head(SummaryCV[!is.na(SummaryCV$eval_1fold), c(1:12)]))
  #
  # Load model
  if (!silent) print(paste0("Loading model: ", SummaryCV$date[rank]))
  modelpath <- paste0(path_output, paste0(gsub(".csv", "/", lauf), "All Models/",
                                          gsub(":", ".", SummaryCV$date[rank]),
                                          ".model"))
  if (file.exists(modelpath) |
      file.exists(gsub(".model", "_1fold.model", modelpath, fixed = TRUE))) {
    if (file.exists(modelpath)) {
      bst <- xgboost::xgb.load(modelpath)
      assign('bst', bst, envir = .GlobalEnv)
    } else {
      bst <- xgboost::xgb.load(gsub(".model", "_1fold.model", modelpath, fixed = TRUE))
    }
    bst_1fold <- xgboost::xgb.load(gsub(".model", "_1fold.model", modelpath, fixed = TRUE))
    assign('bst_1fold', bst_1fold, envir = .GlobalEnv)
    try(file.copy(modelpath,
                  paste0(path_output, gsub(".csv", "/", lauf),
                         "Best Model/model"),
                  overwrite = TRUE), TRUE)
    try(file.copy(gsub(".model", ".model.rds", modelpath),
                  paste0(path_output, gsub(".csv", "/", lauf),
                         "Best Model/model.rds"),
                  overwrite = TRUE), TRUE)
    file.copy(gsub(".model", "_1fold.model", modelpath),
              paste0(path_output, gsub(".csv", "/", lauf),
                     "Best Model/1fold.model"),
              overwrite = TRUE)
    file.copy(gsub(".model", "_1fold.model.rds", modelpath),
              paste0(path_output, gsub(".csv", "/", lauf),
                     "Best Model/1fold.model.rds"),
              overwrite = TRUE)
    file.copy(gsub(".model", "_Error_rate.png", modelpath),
              paste0(path_output, gsub(".csv", "/", lauf),
                     "Best Model/Error_rate.png"),
              overwrite = TRUE)
    file.copy(gsub(".model", "_Evaluation_log.rds", modelpath),
              paste0(path_output, gsub(".csv", "/", lauf),
                     "Best Model/Evaluation_log.rds"),
              overwrite = TRUE)
    try({
      file.copy(gsub(".model", "_Shap_train_eval.rds", modelpath),
                paste0(path_output, gsub(".csv", "/", lauf),
                       "Best Model/Shap_train_eval.rds"),
                overwrite = TRUE)
      file.copy(gsub(".model", "_Shap_prediction.rds", modelpath),
                paste0(path_output, gsub(".csv", "/", lauf),
                       "Best Model/Shap_prediction.rds"),
                overwrite = TRUE)
      file.copy(gsub(".model", "_Shap_datenModell_eval.rds", modelpath),
                paste0(path_output, gsub(".csv", "/", lauf),
                       "Best Model/Shap_datenModell_eval.rds"),
                overwrite = TRUE)
      file.copy(gsub(".model", "_Shap_plot.png", modelpath),
                paste0(path_output, gsub(".csv", "/", lauf),
                       "Best Model/Shap_plot.png"),
                overwrite = TRUE)
    }, TRUE)
  } else {
    # re-run best model     TODO!!!
    # bst <- xgboost(data = d_train, objective = "multi:softprob", num_class = length(unique(y)),
    #                metric = "mlogloss", verbose = 0, max.depth = SummaryCV$depth,
    #                eta = SummaryCV$eta, nround = SummaryCV$nround,
    #                min_child_weight = SummaryCV$min_child_weight, gamma = SummaryCV$gamma,
    #                subsample = SummaryCV$subsample, colsample_bytree = SummaryCV$colsample_bytree)
  }
  rm(modelpath)
  #
  # get pr_oof
  OOFforecast <- readRDS(paste0(path_output, gsub(".csv", "/", lauf), "Data/OOFforecast.rds"))
  i_OOFforecast <- stringdist::stringdistmatrix(
    gsub("-",".",gsub(":",".",SummaryCV$date)),
    gsub("X","",colnames(OOFforecast)[2:ncol(OOFforecast)]))[rank, ]
  i_OOFforecast <- which.min(i_OOFforecast); as.character(SummaryCV$date[rank])
  colnames(OOFforecast[i_OOFforecast + 1])
  pr <- OOFforecast[, i_OOFforecast + 1]
  if (length(y) == length(pr)) {
    train_pr_oof <- as.data.frame(cbind(y, pr))
  } else {
    train_pr_oof <- as.data.frame(cbind(y = y[eval_index], pr))
  }
  assign('train_pr_oof', train_pr_oof, envir = .GlobalEnv)
  #
  # get pr_test
  if (file.exists(paste0(path_output, gsub(".csv", "/", lauf), "Data/y_test.rds"))) {
    TESTforecast <- readRDS(paste0(path_output, gsub(".csv", "/", lauf),
                                   "Data/TESTforecast.rds"))
    y_test <- readRDS(paste0(path_output, gsub(".csv", "/", lauf), "Data/y_test.rds"))
    pr_test <- TESTforecast[, i_OOFforecast + 1]
    test_pr <- as.data.frame(cbind(y_test, pr_test))
    assign('test_pr', test_pr, envir = .GlobalEnv)
  }
  #
  #
  #
  ### generate graphics
  if (plots) {
    #
    ### downsample train_pr_oof if nrow > sample   # quite different results, e.g. optimal cutoff value! error at XGBFI_PDP1!
    if (nrow(train_pr_oof) > sample) {
      set.seed(12345)
      train_pr_oof <- train_pr_oof %>% dplyr::sample_n(sample)
      set.seed(Sys.time())
    }
    #
    ### Plot y vs. model prediction
    if (SRfunctions::SR_is_number(y) & length(unique(y)) > 2) {
      #
      # avoid negative values if min(y) >= 0
      if (min(y, na.rm = TRUE) >=  0) {   # train_pr_oof$y
        train_pr_oof$pr <- dplyr::case_when(is.na(train_pr_oof$pr) ~ NA_real_,
                                            train_pr_oof$pr < 0    ~ 0,
                                            TRUE                   ~ train_pr_oof$pr)
      }
      #
      # regression
      ggplot2::ggplot(train_pr_oof, ggplot2::aes(x = y, y = pr)) +
        ggplot2::geom_point() +
        ggplot2::geom_abline(intercept = 0, slope = 1,
                             colour = "red", linetype = "dashed") +
        ggplot2::labs(y = "model prediction", title = "y vs. model prediction",
                      subtitle = paste0("RMSE:    ", round(Metrics::rmse(train_pr_oof$y,
                                                                         train_pr_oof$pr), 3),
                                        "\nMAE:      ",
                                        round(Metrics::mae(train_pr_oof$y,
                                                           train_pr_oof$pr), 3),
                                        "\nMAPE:    ",
                                        round(sum(abs(train_pr_oof$pr /
                                                        train_pr_oof$y - 1)) /
                                                length(train_pr_oof$y), 3),
                                        "\nR2:          ",
                                        format(stats::cor(train_pr_oof$y,
                                                          train_pr_oof$pr)^2,
                                               digits = 3))) # ,
      # "\nAUC:       ", round(pROC::auc(train_pr_oof$y,
      #                                  train_pr_oof$pr), 3)))
      ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
                             "Best Model/Accuracy y vs. model prediction.png"),
                      width = 9.92, height = 5.3)  # 4.67
    } else {
      #
      # calculate ROC-curve
      ROC <- pROC::roc(response = train_pr_oof$y,
                       predictor = train_pr_oof$pr, algorithm = 2,
                       levels = c(0, 1), direction = "<")
      # ROC <- pROC::roc(response = train_pr_oof$y,
      #                  predictor = train_pr_oof$pr, algorithm = 2, quiet = TRUE)
      #
      # Accuracy and Cut off
      temp <- data.frame(threshold = ROC$thresholds,
                         true_positive_rate = ROC$sensitivities,
                         true_negative_rate = ROC$specificities)
      temp$balanced_accuracy <- (temp$true_positive_rate + temp$true_negative_rate) / 2
      temp$accuracy <-
        (temp$true_positive_rate * sum(train_pr_oof$y == 1) +
           temp$true_negative_rate * sum(train_pr_oof$y == 0)) /
        (sum(train_pr_oof$y == 1) + sum(train_pr_oof$y == 0))
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
        ggplot2::labs(title = "Accuracy and cut off", x = "Threshold",
                      y = "Accuracy", colour = "") +
        ggplot2::theme(legend.position = "top")
      ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
                             "Best Model/Accuracy and Cut off.png"),
                      width = 9.92, height = 5.3)
      #
      # Cut off and Costs
      temp$costs_1_to_1 <-
        temp$true_positive_rate * sum(train_pr_oof$y == 1) * 1 +
        temp$true_negative_rate * sum(train_pr_oof$y == 0) * 1
      temp$costs_1_to_3 <-
        temp$true_positive_rate * sum(train_pr_oof$y == 1) * 1 +
        temp$true_negative_rate * sum(train_pr_oof$y == 0) * 3
      temp$costs_3_to_1 <-
        temp$true_positive_rate * sum(train_pr_oof$y == 1) * 3 +
        temp$true_negative_rate * sum(train_pr_oof$y == 0) * 1
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
        ggplot2::labs(title = "Cut off and costs", x = "Threshold",
                      y = "Costs", colour = "") +
        ggplot2::theme(legend.position = "top")
      ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
                             "Best Model/Cut off and Costs.png"),
                      width = 9.92, height = 5.3)
      #
      # save ROC data
      write.table(temp, paste0(path_output, gsub(".csv", "/", lauf),
                               "Best Model/Cut off, accuracy and costs.csv"),
                  row.names = FALSE, sep = ";")
      #
      # confusion matrix
      if (!cut_off_a %in% c(-Inf, Inf, 0, 1)) {
        confusion_matrix <-
          caret::confusionMatrix(table(ifelse(train_pr_oof$pr > cut_off_a, 1, 0),
                                       train_pr_oof$y),
                                 positive = "1")
        print(confusion_matrix)
        sink(paste0(path_output, gsub(".csv", "/", lauf),
                    "Best Model/Confusion matrix.txt"), append = FALSE)
        print(confusion_matrix)
        sink()
        # plot confusion matrix
        # suppressWarnings(
        SRfunctions::SR_mosaicplot(var1 = factor(train_pr_oof$y, levels = c("0", "1")),   # !!!
                                   var2 = factor(ifelse(train_pr_oof$pr <= cut_off_a, 0, 1),
                                                 levels = c("0", "1")))
        # )
        # suppressWarnings(SRfunctions::SR_mosaicplot(df = train_pr_oof, cutoff = cut_off_a))
        ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
                               "Best Model/Confusion Matrix.png"),
                        width = 9.92, height = 5.3)  # 4.67
      }
      #
      # define mcc function
      mcc <- function(pred, labels) {
        temp <- data.frame(pred = pred, labels = labels)
        if (length(pred) > 100000) {   # NAs produced by integer overflow > 500'000
          set.seed(12345)
          temp <- temp %>% sample_n(100000)
          set.seed(Sys.time())
        }
        prediction <- ROCR::prediction(temp$pred, temp$labels)
        mcc <- ROCR::performance(prediction, "mat")
        err <- max(mcc@y.values[[1]], na.rm = TRUE)
        opt_cutoff = mcc@x.values[[1]][which.max(mcc@y.values[[1]])]
        return(list(mcc = err, opt_cutoff = opt_cutoff))
      }
      #
      # print ROC-curve
      try({
        temp <- data.frame(tpr = ROC$sensitivities,
                           fpr = 1 - ROC$specificities)
        mcc <- mcc(train_pr_oof$pr, train_pr_oof$y)
        ggplot2::ggplot(temp, ggplot2::aes(x = fpr, y = tpr)) +
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
                                          round(as.numeric(ROC$auc), 3),
                                          "\nMCC:                  ",
                                          paste0(round(mcc$mcc, 3),
                                                 " (cutoff = ",
                                                 round(mcc$opt_cutoff, 3), ")"),
                                          "\nAccuracy:           ",
                                          round(as.numeric(confusion_matrix$overall[1]), 3),
                                          "\nSensitivity/TPR:  ",
                                          round(as.numeric(confusion_matrix$byClass[1]), 3),
                                          "\nSpecificity/TNR:  ",
                                          round(as.numeric(confusion_matrix$byClass[2]), 3)))
        ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf), "Best Model/ROC.png"),
                        width = 9.92, height = 5.3)  # 4.67
      })
      rm(temp)
      #
      # print class distribution (model-forecast)
      # relation <- sum(train_pr_oof$y == 1) / sum(train_pr_oof$y == 0)
      # if (relation < 0.2 | relation > 5) {
      ggplot2::ggplot(train_pr_oof, ggplot2::aes(x = pr, fill = as.factor(y))) +
        ggplot2::geom_histogram(bins = 30) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_ba),
                            linetype = "dashed", colour = "turquoise3") +
        ggplot2::annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
                          label = paste0("cut off ba = ", cut_off_ba)) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_a),
                            linetype = "dashed", colour = "darkorchid1") +
        ggplot2::annotate("text", x = cut_off_a + 0.07, y = 0, colour = "darkorchid1",
                          label = paste0("cut off a = ", cut_off_a)) +
        ggplot2::labs(title = "Class distributions", x = "Probability (y = 1)",
                      y = "Count", fill = "y") +
        ggplot2::facet_grid(y~., scales = "free_y")
      # } else {
      #   ggplot(train_pr_oof, ggplot2::aes(x = pr, fill = as.factor(y))) +
      #     geom_histogram(bins = 30, alpha = 0.4, position="dodge") +
      #     geom_vline(ggplot2::aes(xintercept = cut_off_ba), linetype = "dashed", colour = "turquoise3") +
      #     annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
      #              label = paste0("cut off ba = ", cut_off_ba)) +
      #     geom_vline(ggplot2::aes(xintercept = cut_off_a), linetype = "dashed", colour = "darkorchid1") +
      #     annotate("text", x = cut_off_a + 0.07, y = 0, colour = "darkorchid1",
      #              label = paste0("cut off a = ", cut_off_a)) +
      #     labs(title = "Class distributions", x = "Probability (y = 1)", y = "Count", fill = "y")
      # }
      ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
                             "Best Model/Class distributions.png"),
                      width = 9.92, height = 5.3)
    }
    #
    ### Residual drift
    if (exists("test_pr")) {
      train_temp <- train_pr_oof %>%
        dplyr::mutate(residuals = y - pr)
      test_temp <- test_pr %>%
        dplyr::mutate(residuals = y_test - pr_test) %>%
        dplyr::rename(y = y_test, pr = pr_test)
      drift_residual <- SRxgboost_calculate_drift(train_temp, test_temp, n_plots = 10,
                                                  name = paste0(path_output,
                                                                gsub(".csv", "", lauf),
                                                                "/Best Model/"))
    }
    #
    ### Lift Chart
    train_pr_oof$group <- cut(train_pr_oof$pr,
                              breaks = unique(stats::quantile(train_pr_oof$pr,
                                                              probs = seq(0, 1.01, by = 1/20),
                                                              na.rm = TRUE)),
                              ordered_result = TRUE, dig.lab = 10,
                              include.lowest = TRUE, labels = FALSE)
    temp <- train_pr_oof %>%
      dplyr::group_by(group) %>%
      dplyr::summarise(Actual = mean(y), Predicted = mean(pr))
    train_pr_oof$group <- NULL
    temp <- reshape2::melt(temp, id.vars = "group")
    ggplot2::ggplot(temp, ggplot2::aes(x = group, y = value, colour = variable)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
      ggplot2::labs(title = "Lift Chart", y = "y", x = "Sorted Prediction", colour = "")
    ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
                           "Best Model/Lift Chart.png"),
                    width = 9.92, height = 5.3)  # 4.67
    rm(temp)
    #
    ### 1way graphics
    # variable importance
    importance_matrix <- xgboost::xgb.importance(colnames(train_mat), model = bst)
    importance_matrix <- importance_matrix %>%
      dplyr::select(Feature, Gain, Frequency)  %>%
      dplyr::arrange(-Gain) %>%
      dplyr::mutate(Feature = factor(Feature,
                                     levels = rev(stats::reorder(importance_matrix$Feature,
                                                                 importance_matrix$Gain))))
    # plot
    importance_matrix %>%
      dplyr::slice(1:(min(nrow(importance_matrix), 30))) %>%
      reshape2::melt(id.vars = "Feature") %>%
      ggplot2::ggplot(ggplot2::aes(y = value, x = Feature)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::labs(x = "Variable", y = "") +
      ggplot2::scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(5)) +
      ggplot2::facet_grid(~variable, scales = "free") +
      ggplot2::coord_flip()
    ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
                           "Best Model/0 Variable importance.png"),
                    width = 9.92, height = 5.3)  # 4.67
    # save table
    assign('importance_matrix', importance_matrix, envir = .GlobalEnv)
    write.table(importance_matrix,
                paste0(path_output, gsub(".csv", "/", lauf),
                       "Best Model/0 Variable importance.csv"),
                row.names = FALSE, col.names = TRUE, append = FALSE, sep = ";", dec = ",")
    #
    # Partial Dependence Plots
    temp <- importance_matrix %>%
      dplyr::filter(Gain >= min_rel_Gain) %>%
      dplyr::mutate(Feature = as.character(Feature))
    #
    if (nrow(temp) > 1) {
      # run dpd::partial in parallel
      if (nrow(datenModell_eval) > 1000 & pdp_parallel) {
        if (n_core == "max") n_core <- parallel::detectCores() - 1 # min(parallel::detectCores() - 1, 6)
        cl <- parallel::makeCluster(n_core)
        doParallel::registerDoParallel(cl)
        # parallel::clusterEvalQ(cl, library(stats)) # ?
      }
      #
      for (i in 1:nrow(temp)) {
        try({
          # print progress
          print(paste0("Plot ", i, " of ", nrow(temp), ": ", temp[i, 1]))
          #
          # downsample datenModell if nrow > pdp_sample
          if (nrow(datenModell_eval) > pdp_sample) {
            set.seed(12345)
            datenModell_eval_ <- datenModell_eval %>% dplyr::sample_n(pdp_sample)
            set.seed(12345)
            y_ <- data.frame(y = y_test_eval) %>% dplyr::sample_n(pdp_sample)
            test_eval_mat_ <- Matrix::sparse.model.matrix(~. - 1, data = datenModell_eval_)
            pr_ <- data.frame(pr = stats::predict(bst_1fold, test_eval_mat_))
          } else {
            datenModell_eval_ <- datenModell_eval
            y_ <- data.frame(y = y_test_eval)
            test_eval_mat_ <- test_eval_mat
            pr_ <- data.frame(pr = stats::predict(bst_1fold, test_eval_mat))
          }
          #
          # if (y_multiclass == FALSE) {   # linear, binary
          x <- min(stringdist::stringdist(temp$Feature[i], names(datenModell_eval_)))
          #
          if (x > 0) {       # x is factor
            x <- names(datenModell_eval_)[which.min(stringdist::stringdist(
              temp$Feature[i], names(datenModell_eval_), method = "lcs"))]
            xlabel <- x
            xlabel2 <- gsub(x, "", temp$Feature[i])
            stats <- data.frame(x = datenModell_eval_[, xlabel] == xlabel2,
                                Actual = y_$y, Predicted = pr_$pr) %>%
              dplyr::group_by(x) %>%
              dplyr::summarise(Count = n(),
                               Predicted = mean(Predicted),
                               Actual = mean(Actual))
            stats <- stats %>%
              dplyr::left_join(partial,
                               by = c("x_orig" = xlabel)) %>%
              dplyr::rename("Partial_Dependence" = "yhat")
            stats_long <- reshape2::melt(stats[, c(3, 5:7)], id.vars = "x_orig")
            xlabel <- paste0(x, " = ", xlabel2)
            p1 <- ggplot2::ggplot(stats_long,
                                  ggplot2::aes(x = x, y = value, colour = variable)) +
              ggplot2::geom_point(size = 2) +
              ggplot2::labs(x = xlabel, y = "Value") +
              ggplot2::theme(legend.position = "top",
                             legend.title = ggplot2::element_blank()); p1
            p2 <- ggplot2::ggplot(stats, ggplot2::aes(x = x, y = Count)) +
              ggplot2::geom_bar(stat = "identity", position = "dodge") +
              ggplot2::labs(x = xlabel); p2
            p <- grid.arrange(p1, p2, ncol = 1, heights = c(0.75, 0.25)); p
          } else {
            # x is numeric, integer or Date
            xlabel <- temp$Feature[i]
            # check if x = as.numeric(date)
            if (sum(is.na(as.Date(as.character(datenModell_eval_[, xlabel]),
                                  format = "%Y%m%d"))) == 0) {
              xx <- as.Date(as.character(datenModell_eval_[, xlabel]), format = "%Y%m%d")
            } else {
              xx <- datenModell_eval_[, xlabel]
            }
            # check if x = as.numeric(factor), set number of cuts
            if (gsub("_LabelEnc", "", temp$Feature[i]) %in% factor_encoding$feature) {
              xx <- factor(xx)
              levels(xx) <- factor_encoding$levels[factor_encoding$feature ==
                                                     gsub("_LabelEnc", "", temp$Feature[i])]
              # cuts <- length(unique(xx))
              # summarise results
              stats <- data.frame(x = xx, x_orig = datenModell_eval_[, xlabel],
                                  Actual = y_$y, Predicted = pr_$pr) %>%
                dplyr::mutate(Group = xx) %>%
                dplyr::group_by(Group) %>%
                dplyr::summarise(x = x[1],
                                 x_orig = mean(x_orig),
                                 Count = n(),
                                 Predicted = mean(Predicted),
                                 Actual = mean(Actual))
            } else {
              # summarise results
              stats <- data.frame(x = xx, x_orig = datenModell_eval_[, xlabel],
                                  Actual = y_$y, Predicted = pr_$pr) %>%
                dplyr::mutate(Group = cut(x, breaks = pretty(x, pdp_cuts),
                                          include.lowest = TRUE, dig.lab = 10)) %>%
                # mutate(Group = cut(x_orig, breaks = unique(quantile(x_orig, seq(0, 1.01, 0.02))),
                #                    include.lowest = TRUE, dig.lab = 10)) %>%
                dplyr::group_by(Group) %>%
                dplyr::summarise(x = mean(x),
                                 x_orig = mean(x_orig),
                                 Count = n(),
                                 Predicted = mean(Predicted),
                                 Actual = mean(Actual))
            }
            #
            ## partial dependence
            #
            if (pdp_parallel) {
              # in parallel
              partial <- pdp::partial(bst_1fold,
                                      pred.var = temp$Feature[i],
                                      pred.grid = data.frame(stats$x_orig) %>%
                                        stats::setNames(xlabel),
                                      # grid.resolution = 30,
                                      train = test_eval_mat_,
                                      plot = FALSE, chull = TRUE, type = "regression",
                                      parallel = TRUE, paropts = list(.packages = "xgboost"))
            } else {
              # single core
              partial <- pdp::partial(bst_1fold,
                                      pred.var = temp$Feature[i],
                                      pred.grid = data.frame(stats$x_orig) %>%
                                        stats::setNames(xlabel),
                                      # grid.resolution = 30,
                                      train = test_eval_mat_,
                                      plot = FALSE, chull = TRUE, type = "regression")
            }
            #
            stats <- stats %>%
              dplyr::left_join(partial, by = c("x_orig" = xlabel)) %>%
              dplyr::rename("Partial_Dependence" = "yhat")
            #
            # evtl. noch skalieren !!!
            #
            stats_long <- reshape2::melt(stats %>% dplyr::select(x, 5:7), id.vars = "x")
            #
            p1 <- ggplot2::ggplot(stats_long,
                                  ggplot2::aes(x = x, y = value, colour = variable)) +
              ggplot2::geom_point(size = 2) +
              ggplot2::labs(x = "", y = "Value") +
              # ggplot2::labs(x = gsub("_LabelEnc", "", xlabel), y = "Value") +
              ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
              ggplot2::theme(legend.position = "top",
                             legend.title = ggplot2::element_blank())
            if ((gsub("_LabelEnc", "",  temp$Feature[i]) %in% factor_encoding$feature)) {
              # if (cuts > 8) {                                              # factor
              #   p1 <- p1 + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
              # }
            } else if (SRfunctions::SR_is_date(stats_long$x)) {                            # date
              spanne = as.numeric(difftime(max(stats_long$x), min(stats_long$x),
                                           units = "days"))
              if (spanne > 365.25 * 20) {
                p1 <- p1 +
                  ggplot2::scale_x_date(date_breaks = "10 years",
                                        date_labels = "%Y",
                                        date_minor_breaks = "1 year")
              } else if (spanne > 365.25 * 3) {
                p1 <- p1 +
                  ggplot2::scale_x_date(date_breaks = "year",
                                        date_labels = "%Y",
                                        date_minor_breaks = "3 months")
              } else if (spanne > 365.25) {
                p1 <- p1 +
                  ggplot2::scale_x_date(date_breaks = "3 months",
                                        date_labels = "%Y-%m",
                                        date_minor_breaks = "1 month")
              } else if (spanne > 30) {
                p1 <- p1 +
                  ggplot2::scale_x_date(date_breaks = "1 month",
                                        date_labels = "%Y-%m-%d",
                                        date_minor_breaks = "1 week")
              } else {
                p1 <- p1 +
                  ggplot2::scale_x_date(date_breaks = "1 week",
                                        date_labels = "%Y-%m-%d",
                                        date_minor_breaks = "1 day")
              }
            } else {
              p1 <- p1 +
                ggplot2::geom_line(size = I(1)) +                            # numeric
                ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8))
            }; p1
            p2 <- ggplot2::ggplot(stats, ggplot2::aes(x = x, y = Count)) +
              ggplot2::geom_bar(stat = "identity", position = "dodge", orientation = "x") +
              ggplot2::labs(x = gsub("_LabelEnc", "", xlabel))
            if ((gsub("_LabelEnc", "", temp$Feature[i]) %in% factor_encoding$feature)) {
              # if (cuts > 8) {                                              # factor
              #   p1 <- p1 + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3))
              # }
            } else if (SRfunctions::SR_is_date(stats_long$x)) {                             # date
              if (spanne > 365.25 * 20) {
                p2 <- p2 +
                  ggplot2::scale_x_date(date_breaks = "10 years",
                                        date_labels = "%Y",
                                        date_minor_breaks = "1 year")
              } else if (spanne > 365.25 * 3) {
                p2 <- p2 +
                  ggplot2::scale_x_date(date_breaks = "year",
                                        date_labels = "%Y",
                                        date_minor_breaks = "3 months")
              } else if (spanne > 365.25) {
                p2 <- p2 +
                  ggplot2::scale_x_date(date_breaks = "3 months",
                                        date_labels = "%Y-%m",
                                        date_minor_breaks = "1 month")
              } else if (spanne > 30) {
                p2 <- p2 +
                  ggplot2::scale_x_date(date_breaks = "1 month",
                                        date_labels = "%Y-%m-%d",
                                        date_minor_breaks = "1 week")
              } else {
                p2 <- p2 +
                  ggplot2::scale_x_date(date_breaks = "1 week",
                                        date_labels = "%Y-%m-%d",
                                        date_minor_breaks = "1 day")
              }
            } else {
              p2 <- p2 +
                ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8))       # numeric
            }; p2
            p <- gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(0.75, 0.25)); p
          }
          # save graphic
          xlabel <- gsub("[[:punct:]]", "", xlabel)
          try(ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf), "Best Model/", i,
                                     " ", gsub("LabelEnc", "", xlabel), ".png"),
                              plot = p, width = 9.92, height = 5.3))  # 4.67
          try(rm(p), TRUE)
          # save summary table
          write.table(stats, paste0(path_output, gsub(".csv", "/", lauf),
                                    "Best Model/", i, " ", gsub("LabelEnc", "",
                                                                xlabel), ".csv"),
                      row.names = FALSE, sep = ";")
        })
      }
      #
      # stop cluster for pdp::partial
      if (pdp_parallel) {
        try(parallel::stopCluster(cl), TRUE)
        try(parallel::stopCluster(cl), TRUE)
        rm(cl, n_core); invisible(gc())
      }
    }
  }
}
