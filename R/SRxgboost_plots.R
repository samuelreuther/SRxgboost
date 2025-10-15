#' SRxgboost_plots
#'
#' Plots model results for a selected model.
#'
#' @param lauf character
#' @param rank integer, default = 1
#' @param plots boolean, default = TRUE
#' @param sample integer, default = 100000
#' @param silent boolean, default = FALSE
#' @param pdp_plots boolean, default = TRUE
#' @param pdp_min_rel_Gain numeric, default =  0.01
#' @param pdp_sample integer, default = 50000
#' @param pdp_cuts integer, default = NULL, depending on number of data points
#'                 (max. 50 but in average at least pdp_int_cuts_sample per cut)
#' @param pdp_int_sample integer, default = 30000
#' @param pdp_int_cuts_sample integer, default = 50
#' @param pdp_parallel boolean, default = TRUE
#'
#' @return several files in folder
#'
#' @export
SRxgboost_plots <- function(lauf, rank = 1, plots = TRUE, silent = FALSE,
                            uncertainty_quantil = NULL,
                            sample = 100000, pdp_sample = 50000, pdp_int_sample = 30000,
                            pdp_plots = TRUE, pdp_parallel = TRUE, pdp_min_rel_Gain = 0.01,
                            pdp_cuts = NULL, pdp_int_cuts_sample = 50) {
  # Initialisation ####
  #
  # turn off scientific notation
  options(scipen = 999)
  #
  # check lauf ends with ".csv"
  if (!grepl('.csv$', lauf)) lauf <- paste0(lauf, ".csv")
  #
  # set paths
  path_output_best <- paste0(path_output, gsub(".csv", "/", lauf), "Best Model/")
  path_output_model <- paste0(path_output, gsub(".csv", "/", lauf), "All Models/")
  path_output_data <- paste0(path_output, gsub(".csv", "/", lauf), "Data/")
  #
  # delete old plots
  if (plots & dir.exists(path_output_best)) {
    files_to_keep <- c("Uncertainty_stats.rds", "Uncertainty.png")
    all_files <- list.files(path_output_best, full.names = TRUE)
    files_to_delete <- all_files[!basename(all_files) %in% files_to_keep]
    file.remove(files_to_delete)
    # unlink(path_output_best, recursive = TRUE)
    # unlink(paste0(path_output, gsub(".csv", "/", lauf), "Best Model/*"),
    #        recursive = TRUE)
  }
  #
  # create XGB_plots folder
  dir.create(path_output_best, showWarnings = FALSE)
  #
  # Get summary
  SRxgboost_get_summary_CV(lauf)
  assign('SummaryCV', SummaryCV, envir = .GlobalEnv)
  if (!silent) print(utils::head(SummaryCV[!is.na(SummaryCV$eval_1fold), c(1:12)]))
  #
  # Load model
  if (!silent) print(paste0("Loading model: ", SummaryCV$date[rank]))
  modelpath <- paste0(path_output_model, gsub(":", ".", SummaryCV$date[rank]), ".model")
  if (file.exists(modelpath) |
      file.exists(gsub(".model", "_1fold.model", modelpath, fixed = TRUE))) {
    if (file.exists(modelpath)) {
      bst <- xgboost::xgb.load(modelpath)
      bst_ <- readRDS(gsub(".model", ".model.rds", modelpath, fixed = TRUE))
      assign('bst', bst, envir = .GlobalEnv)
    } else {
      bst <- xgboost::xgb.load(gsub(".model", "_1fold.model", modelpath, fixed = TRUE))
    }
    bst_1fold <- xgboost::xgb.load(gsub(".model", "_1fold.model", modelpath, fixed = TRUE))
    assign('bst_1fold', bst_1fold, envir = .GlobalEnv)
    try(file.copy(modelpath,
                  paste0(path_output_best, "model"), overwrite = TRUE), TRUE)
    try(file.copy(gsub(".model", ".model.rds", modelpath),
                  paste0(path_output_best, "model.rds"), overwrite = TRUE), TRUE)
    file.copy(gsub(".model", "_1fold.model", modelpath),
              paste0(path_output_best, "1fold.model"), overwrite = TRUE)
    file.copy(gsub(".model", "_1fold.model.rds", modelpath),
              paste0(path_output_best, "1fold.model.rds"), overwrite = TRUE)
    file.copy(gsub(".model", "_Error_rate.png", modelpath),
              paste0(path_output_best, "Error_rate.png"), overwrite = TRUE)
    file.copy(gsub(".model", "_Evaluation_log.rds", modelpath),
              paste0(path_output_best, "Evaluation_log.rds"), overwrite = TRUE)
    try({
      file.copy(gsub(".model", "_Shap_train_eval.rds", modelpath),
                paste0(path_output_best, "Shap_train_eval.rds"), overwrite = TRUE)
      file.copy(gsub(".model", "_Shap_prediction.rds", modelpath),
                paste0(path_output_best, "Shap_prediction.rds"), overwrite = TRUE)
      file.copy(gsub(".model", "_Shap_datenModell_eval.rds", modelpath),
                paste0(path_output_best, "Shap_datenModell_eval.rds"), overwrite = TRUE)
      file.copy(gsub(".model", "_Shap_index_test_eval.rds", modelpath),
                paste0(path_output_best, "Shap_index_test_eval.rds"), overwrite = TRUE)
      file.copy(gsub(".model", "_Shap_plot.png", modelpath),
                paste0(path_output_best, "Shap_plot.png"), overwrite = TRUE)
    }, TRUE)
  } else {
    # re-run best model     TODO !!!
    # bst <- xgboost(data = d_train, objective = "multi:softprob", num_class = length(unique(y)),
    #                metric = "mlogloss", verbose = 0, max.depth = SummaryCV$depth,
    #                eta = SummaryCV$eta, nround = SummaryCV$nround,
    #                min_child_weight = SummaryCV$min_child_weight, gamma = SummaryCV$gamma,
    #                subsample = SummaryCV$subsample, colsample_bytree = SummaryCV$colsample_bytree)
  }
  rm(modelpath)
  #
  #
  #
  # Get OOFforecast and TESTforecast ####
  #
  SRxgboost_get_OOFforecast_TESTforecast(lauf = lauf, top_rank = 1, ensemble = FALSE)
  if (length(y) == nrow(OOFforecast)) {
    train_pr_oof <- as.data.frame(cbind(y, pr = OOFforecast[, ncol(OOFforecast)]))
  } else {
    train_pr_oof <- as.data.frame(cbind(y = y[eval_index],
                                        pr = OOFforecast[, ncol(OOFforecast)]))
  }
  # for "multi:softprob": add probabilities
  if (objective == "multilabel" & ncol(OOFforecast) > 2) {
    train_pr_oof <- dplyr::bind_cols(train_pr_oof,
                                     OOFforecast[, 2:(ncol(OOFforecast) - 1)] %>%
                                       stats::setNames(paste0("X", 0:(ncol(.) - 1))))
  }
  assign('train_pr_oof', train_pr_oof, envir = .GlobalEnv)
  if (file.exists(paste0(path_output_data, "y_test.rds"))) {
    y_test <- readRDS(paste0(path_output_data, "y_test.rds"))
    test_pr <- as.data.frame(cbind(y_test,
                                   pr_test = TESTforecast[, ncol(TESTforecast)]))
    assign('test_pr', test_pr, envir = .GlobalEnv)
  }
  # # get pr_oof
  # OOFforecast <- readRDS(paste0(path_output_data, "OOFforecast.rds"))
  # i_OOFforecast <- stringdist::stringdistmatrix(
  #   gsub("-",".",gsub(":",".",SummaryCV$date)),
  #   gsub("X","",colnames(OOFforecast)[2:ncol(OOFforecast)]))[rank, ]
  # i_OOFforecast <- which.min(i_OOFforecast); as.character(SummaryCV$date[rank])
  # colnames(OOFforecast[i_OOFforecast + 1])
  # pr <- OOFforecast[, i_OOFforecast + 1]
  # if (length(y) == length(pr)) {
  #   train_pr_oof <- as.data.frame(cbind(y, pr))
  # } else {
  #   train_pr_oof <- as.data.frame(cbind(y = y[eval_index], pr))
  # }
  # assign('train_pr_oof', train_pr_oof, envir = .GlobalEnv)
  # #
  # # get pr_test
  # if (file.exists(paste0(path_output_data, "y_test.rds"))) {
  #   TESTforecast <- readRDS(paste0(path_output_data, "TESTforecast.rds"))
  #   y_test <- readRDS(paste0(path_output_data, "y_test.rds"))
  #   pr_test <- TESTforecast[, i_OOFforecast + 1]
  #   test_pr <- as.data.frame(cbind(y_test, pr_test))
  #   assign('test_pr', test_pr, envir = .GlobalEnv)
  # }
  #
  #
  #
  # Remove uncertain forecasts ####
  #
  datenModell_rows <- nrow(datenModell)
  #
  if (!is.null(uncertainty_quantil)) {
    # always rerun analysis, because folder gets deleted => files do not get deleted anymore
    # SRxgboost_check_uncertainty(lauf = lauf)
    #
    # which predictions to keep
    uncertainty <- uncertainty %>%
      dplyr::mutate(KEEP = ifelse(UNCERTAINTY <= uncertainty_stats$UNCERTAINTY[
        uncertainty_stats$QUANTIL == uncertainty_quantil], TRUE, FALSE))
    #
    # clean up all objects
    #
    id_unique_train_keep <- uncertainty$KEEP[match(id_unique_train, uncertainty$id)]
    train_pr_oof <- train_pr_oof[id_unique_train_keep, ]
    datenModell <- datenModell[id_unique_train_keep, ]
    if (exists("test_pr")) test_pr <- test_pr[id_unique_train_keep, ]
    #
    index_test_eval_keep <- uncertainty$KEEP[match(index_test_eval, uncertainty$id)]
    datenModell_eval <- datenModell_eval[index_test_eval_keep, ]
    test_eval_mat <- test_eval_mat[index_test_eval_keep, ]
    y_test_eval <- y_test_eval[index_test_eval_keep]
    #
    OOFforecast <- OOFforecast[id_unique_train_keep, ]
    if (exists("id_unique_test")) {
      id_unique_test_keep <- uncertainty$KEEP[match(id_unique_test, uncertainty$id)]
      TESTforecast <- TESTforecast[id_unique_test_keep, ]
    } else {
      TESTforecast <- TESTforecast[id_unique_train_keep, ]
    }
    y <- y[id_unique_train_keep]
    #
    index_train_eval_keep <- uncertainty$KEEP[match(index_train_eval, uncertainty$id)]
    y_train_eval <- y_train_eval[index_train_eval_keep]
    #
    # save into Data
    path_temp <- paste0(path_output_data, "Uncertainty",
                        sub("0.", "", format(uncertainty_quantil, nsmall = 2)), "/")
    if (!dir.exists(path_temp)) dir.create(path_temp, showWarnings = FALSE)
    saveRDS(id_unique_train_keep, paste0(path_temp, "id_unique_train_keep.rds"))
    saveRDS(datenModell, paste0(path_temp, "datenModell.rds"))
    if (exists("test_pr")) saveRDS(test_pr, paste0(path_temp, "test_pr.rds"))
    saveRDS(index_test_eval_keep, paste0(path_temp, "index_test_eval_keep.rds"))
    saveRDS(datenModell_eval, paste0(path_temp, "datenModell_eval.rds"))
    saveRDS(test_eval_mat, paste0(path_temp, "test_eval_mat.rds"))
    saveRDS(y_test_eval, paste0(path_temp, "y_test_eval.rds"))
    if (exists("id_unique_test_keep"))
      saveRDS(id_unique_test_keep, paste0(path_temp, "id_unique_test_keep.rds"))
    saveRDS(OOFforecast, paste0(path_temp, "OOFforecast.rds"))
    saveRDS(TESTforecast, paste0(path_temp, "TESTforecast.rds"))
    saveRDS(index_train_eval_keep, paste0(path_temp, "index_train_eval_keep.rds"))
    saveRDS(y_train_eval, paste0(path_temp, "y_train_eval.rds"))
  }
  #
  #
  #
  # Generate graphics ####
  #
  if (plots) {
    ### downsample train_pr_oof if nrow > sample
    # quite different results, e.g. optimal cutoff value! error at XGBFI_PDP1!
    #
    if (nrow(train_pr_oof) > sample) {
      set.seed(12345)
      train_pr_oof <- train_pr_oof %>% dplyr::sample_n(sample)
      set.seed(Sys.time())
    }
    #
    # define mcc function
    # ROCR currently supports only evaluation of binary classification tasks !!!
    mcc <- function(pred, labels) {
      temp <- data.frame(pred = pred, labels = labels)
      if (length(pred) > 100000) {   # NAs produced by integer overflow > 500'000
        set.seed(12345)
        temp <- temp %>% dplyr::sample_n(100000)
        set.seed(Sys.time())
      }
      prediction <- ROCR::prediction(temp$pred, temp$labels)
      mcc <- ROCR::performance(prediction, "mat")
      err <- max(mcc@y.values[[1]], na.rm = TRUE)
      opt_cutoff = mcc@x.values[[1]][which.max(mcc@y.values[[1]])]
      return(list(mcc = err, opt_cutoff = opt_cutoff))
    }
    prAUC <- function(pred, labels) {
      # https://stats.stackexchange.com/questions/10501/calculating-aupr-in-r
      # library(PRROC)
      fg <- pred[labels == 1]
      bg <- pred[labels == 0]
      pr <- PRROC::pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
      prauc <- pr$auc.integral
      # plot(pr)
      #
      # yardstick::pr_auc(preds, truth = diabetes, .pred_pos)$.estimate
      #
      # MLmetrics::PRAUC(preds$.pred_pos, preds$diabetes)
      #
      # get precision and recall, but how to calculate auc correctly???
      # ROC <- pROC::roc(response = labels,
      #                  predictor = pred, algorithm = 2,
      #                  levels = c(0, 1), direction = "<") %>%
      #   pROC::coords(ret = "all", transpose = FALSE)
      #
      # p_load(PerfMeas)
      # => should be faster, but problems with installation and documentation
      #
      return(list(metric="prAUC", value = prauc))
    }
    # clean up foreach/dopar between runs
    unregister_dopar <- function() {
      env <- foreach:::.foreachGlobals
      rm(list = ls(name = env), pos = env)
    }
    #
    ## setup parallelisation cluster ####
    if (pdp_parallel) {
      unregister_dopar()
      if (exists("n_cores")) {
        pdp_n_core <- n_cores
      } else {
        pdp_n_core <- parallel::detectCores() - 1 # min(parallel::detectCores() - 1, 6)
      }
      cl <- parallel::makeCluster(pdp_n_core)
      doParallel::registerDoParallel(cl)
      # parallel::clusterEvalQ(cl, library(stats)) # ?
    }
    #
    #
    #
    ## Plot y vs. model prediction ####
    #
    if (objective == "regression") {   # SRfunctions::SR_is_number(y) & length(unique(y)) > 2
      # avoid negative values if min(y) >= 0
      if (min(y, na.rm = TRUE) >=  0) {   # train_pr_oof$y
        train_pr_oof$pr <- dplyr::case_when(is.na(train_pr_oof$pr) ~ NA_real_,
                                            train_pr_oof$pr < 0    ~ 0,
                                            TRUE                   ~ train_pr_oof$pr)
      }
      #
      # plot
      ggplot2::ggplot(train_pr_oof, ggplot2::aes(x = y, y = pr)) +
        ggplot2::geom_point(shape = 4, alpha = 0.5) +
        ggplot2::geom_abline(intercept = 0, slope = 1,
                             colour = "red", linetype = "dashed") +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6),
                                    labels = scales::format_format(big.mark = "'")) +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6),
                                    labels = scales::format_format(big.mark = "'")) +
        ggplot2::labs(y = "model prediction", title = "y vs. model prediction",
                      subtitle = paste0("RMSE:    ", round(Metrics::rmse(train_pr_oof$y,
                                                                         train_pr_oof$pr), 3) %>%
                                          format(., nsmall = 3),
                                        "\nMAE:      ",
                                        round(Metrics::mae(train_pr_oof$y,
                                                           train_pr_oof$pr), 3) %>%
                                          format(., nsmall = 3),
                                        "\nMAPE:    ",
                                        round(sum(abs(train_pr_oof$pr /
                                                        train_pr_oof$y - 1)) /
                                                length(train_pr_oof$y), 3) %>%
                                          format(., nsmall = 3),
                                        "\nR2:          ",
                                        round(stats::cor(train_pr_oof$y,
                                                         train_pr_oof$pr)^2, 3) %>%
                                          format(., nsmall = 3))) # ,
      # "\nAUC:       ", round(pROC::auc(train_pr_oof$y,
      #                                  train_pr_oof$pr), 3)))
      ggplot2::ggsave(paste0(path_output_best, "Accuracy y vs. model prediction.png"),
                      width = 9.92, height = 5.3)
    } else if (objective == "classification") {
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
      ggplot2::ggsave(paste0(path_output_best, "Accuracy and Cut off.png"),
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
      ggplot2::ggsave(paste0(path_output_best, "Cut off and Costs.png"),
                      width = 9.92, height = 5.3)
      #
      # save ROC data
      utils::write.table(temp, paste0(path_output_best, "Cut off, accuracy and costs.csv"),
                         row.names = FALSE, sep = ";")
      #
      # confusion matrix
      if (!cut_off_a %in% c(-Inf, Inf, 0, 1)) {
        confusion_matrix <-
          caret::confusionMatrix(table(ifelse(train_pr_oof$pr > cut_off_a, 1, 0),
                                       train_pr_oof$y),
                                 positive = "1")
        print(confusion_matrix)
        sink(paste0(path_output_best, "Confusion matrix.txt"), append = FALSE)
        print(confusion_matrix)
        sink()
        # plot confusion matrix
        # suppressWarnings(
        SRfunctions::SR_mosaicplot(var1 = factor(train_pr_oof$y, levels = c("0", "1")),   # !!!
                                   var2 = factor(ifelse(train_pr_oof$pr <= cut_off_a, 0, 1),
                                                 levels = c("0", "1")))
        # )
        # suppressWarnings(SRfunctions::SR_mosaicplot(df = train_pr_oof, cutoff = cut_off_a))
        ggplot2::ggsave(paste0(path_output_best, "Confusion Matrix.png"),
                        width = 9.92, height = 5.3)
      }
      #
      # print ROC-curve
      try({
        temp <- data.frame(tpr = ROC$sensitivities,
                           fpr = 1 - ROC$specificities)
        mcc <- mcc(train_pr_oof$pr, train_pr_oof$y)
        prauc <- prAUC(pred = train_pr_oof$pr, labels = train_pr_oof$y)$value
        prauc_benchmark <- as.numeric(prop.table(table(train_pr_oof$y))[2])
        ggplot2::ggplot(temp, ggplot2::aes(x = fpr, y = tpr)) +
          ggplot2::geom_line() +
          ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray", linewidth = 1,
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
                                          " / AUC PR: ",
                                          format(round(prauc, 3), nsmall = 3),
                                          " (benchmark = ",
                                          format(round(prauc_benchmark, 3),
                                                 nsmall = 3), ") ",
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
        ggplot2::ggsave(paste0(path_output_best, "ROC.png"), width = 9.92, height = 5.3)
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
      ggplot2::ggsave(paste0(path_output_best, "Class distributions.png"),
                      width = 9.92, height = 5.3)
    } else {
      # "multilabel"
      # https://www.datascienceblog.net/post/machine-learning/performance-measures-multi-class-problems/
      #
      # calculate ROC-curve
      suppressMessages(
        ROC <- pROC::multiclass.roc(train_pr_oof$y,
                                    train_pr_oof$pr,
                                    levels = levels(factor(train_pr_oof$y))) # direction = "<"
      )
      # additionally compute ROC-curve for "multi:softprob"
      # WARNING: AUC is better/correcter with "multi:softprob" !!!
      if (ncol(train_pr_oof) > 2) {
        suppressMessages(
          ROC_prob <- pROC::multiclass.roc(train_pr_oof$y,
                                           train_pr_oof[, 3:ncol(train_pr_oof)] %>%
                                             stats::setNames(0:(ncol(.) - 1)))
        )
      }
      #
      # calculate ROC-curve binary (NOT USED FOR NOW)
      # auc_bin <- data.frame()
      # for (i in levels(factor(train_pr_oof$y))) {
      #   auc <- pROC::multiclass.roc(dplyr::if_else(train_pr_oof$y == i, 1, 0),
      #                               dplyr::if_else(train_pr_oof$pr == i, 1, 0))$auc %>%  # direction = "<"
      #     as.numeric()
      #   auc_bin <- dplyr::bind_rows(auc_bin, data.frame(auc = auc))
      #   rm(auc)
      # }; rm(i)
      #
      # confusion matrix
      confusion_matrix <- caret::confusionMatrix(factor(train_pr_oof$pr,
                                                        levels = levels(factor(train_pr_oof$y))),
                                                 factor(train_pr_oof$y))
      print(confusion_matrix)
      sink(paste0(path_output_best, "Confusion matrix.txt"), append = FALSE)
      print(confusion_matrix)
      print(confusion_matrix[["byClass"]])
      sink()
      # plot confusion matrix
      SRfunctions::SR_mosaicplot(var1 = factor(train_pr_oof$y),
                                 var2 = factor(train_pr_oof$pr,
                                               levels = levels(factor(train_pr_oof$y))))
      ggplot2::ggsave(paste0(path_output_best, "Confusion Matrix.png"),
                      width = 9.92, height = 5.3)
      #
      # calculate mcc
      # ROCR currently supports only evaluation of binary classification tasks !!!
      # mcc <- mcc(train_pr_oof$pr, train_pr_oof$y)
      #
      # get binary ROC curves
      ROC_bin <- data.frame()
      if (exists("ROC_prob")) {
        for (i in 1:length(ROC_prob[['rocs']])) {
          suppressMessages({
            ROC_bin <- dplyr::bind_rows(
              ROC_bin,
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
            ROC_bin <- dplyr::bind_rows(
              ROC_bin,
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
        ggplot2::ggplot(ROC_bin, ggplot2::aes(x = fpr, y = tpr,
                                              colour = stats::reorder(binary, -binary_AUC))) +
          ggplot2::geom_line() +
          ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray", linewidth = 1,
                               linetype = "dashed") +
          ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(5),
                                      limits = c(0, 1)) +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5),
                                      limits = c(0, 1)) +
          ggplot2::labs(title = "ROC", colour = "Binary AUC",
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
        ggplot2::ggsave(paste0(path_output_best, "ROC.png"),
                        width = 9.92, height = 5.3)
      })
    }
    #
    #
    #
    ## Residual drift ####
    #
    if (exists("test_pr")) {
      train_temp <- train_pr_oof %>%
        dplyr::mutate(residuals = y - pr)
      test_temp <- test_pr %>%
        dplyr::mutate(residuals = y_test - pr_test) %>%
        dplyr::rename(y = y_test, pr = pr_test)
      drift_residual <- SRxgboost_calculate_drift(train_temp, test_temp, n_plots = 10,
                                                  name = path_output_best)
    }
    #
    #
    #
    ## Lift Chart ####
    #
    if (objective %in% c("regression", "classification")) {
      train_pr_oof$group <- cut(train_pr_oof$pr,
                                breaks = unique(stats::quantile(train_pr_oof$pr,
                                                                probs = seq(0, 1.01, by = 1/20),
                                                                na.rm = TRUE)),
                                ordered_result = TRUE, dig.lab = 10,
                                include.lowest = TRUE, labels = FALSE)
      temp <- train_pr_oof %>%
        dplyr::group_by(group) %>%
        dplyr::summarise(Actual_value = mean(y), Predicted_value = mean(pr),
                         Actual_sd = stats::sd(y), Predicted_sd = stats::sd(pr),
                         n = dplyr::n()) %>%
        dplyr::mutate(Actual_l = Actual_value - 1.96 * Actual_sd / sqrt(n),
                      Actual_u = Actual_value + 1.96 * Actual_sd / sqrt(n),
                      Predicted_l = Predicted_value - 1.96 * Predicted_sd / sqrt(n),
                      Predicted_u = Predicted_value + 1.96 * Predicted_sd / sqrt(n)) %>%
        dplyr::select(group, Actual_value, Actual_l, Actual_u,
                      Predicted_value, Predicted_l, Predicted_u) %>%
        tidyr::pivot_longer(cols = -group, names_to = c("variable", ".value"), names_sep = "_")
      train_pr_oof$group <- NULL
      #
      ggplot2::ggplot(temp, ggplot2::aes(x = group, y = value, colour = variable, group = variable)) +
        ggplot2::geom_errorbar(ggplot2::aes(ymin = l, ymax = u), width = 0.25, linewidth = 0.8) +
        ggplot2::geom_point() + ggplot2::geom_line() +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6),
                                    labels = scales::format_format(big.mark = "'")) +
        ggplot2::labs(title = "Lift Chart", y = "y", x = "Sorted Predictions", colour = "")
      # temp <- train_pr_oof %>%
      #   dplyr::group_by(group) %>%
      #   dplyr::summarise(Actual = mean(y), Predicted = mean(pr))
      # train_pr_oof$group <- NULL
      # temp <- reshape2::melt(temp, id.vars = "group")
      # #
      # ggplot2::ggplot(temp, ggplot2::aes(x = group, y = value, colour = variable)) +
      #   ggplot2::geom_point() +
      #   ggplot2::geom_line() +
      #   ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
      #   ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
      #   ggplot2::labs(title = "Lift Chart", y = "y", x = "Sorted Predictions", colour = "")
      ggplot2::ggsave(paste0(path_output_best, "Lift Chart.png"),
                      width = 9.92, height = 5.3)
      rm(temp)
    } else {
      # "multilabel
      # add baseline of random group
      set.seed(12345)
      train_pr_oof <- train_pr_oof %>%
        dplyr::mutate(random = sample(train_pr_oof$y, nrow(train_pr_oof), replace = FALSE),
                      model_correct = dplyr::if_else(y == pr, 1, 0),
                      baseline_correct = dplyr::if_else(y == random, 1, 0))
      set.seed(Sys.time())
      #
      temp <- dplyr::full_join(
        # calculate precision for model
        train_pr_oof %>%
          dplyr::count(y, model_correct) %>%
          dplyr::group_by(y) %>%
          dplyr::mutate(model_precision = n / sum(n)) %>%
          dplyr::filter(model_correct == 1) %>%
          dplyr::select(y, model = model_precision),
        # calculate precision for baseline
        train_pr_oof %>%
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
        ggplot2::labs(title = "Lift Chart", y = "Precision", x = "Class", colour = "")
      ggplot2::ggsave(paste0(path_output_best, "Lift Chart.png"),
                      width = 9.92, height = 5.3)
      rm(temp)
    }
    #
    #
    #
    ## 1way graphics ####
    #
    ### variable importance ####
    print("Plot VarImp")
    importance_matrix <- xgboost::xgb.importance(feature_names = colnames(train_mat),
                                                 model = bst)
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
      ggplot2::scale_y_continuous(labels = scales::percent,
                                  breaks = scales::pretty_breaks(5)) +
      ggplot2::facet_grid(~variable, scales = "free") +
      ggplot2::coord_flip()
    ggplot2::ggsave(paste0(path_output_best, "VarImp 0.png"),
                    width = 9.92, height = 5.3)
    # save table
    assign('importance_matrix', importance_matrix, envir = .GlobalEnv)
    utils::write.table(importance_matrix,
                       paste0(path_output_best, "VarImp 0.csv"),
                       row.names = FALSE, col.names = TRUE, append = FALSE,
                       sep = ";", dec = ",")
    #
    ### Partial Dependence Plots ####
    # message(substr(as.character(Sys.time()), 1, 19))
    #
    if (pdp_plots) {
      temp <- importance_matrix %>%
        dplyr::filter(Gain >= pdp_min_rel_Gain) %>%
        dplyr::mutate(Feature = as.character(Feature))
      if (nrow(temp) >= 1) {
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
              set.seed(Sys.time())
              test_eval_mat_ <- Matrix::sparse.model.matrix(~. - 1, data = datenModell_eval_)
              pr_ <- data.frame(pr = stats::predict(bst_1fold, test_eval_mat_))
            } else {
              datenModell_eval_ <- datenModell_eval
              y_ <- data.frame(y = y_test_eval)
              test_eval_mat_ <- test_eval_mat
              pr_ <- data.frame(pr = stats::predict(bst_1fold, test_eval_mat))
            }
            scaling_factor = nrow(datenModell_eval_) / datenModell_rows
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
                dplyr::summarise(Count_eval_sample = dplyr::n(),
                                 Count = round(Count_eval_sample / scaling_factor),
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
              p <- gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(0.75, 0.25)); p
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
                  dplyr::summarise(x = x[1], x_orig = mean(x_orig),
                                   Count_eval_sample = dplyr::n(),
                                   Count = round(Count_eval_sample / scaling_factor),
                                   Predicted = mean(Predicted), Actual = mean(Actual))
              } else {
                # summarise results
                if (is.null(pdp_cuts))
                  pdp_cuts <- min(floor(length(xx) / pdp_int_cuts_sample), 50)
                stats <- data.frame(x = xx, x_orig = datenModell_eval_[, xlabel],
                                    Actual = y_$y, Predicted = pr_$pr) %>%
                  dplyr::mutate(Group = cut(x, breaks = pretty(x, pdp_cuts),
                                            include.lowest = TRUE, dig.lab = 10)) %>%
                  dplyr::group_by(Group) %>%
                  dplyr::summarise(x = mean(x), x_orig = mean(x_orig),
                                   Count_eval_sample = dplyr::n(),
                                   Count = round(Count_eval_sample / scaling_factor),
                                   Predicted = mean(Predicted), Actual = mean(Actual))
              }
            }
            #
            ## partial dependence
            #
            stats_ <- stats %>% dplyr::filter(Count_eval_sample >= pdp_int_cuts_sample)
            #
            if (nrow(datenModell_eval_) > 1000 & pdp_parallel) {
              # in parallel
              invisible({
                partial <- pdp::partial(bst_1fold,
                                        pred.var = temp$Feature[i],
                                        pred.grid = data.frame(stats_$x_orig) %>%
                                          stats::setNames(xlabel),
                                        # grid.resolution = 30,
                                        train = test_eval_mat_,
                                        plot = FALSE, chull = TRUE, type = "regression",
                                        # type = ifelse(objective == "multilabel",
                                        #               "classification", objective))
                                        parallel = TRUE, # with foreach package
                                        paropts = list(.packages = "xgboost"))
              })
            } else {
              # single core
              partial <- pdp::partial(bst_1fold,
                                      pred.var = temp$Feature[i],
                                      pred.grid = data.frame(stats_$x_orig) %>%
                                        stats::setNames(xlabel),
                                      # grid.resolution = 30,
                                      train = test_eval_mat_,
                                      plot = FALSE, chull = TRUE, type = "regression")
              # type = ifelse(objective == "multilabel",
              #               "classification", objective))
            }
            rm(stats_)
            #
            stats <- stats %>%
              dplyr::left_join(partial, by = c("x_orig" = xlabel)) %>%
              dplyr::rename("Partial_Dependence" = "yhat")
            # save stats
            utils::write.table(stats, paste0(path_output_best, "VarImp ", i, " ",
                                             gsub("_LabelEnc", "", xlabel), ".csv"),
                               row.names = FALSE, sep = ";")
            #
            stats <- stats %>% dplyr::filter(Count_eval_sample >= pdp_int_cuts_sample)
            #
            scale <- max(stats$Count, na.rm = TRUE) /
              mean(max(stats$Predicted, na.rm = TRUE),
                   max(stats$Actual, na.rm = TRUE),
                   max(stats$Partial_Dependence, na.rm = TRUE))
            scale <- dplyr::if_else(scale < 0.5, scale, round(scale))
            scale <- dplyr::case_when(round(scale, -nchar(scale)) == 0 &
                                        round(scale) > 0 ~ round(scale, -nchar(scale) + 1),
                                      round(scale, -nchar(scale)) > 0 &
                                        round(scale) > 0 ~ round(scale, -nchar(scale)),
                                      round(scale * 10) > 0 ~ round(scale, 1),
                                      round(scale * 100) > 0 ~ round(scale, 2),
                                      round(scale * 1000) > 0 ~ round(scale, 3),
                                      round(scale * 10000) > 0 ~ round(scale, 4),
                                      TRUE ~ scale)
            scale <- 1 / scale
            #
            if ((gsub("_LabelEnc", "",  temp$Feature[i]) %in% factor_encoding$feature)) { # factor
              x_scale <- ggplot2::scale_x_discrete(labels = function(x) {
                gsub("_", " ", x) %>%
                  gsub("-", "- ", .) %>%
                  gsub("/", "/ ", .) %>%
                  stringr::str_replace_all("([a-z])([A-Z])", "\\1 \\2") %>% # split CamelCase
                  stringr::str_wrap(width = 12)                # wrap after ~12 chars
              })
            } else if (SRfunctions::SR_is_date(stats$x)) {                      # date
              spanne = as.numeric(difftime(max(stats$x), min(stats$x), units = "days"))
              if (spanne > 365.25 * 20) {
                x_scale <- ggplot2::scale_x_date(date_breaks = "10 years",
                                                 date_labels = "%Y",
                                                 date_minor_breaks = "1 year")
              } else if (spanne > 365.25 * 3) {
                x_scale <- ggplot2::scale_x_date(date_breaks = "year",
                                                 date_labels = "%Y",
                                                 date_minor_breaks = "3 months")
              } else if (spanne > 365.25) {
                x_scale <- ggplot2::scale_x_date(date_breaks = "3 months",
                                                 date_labels = "%Y-%m",
                                                 date_minor_breaks = "1 month")
              } else if (spanne > 30) {
                x_scale <- ggplot2::scale_x_date(date_breaks = "1 month",
                                                 date_labels = "%Y-%m-%d",
                                                 date_minor_breaks = "1 week")
              } else {
                x_scale <- ggplot2::scale_x_date(date_breaks = "1 week",
                                                 date_labels = "%Y-%m-%d",
                                                 date_minor_breaks = "1 day")
              }
            } else {                                                            # numeric
              x_scale <- ggplot2::scale_x_continuous(
                breaks = scales::pretty_breaks(ifelse(nrow(stats) > 10, 8, 6)))
            }
            #
            p <- ggplot2::ggplot(stats, ggplot2::aes(x = x)) +
              x_scale +
              # Balken für Count (sekundäre Y-Achse)
              ggplot2::geom_col(ggplot2::aes(y = Count * scale),
                                fill = "gray60", alpha = 0.5, na.rm = TRUE) +
              # Linien für Predicted/Actual
              ggplot2::geom_line(ggplot2::aes(y = Predicted, color = "Predicted",
                                              group = 1), linewidth = 1, na.rm = TRUE) +
              ggplot2::geom_point(ggplot2::aes(y = Predicted, color = "Predicted"),
                                  size = 2, na.rm = TRUE) +
              ggplot2::geom_line(ggplot2::aes(y = Actual, color = "Actual", group = 1),
                                 linewidth = 1, na.rm = TRUE) +
              ggplot2::geom_point(ggplot2::aes(y = Actual, color = "Actual"),
                                  size = 2, na.rm = TRUE) +
              ggplot2::geom_line(
                ggplot2::aes(y = Partial_Dependence, color = "Partial_Dependence",
                             group = 1), linewidth = 1, na.rm = TRUE) +
              ggplot2::geom_point(ggplot2::aes(y = Partial_Dependence,
                                               color = "Partial_Dependence"),
                                  size = 2, na.rm = TRUE) +
              ggplot2::scale_color_manual(
                values = scales::hue_pal()(3)[c(2, 1, 3)] %>%
                  stats::setNames(c("Actual", "Predicted", "Partial_Dependence")),
                breaks = c("Actual", "Predicted", "Partial_Dependence"),
                labels = c("Actual", "Predicted", "Partial Dependence")) +
              # Sekundäre Y-Achse für Count
              ggplot2::scale_y_continuous(
                breaks = scales::pretty_breaks(6),
                labels = scales::format_format(big.mark = "'"),
                sec.axis = ggplot2::sec_axis(~ . / scale, name = "Count (gray bar)",
                                             breaks = scales::pretty_breaks(6),
                                             labels = scales::format_format(big.mark = "'"))) +
              ggplot2::labs(x = gsub("_LabelEnc", "", xlabel), y = "Average value of y") +
              ggplot2::theme(legend.position = "top",
                             legend.title = ggplot2::element_blank())
            print(p)
            #
            # simple PDP-graphic
            p3 <- ggplot2::ggplot(stats,
                                  ggplot2::aes(x = x, y = Partial_Dependence, group = 1)) +
              x_scale +
              ggplot2::geom_point(size = 2, colour = "steelblue", na.rm = TRUE) +
              ggplot2::geom_line(linewidth = I(1), colour = "steelblue") +
              ggplot2::labs(x = gsub("_LabelEnc", "", xlabel), y = "Average value of y") +
              ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6))
            #
            # save graphic
            # xlabel <- gsub("[[:punct:]]", "", xlabel)
            try(ggplot2::ggsave(paste0(path_output_best, "VarImp ", i, " ",
                                       gsub("_LabelEnc", "", xlabel), ".png"),
                                plot = p, width = 9.92, height = 5.3))
            try(rm(p), TRUE)
            try(ggplot2::ggsave(paste0(path_output_best, "VarImp ", i, " ",
                                       gsub("_LabelEnc", "", xlabel), " PDP.png"),
                                plot = p3, width = 9.92, height = 5.3))
            try(rm(p3), TRUE)
          })
        }
        #
        # Plot PDP-summary of all important variables
        files <- list.files(path = path_output_best,
                            pattern = "VarImp .*.csv", full.names = TRUE) %>%
          data.frame(filename = .) %>%
          dplyr::filter(!grepl("VarImp 0.csv", filename)) %>%
          dplyr::mutate(Variable =
                          SRfunctions::SR_trim_text(
                            substr(basename(filename), 10, nchar(basename(filename)) - 4)),
                        Variable = paste0(row_number(), " ", Variable))
        #
        df_temp <- data.frame()
        for (i in 1:nrow(files)) {
          df_temp <- dplyr::bind_rows(
            df_temp,
            rio::import(files$filename[i]) %>%
              dplyr::mutate(Group =
                              factor(Group, levels = rio::import(files$filename[i])$Group),
                            x = as.character(x),
                            Variable = files$Variable[i],
                            .before = 1))
        }; rm(i, files)
        saveRDS(df_temp, paste0(path_output_best, "VarImp 0 Alle PDP.rds"))
        #
        df_temp %>%
          ggplot2::ggplot(ggplot2::aes(x = Group, y = Partial_Dependence, group = Variable)) +
          ggplot2::geom_point(colour = "steelblue", na.rm = TRUE) +
          ggplot2::geom_line(colour = "steelblue", na.rm = TRUE) +
          ggplot2::facet_wrap(vars(Variable), scales = "free_x") +
          ggplot2::labs(title = "Partial Dependence")
        ggplot2::ggsave(paste0(path_output_best, "VarImp 0 Alle PDP.png"),
                        width = 9.92, height = 5.3)
        #
        # plot of selected variables
        df_temp %>% dplyr::count(Variable)
        suppressWarnings({
          df_temp %>%
            # select x-variables with a similar range:
            dplyr::filter(Variable %in% c(unique(df_temp$Variable)[1:5])) %>%
            dplyr::mutate(x = as.numeric(x)) %>%
            ggplot2::ggplot(ggplot2::aes(x = x, y = Partial_Dependence,
                                         group = Variable, colour = Variable)) +
            ggplot2::geom_point(na.rm = TRUE) +
            ggplot2::geom_line(na.rm = TRUE) +
            ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6),
                                        labels = scales::format_format(big.mark = "'"),
                                        transform = "log1p") +
            ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6),
                                        labels = scales::format_format(big.mark = "'")) +
            ggplot2::labs(title = "Partial Dependence", x = "x", y = "y")
          ggplot2::ggsave(paste0(path_output_best, "VarImp 0 Alle PDP 2.png"),
                          width = 9.92, height = 5.3)
        })
        #
        rm(df_temp)
      }
    }   # end of 'pdp_plots'
    #
    #
    ## 2way graphics ####
    # message(substr(as.character(Sys.time()), 1, 19))
    #
    ###  variable importance ####
    #
    print("Plot VarImp interaction")
    # downsample datenModell (because EIX::interactions is very slow) => EIX abgelöst!
    if (nrow(datenModell) > pdp_int_sample & FALSE) {
      set.seed(12345)
      datenModell_ <- datenModell %>% dplyr::sample_n(pdp_int_sample)
      train_mat_ <- Matrix::sparse.model.matrix(~. - 1, data = datenModell_)
      set.seed(Sys.time())
    } else {
      datenModell_ <- datenModell
      train_mat_ <- train_mat
    }
    # downsample datenModell_eval (because pdp::partial is very slow)
    if (nrow(datenModell_eval) > pdp_int_sample) {
      set.seed(12345)
      datenModell_eval_ <- datenModell_eval %>% dplyr::sample_n(pdp_int_sample)
      set.seed(12345)
      y_ <- data.frame(y = y_test_eval) %>% dplyr::sample_n(pdp_int_sample)
      set.seed(Sys.time())
      test_eval_mat_ <- Matrix::sparse.model.matrix(~. - 1, data = datenModell_eval_)
      pr_ <- data.frame(pr = stats::predict(bst_1fold, test_eval_mat_))
    } else {
      datenModell_eval_ <- datenModell_eval
      y_ <- data.frame(y = y_test_eval)
      test_eval_mat_ <- test_eval_mat
      pr_ <- data.frame(pr = stats::predict(bst_1fold, test_eval_mat))
    }
    #
    # calculate most important interaction pairs Parent/Child
    #
    dt <- as.data.frame(xgboost::xgb.model.dt.tree(model = bst, trees = 0:999))
    top <- as.data.frame(xgboost::xgb.importance(model = bst)) %>%
      dplyr::mutate(Feature = sub("^f", "", Feature)) %>%
      dplyr::slice_head(n = 30) %>%
      dplyr::pull(Feature)
    edges <- dplyr::bind_rows(
      dt %>%
        dplyr::filter(!is.na(Yes)) %>%
        dplyr::transmute(Tree, ChildID = Yes, Parent = Feature),
      dt %>%
        dplyr::filter(!is.na(No))  %>%
        dplyr::transmute(Tree, ChildID = No,  Parent = Feature)) %>%
      dplyr::mutate(ChildID = as.integer(sub(".*-", "", ChildID)))
    childs <- dt %>%
      dplyr::filter(Feature != "Leaf") %>%
      dplyr::transmute(Tree, ID = Node, Child = Feature, Gain = Quality)
    interactions <- dplyr::left_join(childs, edges, by = c("Tree", "ID" = "ChildID")) %>%
      dplyr::filter(Parent %in% top, Child %in% top) %>%
      dplyr::group_by(Parent, Child) %>%
      dplyr::summarise(sumGain = sum(Gain, na.rm = TRUE),
                       frequency = dplyr::n(),
                       .groups = "drop") %>%
      dplyr::mutate(A = pmin(Parent, Child), B = pmax(Parent, Child)) %>%
      dplyr::group_by(A, B) %>%
      dplyr::summarise(sumGain = sum(sumGain),
                       frequency = sum(frequency),
                       .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(sumGain))
    base::rm(dt, top, edges, childs)
    # clean up
    interactions_clean <- data.frame(interactions) %>%
      dplyr::mutate(Gain = sumGain / sum(sumGain),
                    Frequency = frequency / sum(frequency)) %>%
      dplyr::mutate_at(dplyr::vars(A, B), ~as.numeric(.)) %>%
      dplyr::left_join(data.frame(Parent = colnames(datenModell_eval),
                                  Number = 0:(ncol(datenModell_eval) - 1)),
                       by = dplyr::join_by(A == Number)) %>%
      dplyr::left_join(data.frame(Child = colnames(datenModell_eval),
                                  Number = 0:(ncol(datenModell_eval) - 1)),
                       by = dplyr::join_by(B == Number)) %>%
      dplyr::mutate(Feature = paste0(Parent, " - ", Child))
    #
    # # ALT:
    # interactions <- EIX::interactions(bst, train_mat_, option = "pairs")
    # # interactions_ <- EIX::interactions(bst, train_mat_, option = "interactions")
    # # plot(interactions)
    # interactions_clean <- data.frame(interactions) %>%
    #   dplyr::left_join(importance_matrix %>%
    #                      dplyr::mutate(Parent_Rang = 1:nrow(.)) %>%
    #                      dplyr::select(Feature, Parent_Rang),
    #                    by = c("Parent" = "Feature")) %>%
    #   dplyr::left_join(importance_matrix %>%
    #                      dplyr::mutate(Child_Rang = 1:nrow(.)) %>%
    #                      dplyr::select(Feature, Child_Rang),
    #                    by = c("Child" = "Feature")) %>%
    #   dplyr::mutate(Parent_clean = ifelse(Parent_Rang < Child_Rang, Parent, Child),
    #                 Child_clean = ifelse(Parent_Rang > Child_Rang, Parent, Child),
    #                 Parent = Parent_clean,
    #                 Child = Child_clean) %>%
    #   dplyr::select(-Parent_clean, -Parent_Rang, -Child_clean, -Child_Rang) %>%
    #   dplyr::group_by(Parent, Child) %>%
    #   dplyr::summarise(sumGain = sum(sumGain),
    #                    frequency = sum(frequency),
    #                    .groups = "drop") %>%
    #   dplyr::arrange(-sumGain) %>%
    #   dplyr::mutate(Gain = sumGain / sum(sumGain),
    #                 Frequency = frequency / sum(frequency),
    #                 Feature = paste0(Parent, " - ", Child))
    utils::write.table(interactions_clean,
                       paste0(path_output_best, "VarImpInt 0.csv"),
                       row.names = FALSE, col.names = TRUE, append = FALSE,
                       sep = ";", dec = ",")
    #
    # importance <- EIX::importance(bst, train_mat_, option = "interactions")
    # importance <- EIX::importance(bst, train_mat_, option = "both")
    # plot(importance, top = 5)
    # plot(importance, top = 5, radar = FALSE)
    # head(importance)
    #
    # plot
    interactions_clean %>%
      dplyr::slice(1:(min(nrow(interactions_clean), 30))) %>%
      dplyr::select(Feature, Gain, Frequency) %>%
      reshape2::melt(id.vars = "Feature") %>%
      ggplot2::ggplot(ggplot2::aes(x = reorder(Feature, value), y = value)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::labs(x = "Variable", y = "") +
      ggplot2::scale_y_continuous(labels = scales::percent,
                                  breaks = scales::pretty_breaks(5)) +
      ggplot2::facet_grid(~variable, scales = "free") +
      ggplot2::coord_flip()
    ggplot2::ggsave(paste0(path_output_best, "VarImpInt 0.png"),
                    width = 9.92, height = 5.3)
    #
    ### Partial Dependence Plots ####
    #
    # also plots possible for class predictions in "multilabel"                 # TODO
    # https://journal.r-project.org/archive/2017/RJ-2017-016/RJ-2017-016.pdf
    if (pdp_plots) {
      if (objective %in% c("regression", "classification")) {
        temp <- interactions_clean %>%
          dplyr::filter(Gain >= pdp_min_rel_Gain,
                        Parent != Child)
        # add interactions of 3 most important variables (if not existing)
        temp <- dplyr::full_join(temp,
                                 importance_matrix %>%
                                   dplyr::slice(1:3) %>%
                                   # dplyr::filter(Gain >= pdp_min_rel_Gain) %>%
                                   dplyr::mutate(Feature = as.character(Feature)) %>%
                                   dplyr::select(Feature) %>%
                                   dplyr::cross_join(., .) %>%
                                   dplyr::slice(c(2, 3, 4, 6)) %>%
                                   stats::setNames(c("Parent", "Child")) %>%
                                   dplyr::mutate(Feature = paste0(Parent, " - ", Child)),
                                 by = c("Parent", "Child", "Feature")) %>%
          # remove duplicated (but swapped) pairs
          mutate(sorted_values =
                   purrr::pmap(list(Parent, Child),
                               function(...) paste(sort(c(...)), collapse="_"))) %>%
          distinct(sorted_values, .keep_all = TRUE) %>%
          select(-sorted_values)
        #
        if (nrow(temp) >= 1) {
          for (i in seq_along(temp$Feature)) {   # i <- 1
            # print progress
            print(paste0("Plot interaction ", i, " of ", nrow(temp), ": ", temp$Feature[i]))
            #
            try({
              # calculate stats
              f1 <- temp$Parent[i]; f2 <- temp$Child[i]
              qv <- function(v, m = 11) as.numeric(quantile(v, seq(0.05, 0.95, length.out = m),
                                                            na.rm = TRUE, names = FALSE))
              pg <- stats::setNames(expand.grid(qv(datenModell_eval_[[f1]]),
                                                qv(datenModell_eval_[[f2]]),
                                                KEEP.OUT.ATTRS = FALSE),
                                    c(f1, f2)) %>%
                distinct(.)
              rm(f1, f2, qv)
              #
              if (nrow(datenModell_eval_) > 1000 & pdp_parallel) {
                # in parallel
                invisible({
                  partial <- pdp::partial(bst_1fold,
                                          pred.var = c(temp$Parent[i], temp$Child[i]),
                                          # pred.grid = data.frame(stats$x_orig) %>%
                                          #   stats::setNames(xlabel),
                                          # grid.resolution = 30,
                                          pred.grid = pg, # faster
                                          train = test_eval_mat_,
                                          type = "regression", plot = FALSE,
                                          parallel = TRUE,
                                          paropts = list(.packages = "xgboost"))
                })
              } else {
                # single core
                partial <- pdp::partial(bst_1fold,
                                        pred.var = c(temp$Parent[i], temp$Child[i]),
                                        # pred.grid = data.frame(stats$x_orig) %>%
                                        #   stats::setNames(xlabel),
                                        # grid.resolution = 30,
                                        pred.grid = pg, # faster
                                        train = test_eval_mat_,
                                        type = "regression", plot = FALSE)
              }
              # partial <- pdp::partial(bst_1fold,
              #                         pred.var = c(temp$Parent[i], temp$Child[i]),
              #                         # pred.grid = data.frame(stats$x_orig) %>%
              #                         #   stats::setNames(xlabel),
              #                         # grid.resolution = 30,
              #                         train = test_eval_mat_,
              #                         # train = datenModell_eval_,
              #                         # type = objective,
              #                         type = "regression",
              #                         # type = "classification", prob = TRUE,   # error "classification" !!!
              #                         plot = FALSE)
              #
              # save graphic
              col_x1 <- names(partial)[1]
              col_x2 <- names(partial)[2]
              p <- partial %>%
                ggplot2::ggplot(ggplot2::aes(x = .data[[col_x1]], y = .data[[col_x2]],
                                             fill = yhat)) +
                ggplot2::geom_tile() +
                ggplot2::scale_fill_gradient2(
                  low = "blue", mid = "white", high = "red",
                  midpoint = mean(partial$yhat, na.rm = TRUE),
                  breaks = scales::pretty_breaks(4),
                  labels = scales::format_format(big.mark = "'")) +
                ggplot2::labs(title = "Partial Dependence", fill = "Predicted\nValue")
              ggplot2::ggsave(paste0(path_output_best, "VarImpInt ", i, " ",
                                     gsub("_LabelEnc", "", temp$Feature[i]), ".png"),
                              plot = p, width = 9.92, height = 5.3)
              rm(col_x1, col_x2, p)
              #
              mid <- mean(partial$yhat, na.rm = TRUE)
              rng <- range(partial$yhat, na.rm = TRUE)
              n_below <- round(100 * (mid - rng[1]) / diff(rng))
              n_above <- 100 - n_below
              cols <- c(colorRampPalette(c("blue", "white"))(n_below),
                        colorRampPalette(c("white", "red"))(n_above))
              labs <- names(partial)[1:2] %>%
                gsub("_", " ", .) %>%
                gsub("-", "- ", .) %>%
                gsub("/", "/ ", .) %>%
                stringr::str_replace_all("([a-z])([A-Z])", "\\1 \\2") %>%
                stringr::str_wrap(width = 12)
              p <- pdp::plotPartial(partial, xlab = labs[1], ylab = labs[2],
                                    zlab = "Predicted  \nValue ",
                                    levelplot = FALSE, drape = TRUE,
                                    # colorkey = TRUE,
                                    colorkey = list(labels = list(
                                      at = pretty(partial$yhat),
                                      labels = format(pretty(partial$yhat), big.mark = "'",
                                                      scientific = FALSE))),
                                    col.regions = cols,
                                    scales = list(cex = 1, arrows = FALSE,
                                                  distance = c(1.2, 1.2, 1),
                                                  z = list(draw = FALSE)),
                                    par.settings = list(
                                      axis.line = list(col = "black", lwd = 1.5),
                                      fontsize = list(text = 10, points = 10)
                                    ),
                                    screen = list(z = 30, x = -60, y = 0))
              print(p)
              ggplot2::ggsave(paste0(path_output_best, "VarImpInt ", i, " ",
                                     gsub("_LabelEnc", "", temp$Feature[i]), " 3D.png"),
                              plot = gridExtra::arrangeGrob(p),
                              width = 7, height = 7)
              rm(mid, rng, n_below, n_above, cols, p)
              #
              # save summary table
              utils::write.table(partial,
                                 paste0(path_output_best, "VarImpInt ", i, " ",
                                        gsub("_LabelEnc", "", temp$Feature[i]), ".csv"),
                                 row.names = FALSE, sep = ";")
              rm(partial)
            })
          }
        }
      }
      # message(substr(as.character(Sys.time()), 1, 19))
    }   # end of 'pdp_plots'
    if (pdp_parallel) {
      # stop cluster
      try(parallel::stopCluster(cl), TRUE)
      try(parallel::stopCluster(cl), TRUE)
      try(rm(cl, pdp_n_core), TRUE); invisible(gc())
    }
  }   # end of 'plots'
}

