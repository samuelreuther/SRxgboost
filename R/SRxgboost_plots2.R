#' SRxgboost_plots (testing with explicit parallelisation)
#'
#' with pdp_parallel = FALSE:
#' as is:     15/79 s
# lapply:     14/77 s => nicht schneller
# parLapply:  23/81 s => nicht schneller
# EIX opt:    15/75 s => nicht schneller
# with pdp_parallel = TRUE:
# as is:     13/51 s => 32% schneller => set as new default!
# lapply:    15/46 s => 35% schneller
# parLapply: 46/44 s => nicht schneller
# EIX opt:   12/44 s => nicht schneller
#'
# SRxgboost_plots <- function(lauf, rank = 1,
#                             plots = TRUE, sample = 100000,
#                             silent = FALSE,
#                             pdp_plots = TRUE, pdp_min_rel_Gain = 0.01,
#                             pdp_sample = 20000, pdp_cuts = NULL,
#                             pdp_int_sample = 1000, pdp_int_cuts_sample = 50,
#                             pdp_parallel = TRUE) {
#   ### Initialisation ####
#   #
#   # check lauf ends with ".csv"
#   if (!grepl('.csv$', lauf)) lauf <- paste0(lauf, ".csv")
#   #
#   # delete old plots
#   if (plots & dir.exists(paste0(path_output, gsub(".csv", "/", lauf), "Best Model/"))) {
#     unlink(paste0(path_output, gsub(".csv", "/", lauf), "Best Model/*"),
#            recursive = TRUE)
#   }
#   #
#   # create XGB_plots folder
#   dir.create(paste0(path_output, gsub(".csv", "/", lauf), "Best Model/"),
#              showWarnings = FALSE)
#   #
#   # Get summary
#   SRxgboost_get_summary_CV(lauf)
#   assign('SummaryCV', SummaryCV, envir = .GlobalEnv)
#   if (!silent) print(utils::head(SummaryCV[!is.na(SummaryCV$eval_1fold), c(1:12)]))
#   #
#   # Load model
#   if (!silent) print(paste0("Loading model: ", SummaryCV$date[rank]))
#   modelpath <- paste0(path_output, paste0(gsub(".csv", "/", lauf), "All Models/",
#                                           gsub(":", ".", SummaryCV$date[rank]),
#                                           ".model"))
#   if (file.exists(modelpath) |
#       file.exists(gsub(".model", "_1fold.model", modelpath, fixed = TRUE))) {
#     if (file.exists(modelpath)) {
#       bst <- xgboost::xgb.load(modelpath)
#       bst_ <- readRDS(gsub(".model", ".model.rds", modelpath, fixed = TRUE))
#       assign('bst', bst, envir = .GlobalEnv)
#     } else {
#       bst <- xgboost::xgb.load(gsub(".model", "_1fold.model", modelpath, fixed = TRUE))
#     }
#     bst_1fold <- xgboost::xgb.load(gsub(".model", "_1fold.model", modelpath, fixed = TRUE))
#     assign('bst_1fold', bst_1fold, envir = .GlobalEnv)
#     try(file.copy(modelpath,
#                   paste0(path_output, gsub(".csv", "/", lauf),
#                          "Best Model/model"),
#                   overwrite = TRUE), TRUE)
#     try(file.copy(gsub(".model", ".model.rds", modelpath),
#                   paste0(path_output, gsub(".csv", "/", lauf),
#                          "Best Model/model.rds"),
#                   overwrite = TRUE), TRUE)
#     file.copy(gsub(".model", "_1fold.model", modelpath),
#               paste0(path_output, gsub(".csv", "/", lauf),
#                      "Best Model/1fold.model"),
#               overwrite = TRUE)
#     file.copy(gsub(".model", "_1fold.model.rds", modelpath),
#               paste0(path_output, gsub(".csv", "/", lauf),
#                      "Best Model/1fold.model.rds"),
#               overwrite = TRUE)
#     file.copy(gsub(".model", "_Error_rate.png", modelpath),
#               paste0(path_output, gsub(".csv", "/", lauf),
#                      "Best Model/Error_rate.png"),
#               overwrite = TRUE)
#     file.copy(gsub(".model", "_Evaluation_log.rds", modelpath),
#               paste0(path_output, gsub(".csv", "/", lauf),
#                      "Best Model/Evaluation_log.rds"),
#               overwrite = TRUE)
#     try({
#       file.copy(gsub(".model", "_Shap_train_eval.rds", modelpath),
#                 paste0(path_output, gsub(".csv", "/", lauf),
#                        "Best Model/Shap_train_eval.rds"),
#                 overwrite = TRUE)
#       file.copy(gsub(".model", "_Shap_prediction.rds", modelpath),
#                 paste0(path_output, gsub(".csv", "/", lauf),
#                        "Best Model/Shap_prediction.rds"),
#                 overwrite = TRUE)
#       file.copy(gsub(".model", "_Shap_datenModell_eval.rds", modelpath),
#                 paste0(path_output, gsub(".csv", "/", lauf),
#                        "Best Model/Shap_datenModell_eval.rds"),
#                 overwrite = TRUE)
#       file.copy(gsub(".model", "_Shap_index_test_eval.rds", modelpath),
#                 paste0(path_output, gsub(".csv", "/", lauf),
#                        "Best Model/Shap_index_test_eval.rds"),
#                 overwrite = TRUE)
#       file.copy(gsub(".model", "_Shap_plot.png", modelpath),
#                 paste0(path_output, gsub(".csv", "/", lauf),
#                        "Best Model/Shap_plot.png"),
#                 overwrite = TRUE)
#     }, TRUE)
#   } else {
#     # re-run best model     TODO !!!
#     # bst <- xgboost(data = d_train, objective = "multi:softprob", num_class = length(unique(y)),
#     #                metric = "mlogloss", verbose = 0, max.depth = SummaryCV$depth,
#     #                eta = SummaryCV$eta, nround = SummaryCV$nround,
#     #                min_child_weight = SummaryCV$min_child_weight, gamma = SummaryCV$gamma,
#     #                subsample = SummaryCV$subsample, colsample_bytree = SummaryCV$colsample_bytree)
#   }
#   rm(modelpath)
#   #
#   #
#   #
#   ### get OOFforecast and TESTforecast ####
#   #
#   SRxgboost_get_OOFforecast_TESTforecast(lauf = lauf, top_rank = 1, ensemble = FALSE)
#   if (length(y) == nrow(OOFforecast)) {
#     train_pr_oof <- as.data.frame(cbind(y, pr = OOFforecast[, ncol(OOFforecast)]))
#   } else {
#     train_pr_oof <- as.data.frame(cbind(y = y[eval_index],
#                                         pr = OOFforecast[, ncol(OOFforecast)]))
#   }
#   # for "multi:softprob": add probabilities
#   if (objective == "multilabel" & ncol(OOFforecast) > 2) {
#     train_pr_oof <- dplyr::bind_cols(train_pr_oof,
#                                      OOFforecast[, 2:(ncol(OOFforecast) - 1)] %>%
#                                        stats::setNames(paste0("X", 0:(ncol(.) - 1))))
#   }
#   assign('train_pr_oof', train_pr_oof, envir = .GlobalEnv)
#   if (file.exists(paste0(path_output, gsub(".csv", "/", lauf), "Data/y_test.rds"))) {
#     y_test <- readRDS(paste0(path_output, gsub(".csv", "/", lauf), "Data/y_test.rds"))
#     test_pr <- as.data.frame(cbind(y_test,
#                                    pr_test = TESTforecast[, ncol(TESTforecast)]))
#     assign('test_pr', test_pr, envir = .GlobalEnv)
#   }
#   # # get pr_oof
#   # OOFforecast <- readRDS(paste0(path_output, gsub(".csv", "/", lauf), "Data/OOFforecast.rds"))
#   # i_OOFforecast <- stringdist::stringdistmatrix(
#   #   gsub("-",".",gsub(":",".",SummaryCV$date)),
#   #   gsub("X","",colnames(OOFforecast)[2:ncol(OOFforecast)]))[rank, ]
#   # i_OOFforecast <- which.min(i_OOFforecast); as.character(SummaryCV$date[rank])
#   # colnames(OOFforecast[i_OOFforecast + 1])
#   # pr <- OOFforecast[, i_OOFforecast + 1]
#   # if (length(y) == length(pr)) {
#   #   train_pr_oof <- as.data.frame(cbind(y, pr))
#   # } else {
#   #   train_pr_oof <- as.data.frame(cbind(y = y[eval_index], pr))
#   # }
#   # assign('train_pr_oof', train_pr_oof, envir = .GlobalEnv)
#   # #
#   # # get pr_test
#   # if (file.exists(paste0(path_output, gsub(".csv", "/", lauf), "Data/y_test.rds"))) {
#   #   TESTforecast <- readRDS(paste0(path_output, gsub(".csv", "/", lauf),
#   #                                  "Data/TESTforecast.rds"))
#   #   y_test <- readRDS(paste0(path_output, gsub(".csv", "/", lauf), "Data/y_test.rds"))
#   #   pr_test <- TESTforecast[, i_OOFforecast + 1]
#   #   test_pr <- as.data.frame(cbind(y_test, pr_test))
#   #   assign('test_pr', test_pr, envir = .GlobalEnv)
#   # }
#   #
#   #
#   #
#   ### generate graphics ####
#   #
#   if (plots) {
#     ### downsample train_pr_oof if nrow > sample
#     # quite different results, e.g. optimal cutoff value! error at XGBFI_PDP1!
#     #
#     if (nrow(train_pr_oof) > sample) {
#       set.seed(12345)
#       train_pr_oof <- train_pr_oof %>% dplyr::sample_n(sample)
#       set.seed(Sys.time())
#     }
#     #
#     # define mcc function
#     # ROCR currently supports only evaluation of binary classification tasks !!!
#     mcc <- function(pred, labels) {
#       temp <- data.frame(pred = pred, labels = labels)
#       if (length(pred) > 100000) {   # NAs produced by integer overflow > 500'000
#         set.seed(12345)
#         temp <- temp %>% dplyr::sample_n(100000)
#         set.seed(Sys.time())
#       }
#       prediction <- ROCR::prediction(temp$pred, temp$labels)
#       mcc <- ROCR::performance(prediction, "mat")
#       err <- max(mcc@y.values[[1]], na.rm = TRUE)
#       opt_cutoff = mcc@x.values[[1]][which.max(mcc@y.values[[1]])]
#       return(list(mcc = err, opt_cutoff = opt_cutoff))
#     }
#     prAUC <- function(pred, labels) {
#       # https://stats.stackexchange.com/questions/10501/calculating-aupr-in-r
#       # library(PRROC)
#       fg <- pred[labels == 1]
#       bg <- pred[labels == 0]
#       pr <- PRROC::pr.curve(scores.class0 = fg, scores.class1 = bg, curve = T)
#       prauc <- pr$auc.integral
#       # plot(pr)
#       #
#       # yardstick::pr_auc(preds, truth = diabetes, .pred_pos)$.estimate
#       #
#       # MLmetrics::PRAUC(preds$.pred_pos, preds$diabetes)
#       #
#       # get precision and recall, but how to calculate auc correctly???
#       # ROC <- pROC::roc(response = labels,
#       #                  predictor = pred, algorithm = 2,
#       #                  levels = c(0, 1), direction = "<") %>%
#       #   pROC::coords(ret = "all", transpose = FALSE)
#       #
#       # p_load(PerfMeas)
#       # => should be faster, but problems with installation and documentation
#       #
#       return(list(metric="prAUC", value = prauc))
#     }
#     #
#     #
#     #
#     #### Plot y vs. model prediction ####
#     #
#     if (objective == "regression") {   # SRfunctions::SR_is_number(y) & length(unique(y)) > 2
#       # avoid negative values if min(y) >= 0
#       if (min(y, na.rm = TRUE) >=  0) {   # train_pr_oof$y
#         train_pr_oof$pr <- dplyr::case_when(is.na(train_pr_oof$pr) ~ NA_real_,
#                                             train_pr_oof$pr < 0    ~ 0,
#                                             TRUE                   ~ train_pr_oof$pr)
#       }
#       #
#       # plot
#       ggplot2::ggplot(train_pr_oof, ggplot2::aes(x = y, y = pr)) +
#         ggplot2::geom_point() +
#         ggplot2::geom_abline(intercept = 0, slope = 1,
#                              colour = "red", linetype = "dashed") +
#         ggplot2::labs(y = "model prediction", title = "y vs. model prediction",
#                       subtitle = paste0("RMSE:    ", round(Metrics::rmse(train_pr_oof$y,
#                                                                          train_pr_oof$pr), 3) %>%
#                                           format(., nsmall = 3),
#                                         "\nMAE:      ",
#                                         round(Metrics::mae(train_pr_oof$y,
#                                                            train_pr_oof$pr), 3) %>%
#                                           format(., nsmall = 3),
#                                         "\nMAPE:    ",
#                                         round(sum(abs(train_pr_oof$pr /
#                                                         train_pr_oof$y - 1)) /
#                                                 length(train_pr_oof$y), 3) %>%
#                                           format(., nsmall = 3),
#                                         "\nR2:          ",
#                                         round(stats::cor(train_pr_oof$y,
#                                                          train_pr_oof$pr)^2, 3) %>%
#                                           format(., nsmall = 3))) # ,
#       # "\nAUC:       ", round(pROC::auc(train_pr_oof$y,
#       #                                  train_pr_oof$pr), 3)))
#       ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                              "Best Model/Accuracy y vs. model prediction.png"),
#                       width = 9.92, height = 5.3)  # 4.67
#     } else if (objective == "classification") {
#       # calculate ROC-curve
#       ROC <- pROC::roc(response = train_pr_oof$y,
#                        predictor = train_pr_oof$pr, algorithm = 2,
#                        levels = c(0, 1), direction = "<")
#       # ROC <- pROC::roc(response = train_pr_oof$y,
#       #                  predictor = train_pr_oof$pr, algorithm = 2, quiet = TRUE)
#       #
#       # Accuracy and Cut off
#       temp <- data.frame(threshold = ROC$thresholds,
#                          true_positive_rate = ROC$sensitivities,
#                          true_negative_rate = ROC$specificities)
#       temp$balanced_accuracy <- (temp$true_positive_rate + temp$true_negative_rate) / 2
#       temp$accuracy <-
#         (temp$true_positive_rate * sum(train_pr_oof$y == 1) +
#            temp$true_negative_rate * sum(train_pr_oof$y == 0)) /
#         (sum(train_pr_oof$y == 1) + sum(train_pr_oof$y == 0))
#       temp <- temp %>% dplyr::filter(!threshold %in% c(-Inf, Inf))   # remove -Inf, Inf thresholds NEW !!!
#       # calculate cut off
#       cut_off_ba <- round(temp$threshold[which.max(temp$balanced_accuracy)], 3)
#       cut_off_ba_max <- max(temp$balanced_accuracy)
#       cut_off_a <- round(temp$threshold[which.max(temp$accuracy)], 3)
#       if (cut_off_a == 1) cut_off_a <- 0.99
#       if (cut_off_a == 0) cut_off_a <- 0.01
#       cut_off_a_max <- max(temp$accuracy)
#       # plot accuracy
#       temp$threshold[temp$threshold < 0] <- 0
#       temp$threshold[temp$threshold > 1] <- 1
#       temp_long <- reshape2::melt(temp, id = "threshold")
#       ggplot2::ggplot(temp_long, ggplot2::aes(x = threshold, y = value, colour = variable)) +
#         ggplot2::geom_line() +
#         ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_ba),
#                             linetype = "dashed", colour = "turquoise3") +
#         ggplot2::annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
#                           label = paste0("cut off ba = ", cut_off_ba)) +
#         ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_a),
#                             linetype = "dashed", colour = "darkorchid1") +
#         ggplot2::annotate("text", x = cut_off_a + 0.07, y = 0.05, colour = "darkorchid1",
#                           label = paste0("cut off a = ", cut_off_a)) +
#         ggplot2::scale_x_continuous(limits = c(0, 1),
#                                     breaks = scales::pretty_breaks(6)) +
#         ggplot2::scale_y_continuous(limits = c(0, 1),
#                                     breaks = scales::pretty_breaks(6)) +
#         ggplot2::labs(title = "Accuracy and cut off", x = "Threshold",
#                       y = "Accuracy", colour = "") +
#         ggplot2::theme(legend.position = "top")
#       ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                              "Best Model/Accuracy and Cut off.png"),
#                       width = 9.92, height = 5.3)
#       #
#       # Cut off and Costs
#       temp$costs_1_to_1 <-
#         temp$true_positive_rate * sum(train_pr_oof$y == 1) * 1 +
#         temp$true_negative_rate * sum(train_pr_oof$y == 0) * 1
#       temp$costs_1_to_3 <-
#         temp$true_positive_rate * sum(train_pr_oof$y == 1) * 1 +
#         temp$true_negative_rate * sum(train_pr_oof$y == 0) * 3
#       temp$costs_3_to_1 <-
#         temp$true_positive_rate * sum(train_pr_oof$y == 1) * 3 +
#         temp$true_negative_rate * sum(train_pr_oof$y == 0) * 1
#       #
#       temp %>%
#         dplyr::select(-true_positive_rate, -true_negative_rate, -balanced_accuracy,
#                       -accuracy) %>%
#         reshape2::melt(id = "threshold") %>%
#         ggplot2::ggplot(ggplot2::aes(x = threshold, y = value, colour = variable)) +
#         ggplot2::geom_line() +
#         ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_ba),
#                             linetype = "dashed", colour = "turquoise3") +
#         ggplot2::annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
#                           label = paste0("cut off ba = ", cut_off_ba)) +
#         ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_a),
#                             linetype = "dashed", colour = "darkorchid1") +
#         ggplot2::annotate("text", x = cut_off_a + 0.07, y = 0.05, colour = "darkorchid1",
#                           label = paste0("cut off a = ", cut_off_a)) +
#         ggplot2::scale_x_continuous(limits = c(0, 1),
#                                     breaks = scales::pretty_breaks(6)) +
#         ggplot2::scale_y_continuous(limits = c(0, NA),
#                                     breaks = scales::pretty_breaks(6)) +
#         ggplot2::labs(title = "Cut off and costs", x = "Threshold",
#                       y = "Costs", colour = "") +
#         ggplot2::theme(legend.position = "top")
#       ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                              "Best Model/Cut off and Costs.png"),
#                       width = 9.92, height = 5.3)
#       #
#       # save ROC data
#       utils::write.table(temp, paste0(path_output, gsub(".csv", "/", lauf),
#                                       "Best Model/Cut off, accuracy and costs.csv"),
#                          row.names = FALSE, sep = ";")
#       #
#       # confusion matrix
#       if (!cut_off_a %in% c(-Inf, Inf, 0, 1)) {
#         confusion_matrix <-
#           caret::confusionMatrix(table(ifelse(train_pr_oof$pr > cut_off_a, 1, 0),
#                                        train_pr_oof$y),
#                                  positive = "1")
#         print(confusion_matrix)
#         sink(paste0(path_output, gsub(".csv", "/", lauf),
#                     "Best Model/Confusion matrix.txt"), append = FALSE)
#         print(confusion_matrix)
#         sink()
#         # plot confusion matrix
#         # suppressWarnings(
#         SRfunctions::SR_mosaicplot(var1 = factor(train_pr_oof$y, levels = c("0", "1")),   # !!!
#                                    var2 = factor(ifelse(train_pr_oof$pr <= cut_off_a, 0, 1),
#                                                  levels = c("0", "1")))
#         # )
#         # suppressWarnings(SRfunctions::SR_mosaicplot(df = train_pr_oof, cutoff = cut_off_a))
#         ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                                "Best Model/Confusion Matrix.png"),
#                         width = 9.92, height = 5.3)  # 4.67
#       }
#       #
#       # print ROC-curve
#       try({
#         temp <- data.frame(tpr = ROC$sensitivities,
#                            fpr = 1 - ROC$specificities)
#         mcc <- mcc(train_pr_oof$pr, train_pr_oof$y)
#         prauc <- prAUC(pred = train_pr_oof$pr, labels = train_pr_oof$y)$value
#         prauc_benchmark <- as.numeric(prop.table(table(train_pr_oof$y))[2])
#         ggplot2::ggplot(temp, ggplot2::aes(x = fpr, y = tpr)) +
#           ggplot2::geom_line() +
#           ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray", linewidth = 1,
#                                linetype = "dashed") +
#           ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(5),
#                                       limits = c(0, 1)) +
#           ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5),
#                                       limits = c(0, 1)) +
#           ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_ba),
#                               linetype = "dashed", colour = "turquoise3") +
#           ggplot2::annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
#                             label = paste0("cut off ba = ", cut_off_ba)) +
#           ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_a),
#                               linetype = "dashed", colour = "darkorchid1") +
#           ggplot2::annotate("text", x = cut_off_a + 0.07, y = 0.05, colour = "darkorchid1",
#                             label = paste0("cut off a = ", cut_off_a)) +
#           ggplot2::labs(title = "ROC",
#                         x = "Rate of false positives", y = "Rate of true positives",
#                         subtitle = paste0("AUC:                  ",
#                                           round(as.numeric(ROC$auc), 3) %>%
#                                             format(., nsmall = 3),
#                                           " / AUC PR: ",
#                                           format(round(prauc, 3), nsmall = 3),
#                                           " (benchmark = ",
#                                           format(round(prauc_benchmark, 3),
#                                                  nsmall = 3), ") ",
#                                           "\nMCC:                  ",
#                                           paste0(round(mcc$mcc, 3),
#                                                  " (cutoff = ",
#                                                  round(mcc$opt_cutoff, 3) %>%
#                                                    format(., nsmall = 3), ")"),
#                                           "\nAccuracy:           ",
#                                           round(as.numeric(confusion_matrix$overall[1]), 3) %>%
#                                             format(., nsmall = 3),
#                                           "\nSensitivity/TPR:  ",
#                                           round(as.numeric(confusion_matrix$byClass[1]), 3) %>%
#                                             format(., nsmall = 3),
#                                           "\nSpecificity/TNR:  ",
#                                           round(as.numeric(confusion_matrix$byClass[2]), 3) %>%
#                                             format(., nsmall = 3)))
#         ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf), "Best Model/ROC.png"),
#                         width = 9.92, height = 5.3)  # 4.67
#       })
#       rm(temp)
#       #
#       # print class distribution (model-forecast)
#       # relation <- sum(train_pr_oof$y == 1) / sum(train_pr_oof$y == 0)
#       # if (relation < 0.2 | relation > 5) {
#       ggplot2::ggplot(train_pr_oof, ggplot2::aes(x = pr, fill = as.factor(y))) +
#         ggplot2::geom_histogram(bins = 30) +
#         ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_ba),
#                             linetype = "dashed", colour = "turquoise3") +
#         ggplot2::annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
#                           label = paste0("cut off ba = ", cut_off_ba)) +
#         ggplot2::geom_vline(ggplot2::aes(xintercept = cut_off_a),
#                             linetype = "dashed", colour = "darkorchid1") +
#         ggplot2::annotate("text", x = cut_off_a + 0.07, y = 0, colour = "darkorchid1",
#                           label = paste0("cut off a = ", cut_off_a)) +
#         ggplot2::labs(title = "Class distributions", x = "Probability (y = 1)",
#                       y = "Count", fill = "y") +
#         ggplot2::facet_grid(y~., scales = "free_y")
#       # } else {
#       #   ggplot(train_pr_oof, ggplot2::aes(x = pr, fill = as.factor(y))) +
#       #     geom_histogram(bins = 30, alpha = 0.4, position="dodge") +
#       #     geom_vline(ggplot2::aes(xintercept = cut_off_ba), linetype = "dashed", colour = "turquoise3") +
#       #     annotate("text", x = cut_off_ba + 0.075, y = 0, colour = "turquoise3",
#       #              label = paste0("cut off ba = ", cut_off_ba)) +
#       #     geom_vline(ggplot2::aes(xintercept = cut_off_a), linetype = "dashed", colour = "darkorchid1") +
#       #     annotate("text", x = cut_off_a + 0.07, y = 0, colour = "darkorchid1",
#       #              label = paste0("cut off a = ", cut_off_a)) +
#       #     labs(title = "Class distributions", x = "Probability (y = 1)", y = "Count", fill = "y")
#       # }
#       ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                              "Best Model/Class distributions.png"),
#                       width = 9.92, height = 5.3)
#     } else {
#       # "multilabel"
#       # https://www.datascienceblog.net/post/machine-learning/performance-measures-multi-class-problems/
#       #
#       # calculate ROC-curve
#       suppressMessages(
#         ROC <- pROC::multiclass.roc(train_pr_oof$y,
#                                     train_pr_oof$pr,
#                                     levels = levels(factor(train_pr_oof$y))) # direction = "<"
#       )
#       # additionally compute ROC-curve for "multi:softprob"
#       # WARNING: AUC is better/correcter with "multi:softprob" !!!
#       if (ncol(train_pr_oof) > 2) {
#         suppressMessages(
#           ROC_prob <- pROC::multiclass.roc(train_pr_oof$y,
#                                            train_pr_oof[, 3:ncol(train_pr_oof)] %>%
#                                              stats::setNames(0:(ncol(.) - 1)))
#         )
#       }
#       #
#       # calculate ROC-curve binary (NOT USED FOR NOW)
#       # auc_bin <- data.frame()
#       # for (i in levels(factor(train_pr_oof$y))) {
#       #   auc <- pROC::multiclass.roc(dplyr::if_else(train_pr_oof$y == i, 1, 0),
#       #                               dplyr::if_else(train_pr_oof$pr == i, 1, 0))$auc %>%  # direction = "<"
#       #     as.numeric()
#       #   auc_bin <- dplyr::bind_rows(auc_bin, data.frame(auc = auc))
#       #   rm(auc)
#       # }; rm(i)
#       #
#       # confusion matrix
#       confusion_matrix <- caret::confusionMatrix(factor(train_pr_oof$pr,
#                                                         levels = levels(factor(train_pr_oof$y))),
#                                                  factor(train_pr_oof$y))
#       print(confusion_matrix)
#       sink(paste0(path_output, gsub(".csv", "/", lauf),
#                   "Best Model/Confusion matrix.txt"), append = FALSE)
#       print(confusion_matrix)
#       print(confusion_matrix[["byClass"]])
#       sink()
#       # plot confusion matrix
#       SRfunctions::SR_mosaicplot(var1 = factor(train_pr_oof$y),
#                                  var2 = factor(train_pr_oof$pr,
#                                                levels = levels(factor(train_pr_oof$y))))
#       ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                              "Best Model/Confusion Matrix.png"),
#                       width = 9.92, height = 5.3)  # 4.67
#       #
#       # calculate mcc
#       # ROCR currently supports only evaluation of binary classification tasks !!!
#       # mcc <- mcc(train_pr_oof$pr, train_pr_oof$y)
#       #
#       # get binary ROC curves
#       ROC_bin <- data.frame()
#       if (exists("ROC_prob")) {
#         for (i in 1:length(ROC_prob[['rocs']])) {
#           suppressMessages({
#             ROC_bin <- dplyr::bind_rows(ROC_bin,
#                                         data.frame(binary_ROC = toString(ROC_prob[['rocs']][[i]][[1]][["levels"]]),
#                                                    binary_AUC = as.numeric(pROC::auc(ROC_prob[["rocs"]][[i]][[1]][["response"]],
#                                                                                      ROC_prob[["rocs"]][[i]][[1]][["predictor"]])),
#                                                    # binary_AUC = as.numeric(pROC::auc(ROC_prob[["rocs"]][[i]][[1]][["response"]],
#                                                    #                                   ifelse(ROC_prob[["rocs"]][[i]][[1]][["predictor"]] > 0.5, 1, 0))),
#                                                    thresholds = ROC_prob[['rocs']][[i]][[1]][["thresholds"]],
#                                                    tpr = ROC_prob[['rocs']][[i]][[1]][["sensitivities"]],
#                                                    fpr = 1 - ROC_prob[['rocs']][[i]][[1]][["specificities"]],
#                                                    no = length(ROC_prob[['rocs']][[i]][[1]][["response"]]),
#                                                    no_y_1 = length(ROC_prob[['rocs']][[i]][[1]][["controls"]]),
#                                                    stringsAsFactors = FALSE))
#           })
#         }
#       } else {
#         # WARNING: depends on ROC with "multi:softmax" which is worse than "multi:softprob"
#         for (i in 1:length(ROC[['rocs']])) {
#           suppressMessages({
#             ROC_bin <- dplyr::bind_rows(ROC_bin,
#                                         data.frame(binary_ROC = toString(ROC[['rocs']][[i]][["levels"]]),
#                                                    binary_AUC = as.numeric(pROC::auc(ROC[["rocs"]][[i]][["response"]],
#                                                                                      ROC[["rocs"]][[i]][["predictor"]])), # direction = "<" ???
#                                                    thresholds = ROC[['rocs']][[i]][["thresholds"]],
#                                                    tpr = ROC[['rocs']][[i]][["sensitivities"]],
#                                                    fpr = 1 - ROC[['rocs']][[i]][["specificities"]],
#                                                    no = length(ROC[['rocs']][[i]][["response"]]),
#                                                    no_y_1 = length(ROC[['rocs']][[i]][["controls"]]),
#                                                    stringsAsFactors = FALSE))
#           })
#         }
#       }; rm(i)
#       ROC_bin <- ROC_bin %>%
#         dplyr::mutate(binary = paste0(binary_ROC,
#                                       " (AUC = ", round(binary_AUC, 2) %>% format(., nsmall = 2),
#                                       ", data = ", no, ")"))
#       #
#       # print ROC-curve
#       try({
#         ggplot2::ggplot(ROC_bin, ggplot2::aes(x = fpr, y = tpr,
#                                               colour = stats::reorder(binary, -binary_AUC))) +
#           ggplot2::geom_line() +
#           ggplot2::geom_abline(intercept = 0, slope = 1, color = "gray", linewidth = 1,
#                                linetype = "dashed") +
#           ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(5),
#                                       limits = c(0, 1)) +
#           ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5),
#                                       limits = c(0, 1)) +
#           ggplot2::labs(title = "ROC", colour = "Binary AUC",
#                         x = "Rate of false positives", y = "Rate of true positives",
#                         subtitle = paste0("AUC:                  ",
#                                           round(ifelse(exists("ROC_prob"),
#                                                        as.numeric(ROC_prob$auc),
#                                                        as.numeric(ROC$auc)), 3) %>%
#                                             format(., nsmall = 3),
#                                           # "\nMCC:                  ",
#                                           # paste0(round(mcc$mcc, 3),
#                                           #        " (cutoff = ",
#                                           #        round(mcc$opt_cutoff, 3), ")"),
#                                           "\nAccuracy:           ",
#                                           round(as.numeric(confusion_matrix$overall[1]), 3) %>%
#                                             format(., nsmall = 3),
#                                           "\nPrecision:           ",
#                                           toString(round(as.numeric(confusion_matrix$byClass[, 5]), 2) %>%
#                                                      format(., nsmall = 2)),
#                                           "\nSensitivity/TPR:  ",
#                                           toString(round(as.numeric(confusion_matrix$byClass[, 1]), 2) %>%
#                                                      format(., nsmall = 2)),
#                                           "\nSpecificity/TNR:  ",
#                                           toString(round(as.numeric(confusion_matrix$byClass[, 2]), 2) %>%
#                                                      format(., nsmall = 2)))) +
#           ggplot2::theme(legend.text = ggplot2::element_text(size = 6))
#         ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf), "Best Model/ROC.png"),
#                         width = 9.92, height = 5.3)  # 4.67
#       })
#     }
#     #
#     #
#     #
#     #### Residual drift ####
#     #
#     if (exists("test_pr")) {
#       train_temp <- train_pr_oof %>%
#         dplyr::mutate(residuals = y - pr)
#       test_temp <- test_pr %>%
#         dplyr::mutate(residuals = y_test - pr_test) %>%
#         dplyr::rename(y = y_test, pr = pr_test)
#       drift_residual <- SRxgboost_calculate_drift(train_temp, test_temp, n_plots = 10,
#                                                   name = paste0(path_output,
#                                                                 gsub(".csv", "", lauf),
#                                                                 "/Best Model/"))
#     }
#     #
#     #
#     #
#     #### Lift Chart ####
#     #
#     if (objective %in% c("regression", "classification")) {
#       train_pr_oof$group <- cut(train_pr_oof$pr,
#                                 breaks = unique(stats::quantile(train_pr_oof$pr,
#                                                                 probs = seq(0, 1.01, by = 1/20),
#                                                                 na.rm = TRUE)),
#                                 ordered_result = TRUE, dig.lab = 10,
#                                 include.lowest = TRUE, labels = FALSE)
#       temp <- train_pr_oof %>%
#         dplyr::group_by(group) %>%
#         dplyr::summarise(Actual = mean(y), Predicted = mean(pr))
#       train_pr_oof$group <- NULL
#       temp <- reshape2::melt(temp, id.vars = "group")
#       #
#       ggplot2::ggplot(temp, ggplot2::aes(x = group, y = value, colour = variable)) +
#         ggplot2::geom_point() +
#         ggplot2::geom_line() +
#         ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
#         ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
#         ggplot2::labs(title = "Lift Chart", y = "y", x = "Sorted Predictions", colour = "")
#       ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                              "Best Model/Lift Chart.png"),
#                       width = 9.92, height = 5.3)  # 4.67
#       rm(temp)
#     } else {
#       # "multilabel
#       # add baseline of random group
#       set.seed(12345)
#       train_pr_oof <- train_pr_oof %>%
#         dplyr::mutate(random = sample(train_pr_oof$y, nrow(train_pr_oof), replace = FALSE),
#                       model_correct = dplyr::if_else(y == pr, 1, 0),
#                       baseline_correct = dplyr::if_else(y == random, 1, 0))
#       set.seed(Sys.time())
#       #
#       temp <- dplyr::full_join(
#         # calculate precision for model
#         train_pr_oof %>%
#           dplyr::count(y, model_correct) %>%
#           dplyr::group_by(y) %>%
#           dplyr::mutate(model_precision = n / sum(n)) %>%
#           dplyr::filter(model_correct == 1) %>%
#           dplyr::select(y, model = model_precision),
#         # calculate precision for baseline
#         train_pr_oof %>%
#           dplyr::count(y, baseline_correct) %>%
#           dplyr::group_by(y) %>%
#           dplyr::mutate(baseline_precision = n / sum(n)) %>%
#           dplyr::filter(baseline_correct == 1) %>%
#           dplyr::select(y, baseline = baseline_precision),
#         by = "y") %>%
#         dplyr::ungroup() %>%
#         dplyr::mutate_at(dplyr::vars(model, baseline), ~tidyr::replace_na(., 0)) %>%
#         dplyr::mutate(y = as.character(y),
#                       lift_factor = dplyr::if_else(baseline != 0, model / baseline, 0))
#       #
#       temp %>%
#         dplyr::select(-lift_factor) %>%
#         reshape2::melt(id.vars = "y") %>%
#         ggplot2::ggplot(ggplot2::aes(x = y, y = value, colour = variable, group = variable)) +
#         ggplot2::geom_point() +
#         ggplot2::geom_line() +
#         ggplot2::annotate(geom = "text", x = temp$y, y = temp$model,
#                           label = paste0("lift =\n", round(temp$lift_factor, 1) %>%
#                                            format(., nsmall = 1)),
#                           hjust = "center", vjust = "top", size = 3) +
#         # ggplot2::geom_label(data = temp,
#         #                     ggplot2::aes(x = y, y = model, label = round(lift_factor, 1)))
#         ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6), limits = c(0, 1)) +
#         ggplot2::labs(title = "Lift Chart", y = "Precision", x = "Class", colour = "")
#       ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                              "Best Model/Lift Chart.png"),
#                       width = 9.92, height = 5.3)  # 4.67
#       rm(temp)
#     }
#     #
#     #
#     #
#     #### 1way graphics ####
#     #
#     ##### variable importance ####
#     importance_matrix <- xgboost::xgb.importance(feature_names = colnames(train_mat),
#                                                  model = bst)
#     importance_matrix <- importance_matrix %>%
#       dplyr::select(Feature, Gain, Frequency)  %>%
#       dplyr::arrange(-Gain) %>%
#       dplyr::mutate(Feature = factor(Feature,
#                                      levels = rev(stats::reorder(importance_matrix$Feature,
#                                                                  importance_matrix$Gain))))
#     # plot
#     importance_matrix %>%
#       dplyr::slice(1:(min(nrow(importance_matrix), 30))) %>%
#       reshape2::melt(id.vars = "Feature") %>%
#       ggplot2::ggplot(ggplot2::aes(y = value, x = Feature)) +
#       ggplot2::geom_bar(stat = "identity") +
#       ggplot2::labs(x = "Variable", y = "") +
#       ggplot2::scale_y_continuous(labels = scales::percent,
#                                   breaks = scales::pretty_breaks(5)) +
#       ggplot2::facet_grid(~variable, scales = "free") +
#       ggplot2::coord_flip()
#     ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                            "Best Model/VarImp 0.png"),
#                     width = 9.92, height = 5.3)  # 4.67
#     # save table
#     assign('importance_matrix', importance_matrix, envir = .GlobalEnv)
#     utils::write.table(importance_matrix,
#                        paste0(path_output, gsub(".csv", "/", lauf),
#                               "Best Model/VarImp 0.csv"),
#                        row.names = FALSE, col.names = TRUE, append = FALSE,
#                        sep = ";", dec = ",")
#     #
#     message(substr(as.character(Sys.time()), 1, 19))
#     ##### Partial Dependence Plots ####
#     #
#     if (pdp_plots) {
#       temp <- importance_matrix %>%
#         dplyr::filter(Gain >= pdp_min_rel_Gain) %>%
#         dplyr::mutate(Feature = as.character(Feature))
#       #
#       if (nrow(temp) >= 1) {
#         # run dpd::partial in parallel
#         if (nrow(datenModell_eval) > 1000 & pdp_parallel) {
#           if (exists("n_cores")) {
#             pdp_n_core <- n_cores
#           } else {
#             pdp_n_core <- parallel::detectCores() - 1 # min(parallel::detectCores() - 1, 6)
#           }
#           cl <- parallel::makeCluster(pdp_n_core)
#           doParallel::registerDoParallel(cl)
#           # parallel::clusterEvalQ(cl, library(stats)) # ?
#         }
#         #
#         pdpFunction <- function(i) {
#           try({
#             # downsample datenModell if nrow > pdp_sample
#             if (nrow(datenModell_eval) > pdp_sample) {
#               set.seed(12345)
#               datenModell_eval_ <- datenModell_eval %>% dplyr::sample_n(pdp_sample)
#               set.seed(12345)
#               y_ <- data.frame(y = y_test_eval) %>% dplyr::sample_n(pdp_sample)
#               set.seed(Sys.time())
#               test_eval_mat_ <- Matrix::sparse.model.matrix(~. - 1, data = datenModell_eval_)
#               pr_ <- data.frame(pr = stats::predict(bst_1fold, test_eval_mat_))
#             } else {
#               datenModell_eval_ <- datenModell_eval
#               y_ <- data.frame(y = y_test_eval)
#               test_eval_mat_ <- test_eval_mat
#               pr_ <- data.frame(pr = stats::predict(bst_1fold, test_eval_mat))
#             }
#             #
#             # if (y_multiclass == FALSE) {   # linear, binary
#             x <- min(stringdist::stringdist(temp$Feature[i], names(datenModell_eval_)))
#             #
#             if (x > 0) {       # x is factor
#               x <- names(datenModell_eval_)[which.min(stringdist::stringdist(
#                 temp$Feature[i], names(datenModell_eval_), method = "lcs"))]
#               xlabel <- x
#               xlabel2 <- gsub(x, "", temp$Feature[i])
#               stats <- data.frame(x = datenModell_eval_[, xlabel] == xlabel2,
#                                   Actual = y_$y, Predicted = pr_$pr) %>%
#                 dplyr::group_by(x) %>%
#                 dplyr::summarise(Count = dplyr::n(),
#                                  Predicted = mean(Predicted),
#                                  Actual = mean(Actual))
#               stats <- stats %>%
#                 dplyr::left_join(partial,
#                                  by = c("x_orig" = xlabel)) %>%
#                 dplyr::rename("Partial_Dependence" = "yhat")
#               stats_long <- reshape2::melt(stats[, c(3, 5:7)], id.vars = "x_orig")
#               xlabel <- paste0(x, " = ", xlabel2)
#               p1 <- ggplot2::ggplot(stats_long,
#                                     ggplot2::aes(x = x, y = value, colour = variable)) +
#                 ggplot2::geom_point(size = 2) +
#                 ggplot2::labs(x = xlabel, y = "Value") +
#                 ggplot2::theme(legend.position = "top",
#                                legend.title = ggplot2::element_blank()); p1
#               p2 <- ggplot2::ggplot(stats, ggplot2::aes(x = x, y = Count)) +
#                 ggplot2::geom_bar(stat = "identity", position = "dodge") +
#                 ggplot2::labs(x = xlabel); p2
#               p <- gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(0.75, 0.25)); p
#             } else {
#               # x is numeric, integer or Date
#               xlabel <- temp$Feature[i]
#               # check if x = as.numeric(date)
#               if (sum(is.na(as.Date(as.character(datenModell_eval_[, xlabel]),
#                                     format = "%Y%m%d"))) == 0) {
#                 xx <- as.Date(as.character(datenModell_eval_[, xlabel]), format = "%Y%m%d")
#               } else {
#                 xx <- datenModell_eval_[, xlabel]
#               }
#               # check if x = as.numeric(factor), set number of cuts
#               if (gsub("_LabelEnc", "", temp$Feature[i]) %in% factor_encoding$feature) {
#                 xx <- factor(xx)
#                 levels(xx) <- factor_encoding$levels[factor_encoding$feature ==
#                                                        gsub("_LabelEnc", "", temp$Feature[i])]
#                 # cuts <- length(unique(xx))
#                 # summarise results
#                 stats <- data.frame(x = xx, x_orig = datenModell_eval_[, xlabel],
#                                     Actual = y_$y, Predicted = pr_$pr) %>%
#                   dplyr::mutate(Group = xx) %>%
#                   dplyr::group_by(Group) %>%
#                   dplyr::summarise(x = x[1],
#                                    x_orig = mean(x_orig),
#                                    Count = dplyr::n(),
#                                    Predicted = mean(Predicted),
#                                    Actual = mean(Actual))
#               } else {
#                 # summarise results
#                 if (is.null(pdp_cuts))
#                   pdp_cuts <- min(round(length(xx) / pdp_int_cuts_sample), 40)
#                 stats <- data.frame(x = xx, x_orig = datenModell_eval_[, xlabel],
#                                     Actual = y_$y, Predicted = pr_$pr) %>%
#                   dplyr::mutate(Group = cut(x, breaks = pretty(x, pdp_cuts),
#                                             include.lowest = TRUE, dig.lab = 10)) %>%
#                   # dplyr::mutate(Group = cut(x_orig, breaks = unique(quantile(x_orig, seq(0, 1.01, 0.02))),
#                   #                    include.lowest = TRUE, dig.lab = 10)) %>%
#                   dplyr::group_by(Group) %>%
#                   dplyr::summarise(x = mean(x),
#                                    x_orig = mean(x_orig),
#                                    Count = dplyr::n(),
#                                    Predicted = mean(Predicted),
#                                    Actual = mean(Actual))
#               }
#               #
#               ## partial dependence
#               #
#               if (pdp_parallel) {
#                 # in parallel
#                 partial <- pdp::partial(bst_1fold,
#                                         pred.var = temp$Feature[i],
#                                         pred.grid = data.frame(stats$x_orig) %>%
#                                           stats::setNames(xlabel),
#                                         # grid.resolution = 30,
#                                         train = test_eval_mat_,
#                                         plot = FALSE, chull = TRUE, type = "regression",
#                                         # type = ifelse(objective == "multilabel",
#                                         #               "classification", objective))
#                                         parallel = TRUE,
#                                         paropts = list(.packages = "xgboost"))
#               } else {
#                 # single core
#                 partial <- pdp::partial(bst_1fold,
#                                         pred.var = temp$Feature[i],
#                                         pred.grid = data.frame(stats$x_orig) %>%
#                                           stats::setNames(xlabel),
#                                         # grid.resolution = 30,
#                                         train = test_eval_mat_,
#                                         plot = FALSE, chull = TRUE, type = "regression")
#                 # type = ifelse(objective == "multilabel",
#                 #               "classification", objective))
#               }
#               #
#               stats <- stats %>%
#                 dplyr::left_join(partial, by = c("x_orig" = xlabel)) %>%
#                 dplyr::rename("Partial_Dependence" = "yhat")
#               #
#               # evtl. noch skalieren !!!
#               #
#               stats_long <- reshape2::melt(stats %>% dplyr::select(x, 5:7), id.vars = "x")
#               #
#               p1 <- ggplot2::ggplot(stats_long,
#                                     ggplot2::aes(x = x, y = value, colour = variable)) +
#                 ggplot2::geom_point(size = 2) +
#                 ggplot2::labs(x = "", y = "Value") +
#                 # ggplot2::labs(x = gsub("_LabelEnc", "", xlabel), y = "Value") +
#                 ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
#                 ggplot2::theme(legend.position = "top",
#                                legend.title = ggplot2::element_blank())
#               if ((gsub("_LabelEnc", "",  temp$Feature[i]) %in% factor_encoding$feature)) {
#                 # if (cuts > 8) {                                               # factor
#                 #   p1 <- p1 + theme(axis.text.x = element_text(angle = 90,
#                 #                                               hjust = 1, vjust = 0.3))
#                 # }
#               } else if (SRfunctions::SR_is_date(stats_long$x)) {               # date
#                 spanne = as.numeric(difftime(max(stats_long$x), min(stats_long$x),
#                                              units = "days"))
#                 if (spanne > 365.25 * 20) {
#                   p1 <- p1 +
#                     ggplot2::scale_x_date(date_breaks = "10 years",
#                                           date_labels = "%Y",
#                                           date_minor_breaks = "1 year")
#                 } else if (spanne > 365.25 * 3) {
#                   p1 <- p1 +
#                     ggplot2::scale_x_date(date_breaks = "year",
#                                           date_labels = "%Y",
#                                           date_minor_breaks = "3 months")
#                 } else if (spanne > 365.25) {
#                   p1 <- p1 +
#                     ggplot2::scale_x_date(date_breaks = "3 months",
#                                           date_labels = "%Y-%m",
#                                           date_minor_breaks = "1 month")
#                 } else if (spanne > 30) {
#                   p1 <- p1 +
#                     ggplot2::scale_x_date(date_breaks = "1 month",
#                                           date_labels = "%Y-%m-%d",
#                                           date_minor_breaks = "1 week")
#                 } else {
#                   p1 <- p1 +
#                     ggplot2::scale_x_date(date_breaks = "1 week",
#                                           date_labels = "%Y-%m-%d",
#                                           date_minor_breaks = "1 day")
#                 }
#               } else {
#                 p1 <- p1 +
#                   ggplot2::geom_line(linewidth = I(1)) +                        # numeric
#                   ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8))
#               }; p1
#               p2 <- ggplot2::ggplot(stats, ggplot2::aes(x = x, y = Count)) +
#                 ggplot2::geom_bar(stat = "identity", position = "dodge", orientation = "x") +
#                 ggplot2::labs(x = gsub("_LabelEnc", "", xlabel))
#               if ((gsub("_LabelEnc", "", temp$Feature[i]) %in% factor_encoding$feature)) {
#                 # if (cuts > 8) {                                               # factor
#                 #   p1 <- p1 + theme(axis.text.x = element_text(angle = 90,
#                 #                                               hjust = 1, vjust = 0.3))
#                 # }
#               } else if (SRfunctions::SR_is_date(stats_long$x)) {               # date
#                 if (spanne > 365.25 * 20) {
#                   p2 <- p2 +
#                     ggplot2::scale_x_date(date_breaks = "10 years",
#                                           date_labels = "%Y",
#                                           date_minor_breaks = "1 year")
#                 } else if (spanne > 365.25 * 3) {
#                   p2 <- p2 +
#                     ggplot2::scale_x_date(date_breaks = "year",
#                                           date_labels = "%Y",
#                                           date_minor_breaks = "3 months")
#                 } else if (spanne > 365.25) {
#                   p2 <- p2 +
#                     ggplot2::scale_x_date(date_breaks = "3 months",
#                                           date_labels = "%Y-%m",
#                                           date_minor_breaks = "1 month")
#                 } else if (spanne > 30) {
#                   p2 <- p2 +
#                     ggplot2::scale_x_date(date_breaks = "1 month",
#                                           date_labels = "%Y-%m-%d",
#                                           date_minor_breaks = "1 week")
#                 } else {
#                   p2 <- p2 +
#                     ggplot2::scale_x_date(date_breaks = "1 week",
#                                           date_labels = "%Y-%m-%d",
#                                           date_minor_breaks = "1 day")
#                 }
#               } else {
#                 p2 <- p2 +
#                   ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) # numeric
#               }; p2
#               p <- gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(0.75, 0.25)); p
#             }
#             #
#             # simple PDP-graphic
#             p3 <- ggplot2::ggplot(stats,
#                                   ggplot2::aes(x = x, y = Partial_Dependence)) +
#               ggplot2::geom_point(size = 2, colour = "steelblue") +
#               ggplot2::labs(x = gsub("_LabelEnc", "", xlabel), y = "Value") +
#               ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6))
#             if ((gsub("_LabelEnc", "",  temp$Feature[i]) %in% factor_encoding$feature)) {
#               # if (cuts > 8) {                                               # factor
#               #   p3 <- p3 + theme(axis.text.x = element_text(angle = 90,
#               #                                               hjust = 1, vjust = 0.3))
#               # }
#             } else if (SRfunctions::SR_is_date(stats$x)) {               # date
#               spanne = as.numeric(difftime(max(stats$x), min(stats$x),
#                                            units = "days"))
#               if (spanne > 365.25 * 20) {
#                 p3 <- p3 +
#                   ggplot2::scale_x_date(date_breaks = "10 years",
#                                         date_labels = "%Y",
#                                         date_minor_breaks = "1 year")
#               } else if (spanne > 365.25 * 3) {
#                 p3 <- p3 +
#                   ggplot2::scale_x_date(date_breaks = "year",
#                                         date_labels = "%Y",
#                                         date_minor_breaks = "3 months")
#               } else if (spanne > 365.25) {
#                 p3 <- p3 +
#                   ggplot2::scale_x_date(date_breaks = "3 months",
#                                         date_labels = "%Y-%m",
#                                         date_minor_breaks = "1 month")
#               } else if (spanne > 30) {
#                 p3 <- p3 +
#                   ggplot2::scale_x_date(date_breaks = "1 month",
#                                         date_labels = "%Y-%m-%d",
#                                         date_minor_breaks = "1 week")
#               } else {
#                 p3 <- p3 +
#                   ggplot2::scale_x_date(date_breaks = "1 week",
#                                         date_labels = "%Y-%m-%d",
#                                         date_minor_breaks = "1 day")
#               }
#             } else {
#               p3 <- p3 +
#                 ggplot2::geom_line(linewidth = I(1), colour = "steelblue") +    # numeric
#                 ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8))
#             }
#             #
#             # save graphic
#             xlabel <- gsub("[[:punct:]]", "", xlabel)
#             try(ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                                        "Best Model/VarImp ", i, " ",
#                                        gsub("LabelEnc", "", xlabel), ".png"),
#                                 plot = p, width = 9.92, height = 5.3))  # 4.67
#             try(rm(p), TRUE)
#             try(ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                                        "Best Model/VarImp ", i, " ",
#                                        gsub("LabelEnc", "", xlabel), " PDP.png"),
#                                 plot = p3, width = 9.92, height = 5.3))  # 4.67
#             try(rm(p3), TRUE)
#             # save summary table
#             utils::write.table(stats, paste0(path_output, gsub(".csv", "/", lauf),
#                                              "Best Model/VarImp ", i, " ",
#                                              gsub("LabelEnc", "", xlabel), ".csv"),
#                                row.names = FALSE, sep = ";")
#           })
#         }
#         #
#         print(paste0("Plot PDPs in parallel for: ",
#                      paste0(temp$Feature, collapse = ", ")))
#         # plot.list <- lapply(1:nrow(temp), pdpFunction)
#         #
#         if (exists("n_cores")) {
#           pdp_n_core <- n_cores
#         } else {
#           pdp_n_core <- parallel::detectCores() - 1 # min(parallel::detectCores() - 1, 6)
#         }
#         cl2 <- parallel::makeCluster(pdp_n_core)
#         assign("temp", temp, envir = .GlobalEnv)
#         assign("pdpFunction", pdpFunction, envir = .GlobalEnv)
#         parallel::clusterExport(cl2, varlist = c("temp", "pdpFunction",
#                                                  "datenModell_eval", "y_test_eval",
#                                                  "factor_encoding", "path_output"))
#         parallel::clusterEvalQ(cl2, library(dplyr))
#         plot.list <- parallel::parLapply(cl2, 1:nrow(temp), pdpFunction)
#         parallel::stopCluster(cl2)
#         rm(temp, pdpFunction)
#         #
#         # Plot PDP-summary of all important variables
#         files <- list.files(path = paste0(path_output, gsub(".csv", "/", lauf),
#                                           "/Best Model/"),
#                             pattern = "VarImp .*.csv", full.names = TRUE) %>%
#           data.frame(filename = .) %>%
#           dplyr::filter(!grepl("VarImp 0.csv", filename)) %>%
#           dplyr::mutate(Variable =
#                           SRfunctions::SR_trim_text(substr(basename(filename),
#                                                            10, nchar(basename(filename)) - 4)),
#                         Variable = paste0(row_number(), " ", Variable))
#         #
#         df_temp <- data.frame()
#         for (i in 1:nrow(files)) {
#           df_temp <- dplyr::bind_rows(
#             df_temp,
#             rio::import(files$filename[i]) %>%
#               dplyr::mutate(Group = factor(Group,
#                                            levels = rio::import(files$filename[i])$Group),
#                             x = as.character(x),
#                             Variable = files$Variable[i],
#                             .before = 1))
#         }; rm(i, files)
#         saveRDS(df_temp, paste0(path_output, gsub(".csv", "/", lauf),
#                                 "/Best Model/VarImp 0 Alle PDP.rds"))
#         #
#         df_temp %>%
#           ggplot2::ggplot(ggplot2::aes(x = Group, y = Partial_Dependence, group = Variable)) +
#           ggplot2::geom_point(colour = "steelblue") +
#           ggplot2::geom_line(colour = "steelblue") +
#           ggplot2::facet_wrap(vars(Variable), scales = "free_x") +
#           ggplot2::labs(title = "Partial Dependence Plots")
#         ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                                "/Best Model/VarImp 0 Alle PDP.png"),
#                         width = 9.92, height = 5.3)
#         #
#         # plot of selected variables
#         if (TRUE) {
#           # lauf <- "XGB_2023_AnteilAB_v11"
#           # df_temp <- readRDS(paste0(path_output, gsub(".csv", "/", lauf),
#           #                           "/Best Model/VarImp 0 Alle PDP.rds"))
#           df_temp %>% dplyr::count(Variable)
#           suppressWarnings({
#             df_temp %>%
#               # select x-variables with a similar range:
#               dplyr::filter(Variable %in% c(unique(df_temp$Variable)[1:5])) %>%
#               # dplyr::mutate(Variable = Variable %>%
#               #                 sub("KD", "Kunden ", .) %>%
#               #                 sub("ANT", " Anteil", .)) %>%
#               dplyr::mutate(x = as.numeric(x)) %>%
#               ggplot2::ggplot(ggplot2::aes(x = x, y = Partial_Dependence,
#                                            group = Variable, colour = Variable)) +
#               ggplot2::geom_point() +
#               ggplot2::geom_line() +
#               ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6),
#                                           labels = scales::format_format(big.mark = "'"),
#                                           transform = "log1p") +
#               # transform = scales::transform_yj()) +
#               ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6),
#                                           # labels = scales::percent) +
#                                           labels = scales::format_format(big.mark = "'")) +
#               ggplot2::labs(title = "Partial Dependence Plot", x = "x", y = "y")
#             ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                                    "/Best Model/VarImp 0 Alle PDP 2.png"),
#                             width = 9.92, height = 5.3)
#           })
#         }
#         #
#         rm(df_temp)
#       }
#     }   # end of 'pdp_plots'
#     #
#     #
#     message(substr(as.character(Sys.time()), 1, 19))
#     #### 2way graphics ####
#     #
#     #####  variable importance ####
#     #
#     # downsample datenModell (because EIX::interactions is very slow)
#     if (nrow(datenModell) > pdp_int_sample) {
#       set.seed(12345)
#       datenModell_ <- datenModell %>% dplyr::sample_n(pdp_int_sample)
#       train_mat_ <- Matrix::sparse.model.matrix(~. - 1, data = datenModell_)
#       set.seed(Sys.time())
#     } else {
#       datenModell_ <- datenModell
#       train_mat_ <- train_mat
#     }
#     # downsample datenModell_eval (because pdp::partial is very slow)
#     if (nrow(datenModell_eval) > pdp_int_sample) {
#       set.seed(12345)
#       datenModell_eval_ <- datenModell_eval %>% dplyr::sample_n(pdp_int_sample)
#       set.seed(12345)
#       y_ <- data.frame(y = y_test_eval) %>% dplyr::sample_n(pdp_int_sample)
#       set.seed(Sys.time())
#       test_eval_mat_ <- Matrix::sparse.model.matrix(~. - 1, data = datenModell_eval_)
#       pr_ <- data.frame(pr = stats::predict(bst_1fold, test_eval_mat_))
#     } else {
#       datenModell_eval_ <- datenModell_eval
#       y_ <- data.frame(y = y_test_eval)
#       test_eval_mat_ <- test_eval_mat
#       pr_ <- data.frame(pr = stats::predict(bst_1fold, test_eval_mat))
#     }
#     #
#     # calculate most important interaction pairs Parent/Child
#     interactions <- EIX::interactions(bst, train_mat_, option = "pairs")
#     # interactions <- EIX::interactions(bst, train_mat_, option = "interactions")
#     # plot(interactions)
#     #
#     # clean up stats
#     interactions_clean <- data.frame(interactions) %>%
#       dplyr::left_join(importance_matrix %>%
#                          dplyr::mutate(Parent_Rang = 1:nrow(.)) %>%
#                          dplyr::select(Feature, Parent_Rang),
#                        by = c("Parent" = "Feature")) %>%
#       dplyr::left_join(importance_matrix %>%
#                          dplyr::mutate(Child_Rang = 1:nrow(.)) %>%
#                          dplyr::select(Feature, Child_Rang),
#                        by = c("Child" = "Feature")) %>%
#       dplyr::mutate(Parent_clean = ifelse(Parent_Rang < Child_Rang, Parent, Child),
#                     Child_clean = ifelse(Parent_Rang > Child_Rang, Parent, Child),
#                     Parent = Parent_clean,
#                     Child = Child_clean) %>%
#       dplyr::select(-Parent_clean, -Parent_Rang, -Child_clean, -Child_Rang) %>%
#       dplyr::group_by(Parent, Child) %>%
#       dplyr::summarise(sumGain = sum(sumGain),
#                        frequency = sum(frequency),
#                        .groups = "drop") %>%
#       dplyr::arrange(-sumGain) %>%
#       dplyr::mutate(Gain = sumGain / sum(sumGain),
#                     Frequency = frequency / sum(frequency),
#                     Feature = paste0(Parent, " - ", Child))
#     utils::write.table(interactions_clean,
#                        paste0(path_output, gsub(".csv", "/", lauf),
#                               "Best Model/VarImpInt 0.csv"),
#                        row.names = FALSE, col.names = TRUE, append = FALSE,
#                        sep = ";", dec = ",")
#     #
#     # importance <- EIX::importance(bst, train_mat_, option = "interactions")
#     # importance <- EIX::importance(bst, train_mat_, option = "both")
#     # plot(importance, top = 5)
#     # plot(importance, top = 5, radar = FALSE)
#     # head(importance)
#     #
#     # plot
#     interactions_clean %>%
#       dplyr::slice(1:(min(nrow(interactions_clean), 30))) %>%
#       dplyr::select(Feature, Gain, Frequency) %>%
#       reshape2::melt(id.vars = "Feature") %>%
#       ggplot2::ggplot(ggplot2::aes(x = reorder(Feature, value), y = value)) +
#       ggplot2::geom_bar(stat = "identity") +
#       ggplot2::labs(x = "Variable", y = "") +
#       ggplot2::scale_y_continuous(labels = scales::percent,
#                                   breaks = scales::pretty_breaks(5)) +
#       ggplot2::facet_grid(~variable, scales = "free") +
#       ggplot2::coord_flip()
#     ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                            "Best Model/VarImpInt 0.png"),
#                     width = 9.92, height = 5.3)  # 4.67
#     #
#     ##### Partial Dependence Plots ####
#     #
#     # also plots possible for class predictions in "multilabel"                 # TODO
#     # https://journal.r-project.org/archive/2017/RJ-2017-016/RJ-2017-016.pdf
#     if (pdp_plots) {
#       if (objective %in% c("regression", "classification")) {
#         temp <- interactions_clean %>%
#           dplyr::filter(Gain >= pdp_min_rel_Gain,
#                         Parent != Child)
#         # add interactions of 3 most important variables (if not existing)
#         temp <- dplyr::full_join(temp,
#                                  importance_matrix %>%
#                                    dplyr::slice(1:3) %>%
#                                    # dplyr::filter(Gain >= pdp_min_rel_Gain) %>%
#                                    dplyr::mutate(Feature = as.character(Feature)) %>%
#                                    dplyr::select(Feature) %>%
#                                    dplyr::cross_join(., .) %>%
#                                    dplyr::slice(c(2, 3, 4, 6)) %>%
#                                    stats::setNames(c("Parent", "Child")) %>%
#                                    dplyr::mutate(Feature = paste0(Parent, " - ", Child)),
#                                  by = c("Parent", "Child", "Feature")) %>%
#           # remove duplicated (but swapped) pairs
#           mutate(sorted_values = purrr::pmap(list(Parent, Child),
#                                       function(...) paste(sort(c(...)), collapse="_"))) %>%
#           distinct(sorted_values, .keep_all = TRUE) %>%
#           select(-sorted_values)
#         #
#         if (nrow(temp) >= 1) {
#           for (i in seq_along(temp$Feature)) {   # i <- 1
#             # print progress
#             print(paste0("Plot ", i, " of ", nrow(temp), ": ", temp$Feature[i]))
#             #
#             try({
#               # calculate stats
#               if (pdp_parallel) {
#                 # in parallel
#                 partial <- pdp::partial(bst_1fold,
#                                         pred.var = c(temp$Parent[i], temp$Child[i]),
#                                         # pred.grid = data.frame(stats$x_orig) %>%
#                                         #   stats::setNames(xlabel),
#                                         # grid.resolution = 30,
#                                         train = test_eval_mat_,
#                                         type = "regression", plot = FALSE,
#                                         parallel = TRUE,
#                                         paropts = list(.packages = "xgboost"))
#               } else {
#                 # single core
#                 partial <- pdp::partial(bst_1fold,
#                                         pred.var = c(temp$Parent[i], temp$Child[i]),
#                                         # pred.grid = data.frame(stats$x_orig) %>%
#                                         #   stats::setNames(xlabel),
#                                         # grid.resolution = 30,
#                                         train = test_eval_mat_,
#                                         type = "regression", plot = FALSE)
#               }
#               # partial <- pdp::partial(bst_1fold,
#               #                         pred.var = c(temp$Parent[i], temp$Child[i]),
#               #                         # pred.grid = data.frame(stats$x_orig) %>%
#               #                         #   stats::setNames(xlabel),
#               #                         # grid.resolution = 30,
#               #                         train = test_eval_mat_,
#               #                         # train = datenModell_eval_,
#               #                         # type = objective,
#               #                         type = "regression",
#               #                         # type = "classification", prob = TRUE,   # error "classification" !!!
#               #                         plot = FALSE)
#               #
#               # save graphic
#               p <- ggplot2::autoplot(partial) +
#                 ggplot2::labs(title = "Partial Dependence Plot")
#               ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                                      "Best Model/VarImpInt ", i, " ",
#                                      gsub("_LabelEnc", "", temp$Feature[i]), ".png"),
#                               plot = p, width = 9.92, height = 5.3)  # 4.67
#               rm(p)
#               #
#               p <- pdp::plotPartial(partial, zlab = "y",
#                                     levelplot = FALSE, drape = TRUE)
#               ggplot2::ggsave(paste0(path_output, gsub(".csv", "/", lauf),
#                                      "Best Model/VarImpInt ", i, " ",
#                                      gsub("_LabelEnc", "", temp$Feature[i]), " 3D.png"),
#                               plot = gridExtra::arrangeGrob(p),
#                               width = 9.92, height = 5.3)  # 4.67
#               rm(p)
#               #
#               # save summary table
#               utils::write.table(partial,
#                                  paste0(path_output, gsub(".csv", "/", lauf),
#                                         "Best Model/VarImpInt ", i, " ",
#                                         gsub("_LabelEnc", "", temp$Feature[i]), ".csv"),
#                                  row.names = FALSE, sep = ";")
#               rm(partial)
#             })
#           }
#         }
#       }
#       # stop cluster for pdp::partial
#       if (pdp_parallel) {
#         try(parallel::stopCluster(cl), TRUE)
#         try(parallel::stopCluster(cl), TRUE)
#         rm(cl, pdp_n_core); invisible(gc())
#       }
#       message(substr(as.character(Sys.time()), 1, 19))
#     }   # end of 'pdp_plots'
#   }   # end of 'plots'
# }
