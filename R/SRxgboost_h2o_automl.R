#' SRxgboost_h2o_automl
#'
#' Runs several models with h2o.automl.
#' Install latestest version of h2o:
#' install.packages("h2o", type="source",
#                  repos=(c("http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R")))
#'
#' @param y character
#' @param data_train data.frame
#' @param data_test data.frame
#' @param metric character: "AUC", "RMSE", ...
#' @param kfold integer
#' @param max_runtime_sec integer
#' @param run_shap boolean
#'
#' @return writes a lot of files
#'
#' @export
SRxgboost_h2o_automl <- function(y = NULL,
                                 data_train = NULL,
                                 data_test = NULL,
                                 metric = NULL,
                                 kfold = 5,
                                 max_runtime_sec = 5 * 60,
                                 run_shap = FALSE) {
  # start h2o
  # Error: cannot open the connection, Permission !!!
  Sys.setenv(https_proxy = "")
  Sys.setenv(http_proxy = "")
  Sys.setenv(http_proxy_user = "")
  Sys.setenv(https_proxy_user = "")
  n_core <- parallel::detectCores() - 1 # min(parallel::detectCores() - 1, 6)
  h2o::h2o.init(nthreads = n_core, max_mem_size = '16g') # always verbose!
  #
  # define x, y and convert df to H2OFrame
  train_h2o <- h2o::as.h2o(data_train)
  if (!is.null(data_test)) test_h2o <- h2o::as.h2o(data_test)   # [1:1000, ]
  x <- setdiff(names(train_h2o), y)
  #
  # set default metric if not defined
  if (is.null(metric)) {
    metric <- dplyr::case_when(length(unique(data_train[, y])) == 2 ~ "AUC",
                               length(unique(data_train[, y])) >  2 ~ "RMSE")
  }
  #
  # set y as.factor for classification
  if (length(unique(data_train[, y])) == 2 & class(train_h2o[, y]) != "factor") {
    train_h2o[, y] <- as.factor(train_h2o[, y])
  }
  #
  #
  #
  ### Automatic Machine Learning
  #
  aml <- h2o::h2o.automl(x = x, y = y,
                         training_frame = train_h2o,
                         nfolds = kfold,
                         max_runtime_secs = max_runtime_sec,
                         stopping_metric = metric,
                         stopping_rounds = 20,
                         keep_cross_validation_predictions = TRUE,
                         keep_cross_validation_models = TRUE,
                         seed = 12345)
  #
  # create output dir
  path_output_ <- paste0(path_output, gsub(".csv", "", lauf), "/h2o.automl/")
  if (!dir.exists(path_output_)) {dir.create(path_output_, showWarnings = FALSE, recursive = TRUE)}
  #
  # Extract leaderboard and leader model
  lb <- aml@leaderboard; print(lb)
  saveRDS(as.data.frame(lb), paste0(path_output_, "leaderboard.rds"))
  lb_leader <- aml@leader; lb_leader
  saveRDS(lb_leader, paste0(path_output_, "leaderboard_leader.rds"))
  lb %>%
    as.data.frame() %>%
    utils::head(20) %>%
    # dplyr::mutate(model_id = substr(model_id, 1, regexpr("_AutoML_", model_id) - 1) %>%
    #          gsub("_", " ", .)) %>%
    dplyr::mutate(model_id = stats::reorder(model_id, auc)) %>%
    reshape2::melt(id = "model_id") %>%
    dplyr::filter(variable %in% c("auc", "logloss", "mean_per_class_error", "rmse")) %>%
    dplyr::mutate(variable = gsub("mean_per_class_error", "mean per\nclass error", variable)) %>%
    ggplot2::ggplot(ggplot2::aes(x = model_id, y = value)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(ggplot2::aes(label = round(value, 3)),
                       position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5)) +
    ggplot2::facet_grid(.~variable, scales = "free", space = "free_x") +
    ggplot2::coord_flip()
  ggplot2::ggsave(paste0(path_output_, "leaderboard.png"), width = 9.92, height = 5.3)
  #
  #
  #
  ### save some results of topmodel
  #
  # save topmodel
  topmodel <- as.data.frame(aml@leaderboard$model_id) %>%
    dplyr::filter(!grepl("StackedEnsemble", model_id)) %>%
    dplyr::slice(1) %>%
    dplyr::pull(model_id)
  topmodel <- match(topmodel, as.data.frame(aml@leaderboard$model_id)$model_id)
  model <- h2o::h2o.getModel(aml@leaderboard[topmodel, 1])
  h2o::h2o.saveModel(model, path = path_output_, force = TRUE)
  #
  # Variable importance
  varimp <- h2o::h2o.varimp(model)
  saveRDS(varimp, paste0(path_output_, "varimp_topmodel.rds"))
  #
  varimp %>%
    dplyr::arrange(-percentage) %>%
    dplyr::slice(1:min(30, nrow(.))) %>%
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(variable, percentage), y = percentage)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(x = "Variable", y = "Importance (%)") +
    ggplot2::scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(5)) +
    ggplot2::coord_flip()
  ggplot2::ggsave(paste0(path_output_, "varimp_plot_topmodel.png"), width = 9.92, height = 5.3)
  #
  # partial dependence plots   TODO !!!
  # tbd
  #
  # Shap values
  if (run_shap) {
    shap_TRAIN <- as.data.frame(h2o::h2o.predict_contributions(model, train_h2o))
    saveRDS(shap_TRAIN, paste0(path_output_, "shap_TRAIN_topmodel.rds"))
    if (!is.null(data_test)) {
      shap_TEST <- as.data.frame(h2o::h2o.predict_contributions(model, test_h2o))
      saveRDS(shap_TEST, paste0(path_output_, "shap_TEST_topmodel.rds"))
    }
  }
  # save OOF CV train predictions
  pred_OOF <- as.data.frame(h2o::h2o.cross_validation_holdout_predictions(model))
  saveRDS(pred_OOF, paste0(path_output_, "predictions_OOF_topmodel.rds"))
  pred_TRAIN <- h2o::h2o.cross_validation_predictions(model)
  saveRDS(pred_TRAIN, paste0(path_output_, "predictions_TRAIN_topmodel.rds"))
  #
  # Save Leader Model
  h2o::h2o.saveModel(lb_leader, path = path_output_, force = TRUE)
  # h2o::h2o.download_mojo(lb_leader, path = path_output_)
  #
  # save OOF predictions
  # pred_OOF <- as.data.frame(h2o::h2o.cross_validation_holdout_predictions(lb_leader))
  # saveRDS(pred_OOF, paste0(path_output_, "predictions_OOF.rds"))
  #
  # make predictions on test
  if (!is.null(data_test)) {
    pred_TEST <- as.data.frame(h2o::h2o.predict(lb_leader, test_h2o))
    saveRDS(pred_TEST, paste0(path_output_, "predictions_TEST.rds"))
    utils::write.table(pred_TEST, paste0(path_output_, "predictions_TEST",
                                         " train", round(lb_leader@model[["training_metrics"]]@metrics[[metric]], 4),
                                         " test", round(lb_leader@model[["cross_validation_metrics"]]@metrics[[metric]], 4), ".csv"),
                       row.names = FALSE, sep = ",", dec = ".")
  }
  #
  # clean up
  rm(train_h2o, test_h2o, x, y, aml, lb, lb_leader, pred_TRAIN, pred_OOF, pred_TEST,
     shap_TRAIN, shap_TEST, varimp, metric, topmodel, path_output_); invisible(gc())
  h2o::h2o.shutdown(prompt = FALSE)
}
