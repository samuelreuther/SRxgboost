#' SRxgboost_cleanup
#'
#' Removes all standard SRxgboost-objects from global environment.
#'
#' @return NULL
#'
#' @export
SRxgboost_cleanup <- function() {
  suppressWarnings(
    try(rm(datenModell, SummaryCV, train_pr_oof, xgbfi, bst, bst_1fold, dtesting, dtrain,
           dtraining, featureList, lauf, lf, no_folds, res, trainmat, y, data_train,
           datenModelltest, testing, training, data_test, objective, testingmat, trainingmat,
           yname, ytesting, ytraining, testmat, importance_matrix, eval_index,
           nround, eta, obj, metric, runs, nfold, trees, verbose, test_param, save_model,
           run_kfold, save_test_forecast, best_params, d_test, d_test_eval, d_train,
           d_train_eval, test_eval_mat, test_mat, train_eval_mat, train_mat, y_test_eval,
           y_train_eval, factor_encoding, forecast_mat, d_forecast, datenModell_eval,
           datenModell_train, y_test, folds, y_train, y_train_all,
           index_train_eval, index_test_eval,
           y_OOF, OOFforecast, TESTforecast, SummaryCV_META,
           envir = .GlobalEnv), TRUE))
  invisible(gc())
}
