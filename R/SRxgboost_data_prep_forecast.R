#' SRxgboost_data_prep_forecast
#'
#' Prepare data for forecasting with XGBOOST model. Creates a sparse matrix and dmatrix.
#'
#' @param df data.frame
#' @param label_encoding boolean
#'
#' @return assigns output into global workspace
#'
#' @export
SRxgboost_data_prep_forecast <- function(df,
                                         label_encoding = TRUE) {
  #
  # # data cleaning
  # # factors
  # datenModell <- SR_feat_eng_factors(datenModell, make_na_explicit = TRUE,
  #                                    combine_rare_levels = TRUE, prop = 0.01)
  # datenModelltest <- SR_feat_eng_factors(datenModelltest, make_na_explicit = TRUE,
  #                                        combine_rare_levels = TRUE, prop = 0.01)
  # if (label_encoding) {
  #   datenModell <- SR_feat_eng_factors(datenModell, label_encoding = TRUE)
  #   datenModelltest <- SR_feat_eng_factors(datenModelltest, label_encoding = TRUE,
  #                                          use_other_df = data_train)
  # }
  # # replace NA for numeric and date
  # datenModell <- SR_replace_NA_median(datenModell)
  # datenModelltest <- SR_replace_NA_median(datenModelltest, use_other_df = data_train)
  # # numeric
  # datenModell <- SR_feat_eng_numeric(datenModell, trim_outliers = TRUE, exception = yname)
  # datenModelltest <- SR_feat_eng_numeric(datenModelltest, trim_outliers = TRUE, exception = yname)
  # # datenModell <- SR_feat_eng_numeric(datenModell, replace_NA_special_value = TRUE)
  # # datenModelltest <- SR_feat_eng_numeric(datenModelltest, replace_NA_special_value = TRUE)
  # # date
  # datenModell <- SR_feat_eng_date(datenModell, only_date_to_numeric = TRUE)
  # datenModelltest <- SR_feat_eng_date(datenModelltest, only_date_to_numeric = TRUE)
  # # remove columns with unique value
  # datenModell <- SR_remove_column_with_unique_value(datenModell)
  # datenModelltest <- datenModell[, names(datenModell)]
  #
  # feature cleaning
  # df <- SR_feat_eng_factors(df, make_na_explicit = TRUE)
  # if (label_encoding) {
  #   df <- SR_feat_eng_factors(df, label_encoding = TRUE)
  # }
  # df <- SR_feat_eng_numeric(df, replace_NA_special_value = TRUE)
  # df <- SR_feat_eng_date(df, only_date_to_numeric = TRUE)
  # df <- df[, names(datenModell)]
  #
  # turn train and test into matrices for dummy variables
  options(na.action = 'na.pass')  # global option !!!
  #
  forecast_mat <- Matrix::sparse.model.matrix(~. -1, data = df)
  d_forecast <- xgboost::xgb.DMatrix(data = forecast_mat)
  #
  # return results to environment
  assign('forecast_mat', forecast_mat, envir = .GlobalEnv)
  assign('d_forecast', d_forecast, envir = .GlobalEnv)
}
