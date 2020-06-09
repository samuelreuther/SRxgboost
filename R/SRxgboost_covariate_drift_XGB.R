#' SRxgboost_covariate_drift_XGB
#'
#' Check for covariate drift with XGBOOST Classification-Model (y = 0/1).
#'
#' @param df1 data.frame
#' @param df2 data.frame
#'
#' @return several files in folder
#'
#' @export
SRxgboost_covariate_drift_XGB <- function(df1 = NULL,
                                          df2 = NULL) {
  # create folder
  dir.create(paste0(path_output, gsub(".csv", "/", lauf)), showWarnings = FALSE, recursive = TRUE)
  #
  ### prepare data
  suppressWarnings(
    train_all <- dplyr::bind_rows(df1 %>%
                                    dplyr::select(dplyr::one_of(dplyr::intersect(names(df1),
                                                                                 names(df2)))) %>%
                                    dplyr::mutate(y = 0),
                                  df2 %>%
                                    dplyr::select(dplyr::one_of(dplyr::intersect(names(df1),
                                                                                 names(df2)))) %>%
                                    dplyr::mutate(y = 1)))
  # downsample data
  if (nrow(train_all) > 10000) {
    set.seed(12345)
    train <- train_all %>%
      dplyr::group_by(y) %>%
      dplyr::sample_n(min(nrow(df1), nrow(df2), 5000)) %>%
      data.frame()
    set.seed(Sys.time())
  } else {
    train <- train_all
  }
  rm(train_all)
  assign('train', train, envir = .GlobalEnv)
  id_unique_train <- 1:nrow(train)
  assign('id_unique_train', id_unique_train, envir = .GlobalEnv)
  #
  # count y
  train %>% dplyr::count(y) %>% print()
  #
  # SRxgboost_data_prep
  no_folds <- 5
  assign('no_folds', no_folds, envir = .GlobalEnv)
  SRxgboost_data_prep("y", data_train = train, data_test = NULL, no_folds = no_folds,
                      eval_index = NULL, objective = "binary", label_encoding = TRUE,
                      check_covariate_drift = FALSE)
  #
  # SRxgboost_run
  cat(paste0(Sys.time(), " XGBOOST Modelle für: Covariate Drift\n"))
  SRxgboost_run(nround = 10000, eta = 0.1, dart = 0, continue_threshold = 0.1, runs = 2,
                obj = "binary:logistic", metric = "auc", max_overfit = -1,
                nfold = no_folds, trees = 1, verbose = 0, test_param = FALSE,
                run_final_model = TRUE, best_params = NULL)
  #
  # SRxgboost_plots results of best model
  SRxgboost_plots(lauf = lauf, rank = 1, min_rel_Gain = 0.02)
  #
  # clean up
  rm(no_folds)
}
