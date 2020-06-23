#' SRxgboost_sample_training_data
#'
#' Function to sample training data with different methods.
#'
#' df_new <- SR_sample_training_data(df = df, folds = folds, sample_method = "ubOver")
#' df_new <- SR_sample_training_data(df = df, folds = folds, sample_method = "ubOver")
#' df_new <- SR_sample_training_data(df = df, folds = folds, sample_method = "ubUnder")
#' df_new <- SR_sample_training_data(df = df, folds = folds, sample_method = "ubSMOTE")
#' df_new <- SR_sample_training_data(df = df, folds = folds, sample_method = "ubOSS")
#' df_new <- SR_sample_training_data(df = df, folds = folds, sample_method = "ubCNN")
#' df_new <- SR_sample_training_data(df = df, folds = folds, sample_method = "ubENN")   # error?
#' df_new <- SR_sample_training_data(df = df, folds = folds, sample_method = "ubNCL")
#' df_new <- SR_sample_training_data(df = df, folds = folds, sample_method = "ubTomek")
#' xx_ubUnder_perc50 <- SR_sample_training_data(df = df, folds = folds,
#'                                             sample_method = "ubUnder", perc = 50)
#' xx_ubUnder_perc40 <- SR_sample_training_data(df = df, folds = folds,
#'                                              sample_method = "ubUnder", perc = 40)
#' xx_ubSMOTE_po200_pu200_k5 <- SR_sample_training_data(df = df, folds = folds,
#'                                                      sample_method = "ubSMOTE",
#'                                                      percOver = 200, percUnder = 200, k = 5)
#'
#' https://cran.r-project.org/web/packages/unbalanced/unbalanced.pdf
#'
#' @param df data.frame
#' @param folds list
#' @param sample_method character: ubOver, ubUnder, ubSMOTE, ubOSS, ubCNN, ubENN, ubNCL, ubTomek
#' @param percOver integer: parameter used in ubSMOTE
#' @param percUnder integer: parameter used in ubSMOTE
#' @param k integer: parameter used in ubOver, ubSMOTE, ubCNN, ubENN, ubNCL
#' @param perc integer: parameter used in ubUnder
#' @param method character; parameter used in ubUnder
#' @param w ???: parameter used in ubUnder
#' @param verbose boolean
#'
#' @return return sampled data and new folds
#'
#' @export
SRxgboost_sample_training_data <- function(df,
                                           folds,
                                           sample_method,        # ubOver, ubUnder, ubSMOTE, ubOSS, ubCNN, ubENN, ubNCL, ubTomek
                                           percOver = 200,       # parameter used in ubSMOTE
                                           percUnder = 200,      # parameter used in ubSMOTE
                                           k = 5,                # parameter used in ubOver, ubSMOTE, ubCNN, ubENN, ubNCL
                                           perc = 50,            # parameter used in ubUnder
                                           method = "percPos",   # parameter used in ubUnder
                                           w = NULL,             # parameter used in ubUnder
                                           verbose = TRUE){
  ### create new training data with different sampling methods for each fold
  #
  # create new data.frame for sampled data
  df_sampled <- data.frame()
  #
  for (i in 1:length(folds)) {
    # get rows for this fold
    df_temp <- df[folds[[i]], ] %>%
      dplyr::mutate(y = as.factor(y))
    # df_temp %>% dplyr::count(y) %>% dplyr::mutate(n_percent = n/sum(n))
    #
    # sample data
    # only numeric features are allowed: ubENN, ubNCL, ubOSS, ubCNN, ubTomek    TODO !!!
    df_temp <- unbalanced::ubBalance(X = df_temp %>% dplyr::select(-y),
                                     Y = df_temp$y,
                                     positive = 1,
                                     type = sample_method,
                                     percOver = percOver,
                                     percUnder = percUnder,
                                     k = dplyr::if_else(sample_method %in% c("ubCNN"), 1, k),   # strange error
                                     perc = perc,
                                     method = method,
                                     w = w,
                                     verbose = verbose)
    #
    # sampled dataset
    df_temp <- data.frame(y = df_temp$Y, df_temp$X) %>%
      dplyr::mutate(y = as.numeric(as.character(y)),
                    fold = i)
    # df_temp %>% dplyr::count(y) %>% dplyr::mutate(n_percent = n/sum(n))
    #
    # rewrite folds[[i]]
    folds[[i]] <- (nrow(df_sampled) + 1):(nrow(df_sampled) + nrow(df_temp))
    #
    # create new data.frame
    df_sampled <- dplyr::bind_rows(df_sampled, df_temp)
    #
    # clean up
    rm(df_temp)
  }; rm(i)
  #
  # show results
  if (verbose) {
    df_sampled %>%
      dplyr::count(y) %>%
      dplyr::mutate(n_percent = n/sum(n)) %>%
      data.frame() %>%
      print()
  }
  #
  # randomize rows again (because it is sorted by fold now)
  set.seed(12345)
  rows_randomised <- sample(1:nrow(df_sampled), nrow(df_sampled), replace = FALSE)
  set.seed(Sys.time())
  #
  df_sampled_ <- df_sampled %>%
    mutate(row = 1:nrow(.))
  df_sampled_ <- df_sampled_[rows_randomised, ] %>%
    mutate(row_new = 1:nrow(.))
  #
  folds_ <- vector("list", length(folds))
  for (i in 1:length(folds)) {
    folds_[[i]] <- df_sampled_$row_new[df_sampled_$fold == i]
  }; rm(i)
  #
  rm(rows_randomised)
  #
  # return sampled data and new folds
  return(list(df_sampled_, folds_))
}
