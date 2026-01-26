#' SRxgboost_compare_sample_methods
#'
#' Function to compare sample methods with XGBOOST-models.
#'
#' @param df_train data.frame
#' @param df_test data.frame
#' @param folds list, output from SRxgboost_create_folds
#' @param runs integer, number of xgboost runs
#' @param sample_methods list, c("ubOver", "ubUnder", "ubSMOTE", "ubOSS", "ubCNN",
#'                                "ubENN", "ubNCL", "ubTomek")
#' @param pdp_parallel boolean, default = TRUE
#'
#' @return lots of files and a graphic of method comparison
#'
#' @export
SRxgboost_compare_sample_methods <- function(df_train,
                                             # y_name,                  TODO !!!
                                             df_test = NULL,
                                             folds,
                                             runs = 2,
                                             sample_methods,
                                             pdp_parallel = TRUE) {
  ### checks
  # check path_output exists
  if (!exists("path_output")) cat("'path_output' is missing \n")
  # check if y_name is 'y'
  if (!"y" %in% names(df_train)) stop("y_name must be 'y'")
  #
  # backup path_output
  path_output_backup <- path_output
  #
  # loop through all selected sample_methods
  for (m in c("unbalanced", sample_methods)) {   # m <- "ubOver", m <- "unbalanced"
    # check if run exists already and print progress
    # lauf <- paste0(m, ".csv")
    lauf <- m
    assign("lauf", lauf, envir = .GlobalEnv)
    path_output <- paste0(path_output_backup, "compare_sample_methods/")
    assign("path_output", path_output, envir = .GlobalEnv)
    if (dir.exists(paste0(path_output, lauf, "/Best Model"))) {
      suppressWarnings(rm(lauf, path_output))
      next()
    } else {
      dir.create(path_output, showWarnings = FALSE, recursive = TRUE)
    }
    cat(paste0("\n", "Run:   ", m, "\n"))
    try({
      # sample training data
      if (m == "unbalanced") {
        return <- list(df_train, folds)
      } else {
        return <- SRxgboost_sample_training_data(folds = folds, df = df_train,
                                                 sample_method = m, verbose = FALSE)
      }
      df_sampled <- return[[1]]
      folds_sampled <- return[[2]]
      #
      # run XGB_data_prep
      id_unique_train <- 1:nrow(df_sampled)
      assign("id_unique_train", id_unique_train, envir = .GlobalEnv)
      SRxgboost_data_prep(yname = "y", data_train = df_sampled, data_test = df_test,
                          folds = folds_sampled, objective = "classification",
                          check_covariate_drift = FALSE)
      #
      # run XGB_RUN with CV on folds
      SRxgboost_run(nround = 10000, eta = 0.1, obj = "binary:logistic", metric = "auc",
                    runs = runs, folds = folds_sampled, run_final_model = TRUE, max_overfit = -1)
      #
      # generate plots for best model
      SRxgboost_plots(lauf = lauf, rank = 1, pdp_min_rel_Gain = 0.05,
                      pdp_parallel = pdp_parallel)
      #
      # clean up
      saveRDS(return, paste0(path_output, m, "/Data/return.rds"))
      suppressWarnings(rm(return, df_sampled, folds_sampled, id_unique_train,
                          OOFforecast, TESTforecast, SummaryCV_META, test_pr,
                          y_OOF, factor_encoding))
      SRxgboost_cleanup()
    }, TRUE)
  }; suppressWarnings(rm(m, lauf))
  #
  ### plot error comparison
  #
  comparison <- data.frame()
  for (i in c("unbalanced", sample_methods)) {
    if (!file.exists(paste0(path_output, i, "/Data/return.rds"))) next()
    return <- readRDS(paste0(path_output, i, "/Data/return.rds"))
    df_temp <- utils::read.csv2(paste0(path_output, i, "/Summary.csv")) %>%
      dplyr::arrange(-test) %>%
      utils::head(1) %>%
      dplyr::mutate(Lauf = i) %>%
      mutate(train_nrow = nrow(return[[1]]),
             train_y_1 = sum(return[[1]]$y == 1),
             train_y_1_percent = train_y_1 / train_nrow)
    comparison <- dplyr::bind_rows(comparison, df_temp)
    rm(return, df_temp)
  }; suppressWarnings(rm(i))
  #
  # files <- list.files(path_output, pattern = "Summary.csv", recursive = TRUE)
  # comparison <- data.frame()
  # for (i in files) {
  #   comparison <- dplyr::bind_rows(comparison,
  #                                  utils::read.csv2(paste0(path_output, i)) %>%
  #                                    dplyr::arrange(-test) %>%
  #                                    utils::head(1) %>%
  #                                    dplyr::mutate(Lauf = i %>%
  #                                                    gsub("/Summary.csv", "", .)))
  # }; suppressWarnings(rm(i, files))
  #
  if (!file.exists(paste0(path_output, "unbalanced/Data/y_test.rds"))) {
    # plot model error
    p <- comparison %>%
      dplyr::select(Lauf, train, CV = test) %>%
      dplyr::mutate(Lauf = factor(Lauf),
                    Lauf = stats::reorder(Lauf, -CV)) %>%
      reshape2::melt(id = "Lauf") %>%
      ggplot2::ggplot(ggplot2::aes(x = Lauf, y = value, colour = variable,
                                   group = variable, label = round(value, 3))) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2) +
      ggrepel::geom_text_repel(size = 3, direction = "y") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5)) +
      ggplot2::labs(y = "AUC", colour = "",
                    title = "Vergleich der Modellgüte der einzelnen Sample Methoden") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
      ggplot2::theme(legend.position = "top")
    print(p)
    ggplot2::ggsave(paste0(path_output, "comparison.png"), p,
                    width = 9.92, height = 5.3)
    rm(p)
    #
    # plot training data
    p <- comparison %>%
      dplyr::mutate(Lauf = factor(Lauf),
                    Lauf = stats::reorder(Lauf, -test)) %>%
      ggplot2::ggplot(ggplot2::aes(x = Lauf, y = train_nrow, size = train_y_1_percent,
                                   label = paste0(round(train_y_1_percent, 3) * 100, " %"))) +
      ggplot2::geom_point() +
      ggrepel::geom_text_repel(size = 3, direction = "y") +
      # ggplot2::geom_text(size = 3) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5)) +
      ggplot2::labs(y = "Anzahl Trainingsdaten", size = "Anteil y = 1",
                    title = "Vergleich der Trainingsdaten der einzelnen Sample Methoden") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6),
                                  labels = scales::format_format(big.mark = "'"),
                                  limits = c(0, NA)) +
      ggplot2::scale_size_continuous(labels = scales::percent, limits = c(0, 0.5))
    print(p)
    ggplot2::ggsave(paste0(path_output, "comparison_training_data.png"), p,
                    width = 9.92, height = 5.3)
    rm(p)
  } else {
    # plot train, test and CV results                      WARNING !!! works only if runs = 2   TODO !!!
    # read y_test
    TESTforecast <- data.frame(y_test = readRDS(paste0(path_output, "unbalanced/Data/y_test.rds")))
    # read TESTforecast i loop
    files <- list.files(path_output, pattern = "Summary.csv", recursive = TRUE)
    auc_test <- data.frame(Lauf = gsub("/Summary.csv", "", files))
    for (i in files) {
      # calculate AUC
      TESTforecast <- dplyr::bind_cols(TESTforecast,
                                       readRDS(paste0(path_output, gsub("Summary.csv", "", i),
                                                      "Data/TESTforecast.rds")) %>%
                                         dplyr::select(2) %>%
                                         stats::setNames(gsub("/Summary.csv", "", i, fixed = TRUE)))
      # calculate AUC
      auc_test$auc[grep(i, files)] <- round(as.numeric(pROC::auc(TESTforecast$y_test,
                                                                 TESTforecast[, ncol(TESTforecast)],
                                                                 levels = c(0, 1), direction = "<")), 3)
    }; suppressWarnings(rm(i, files))
    #
    # add test error to comparison
    comparison <- dplyr::left_join(comparison,
                                   auc_test %>% dplyr::rename(test_data = auc),
                                   by = "Lauf")
    #
    # plot model error
    p <- comparison %>%
      dplyr::select(Lauf, train, CV = test, test = test_data) %>%
      dplyr::mutate(Lauf = factor(Lauf),
                    Lauf = stats::reorder(Lauf, -test)) %>%
      reshape2::melt(id = "Lauf") %>%
      ggplot2::ggplot(ggplot2::aes(x = Lauf, y = value, colour = variable,
                                   group = variable, label = round(value, 3))) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2) +
      ggrepel::geom_text_repel(size = 3, direction = "y") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5)) +
      ggplot2::labs(y = "AUC", colour = "",
                    title = "Vergleich der Modellgüte der einzelnen Sample Methoden") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
      ggplot2::theme(legend.position = "top")
    print(p)
    ggplot2::ggsave(paste0(path_output, "comparison.png"), p,
                    width = 9.92, height = 5.3)
    rm(p)
    #
    # plot training data
    p <- comparison %>%
      dplyr::mutate(Lauf = factor(Lauf),
                    Lauf = stats::reorder(Lauf, -test_data)) %>%
      ggplot2::ggplot(ggplot2::aes(x = Lauf, y = train_nrow, size = train_y_1_percent,
                                   label = paste0(round(train_y_1_percent, 3) * 100, " %"))) +
      ggplot2::geom_point() +
      ggrepel::geom_text_repel(size = 3, direction = "y") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5)) +
      ggplot2::labs(y = "Anzahl Trainingsdaten", size = "Anteil y = 1",
                    title = "Vergleich der Trainingsdaten der einzelnen Sample Methoden") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6),
                                  labels = scales::format_format(big.mark = "'"),
                                  limits = c(0, NA)) +
      ggplot2::scale_size_continuous(labels = scales::percent, limits = c(0, 0.5))
    print(p)
    ggplot2::ggsave(paste0(path_output, "comparison_training_data.png"), p,
                    width = 9.92, height = 5.3)
    rm(p)
  }
  #
  # save comparison
  saveRDS(comparison, paste0(path_output, "comparison.rds"))
  #
  #
  # reset path_output
  path_output <- path_output_backup
  assign("path_output", path_output, envir = .GlobalEnv)
  suppressWarnings(rm(path_output_backup))
  #
  # return comparison
  return(comparison)
}
