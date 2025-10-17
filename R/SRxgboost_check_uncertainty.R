#' SRxgboost_check_uncertainty
#'
#' Check model quality (R2, AUC) for different cutoffs of uncertainty.
#' But only middle range-forecasts are checked.
#'
#' @param lauf character
#' @param quantiles vector, default: seq(0.8, 1, by = 0.005)
#' @param range vector, default: c(0.25, 0.75)
#'
#' @return uncertainty stats, list with ids and plot
#'
#' @export
SRxgboost_check_uncertainty <- function(lauf, quantiles = seq(0.5, 1, by = 0.005),
                                        range = c(0.25, 0.75)) {
  # Set paths ####
  #
  # check lauf ends with ".csv"
  if (!grepl('.csv$', lauf)) lauf <- paste0(lauf, ".csv")
  path_output_best <- paste0(path_output, gsub(".csv", "/", lauf), "Best Model/")
  path_output_data <- paste0(path_output, gsub(".csv", "/", lauf), "Data/")
  if (!dir.exists(path_output_best)) dir.create(path_output_best, showWarnings = FALSE)
  #
  #
  # SummaryCV ####
  #
  SRxgboost_get_summary_CV(lauf)
  assign('SummaryCV', SummaryCV, envir = .GlobalEnv)
  #
  #
  # OOFforecast and TESTforecast ####
  #
  SRxgboost_get_OOFforecast_TESTforecast(lauf = lauf, top_rank = 11, ensemble = FALSE)
  objective <- readRDS(paste0(path_output_data, "objective.rds"))
  #
  if (length(y_OOF$y) == nrow(OOFforecast)) {
    train_pr_oof <- as.data.frame(cbind(y_OOF$y, pr = OOFforecast[, 2])) # col 2 is best model
  } else {
    train_pr_oof <- as.data.frame(cbind(y = y[eval_index], pr = OOFforecast[, 2]))
  }
  # for "multi:softprob": add probabilities
  if (objective == "multilabel" & ncol(OOFforecast) > 2) {
    train_pr_oof <- dplyr::bind_cols(train_pr_oof,
                                     OOFforecast[, 2:(ncol(OOFforecast) - 1)] %>%
                                       stats::setNames(paste0("X", 0:(ncol(.) - 1))))
  }
  if (file.exists(paste0(path_output_data, "y_test.rds"))) {
    y_test <- readRDS(paste0(path_output_data, "y_test.rds"))
    test_pr <- as.data.frame(cbind(y_test, pr_test = TESTforecast[, 2]))
  }
  #
  #
  # Calculate uncertainty ####
  #
  mat <- as.matrix(OOFforecast[, 3:ncol(OOFforecast)])
  OOFforecast <- OOFforecast %>%
    dplyr::mutate(MEAN = rowMeans(mat, na.rm = TRUE),
                  SD = matrixStats::rowSds(mat, na.rm = TRUE),
                  y = y_OOF$y)
  #
  mat <- as.matrix(TESTforecast[, 3:ncol(TESTforecast)])
  TESTforecast <- TESTforecast %>%
    dplyr::mutate(MEAN = rowMeans(mat, na.rm = TRUE),
                  SD = matrixStats::rowSds(mat, na.rm = TRUE)) %>%
    { if (exists("y_test")) dplyr::mutate(., y = y_test$y) else . }
  #
  quantiles_sd <- round(quantile(OOFforecast$SD, probs = quantiles), 6)
  quantiles_mean <- round(quantile(OOFforecast$MEAN, probs = range), 6)
  #
  calculate_stats <- function(df) {
    if (objective == "regression") {
      as.numeric(round(stats::cor(df$y, df[, 2])^2, 5))
    } else if (objective == "classification") {
      ROC <- try(pROC::roc(response = df$y, predictor = df[, 2],
                           algorithm = 2, levels = c(0, 1),
                           direction = "<"), silent = TRUE)
      if (inherits(ROC, "try-error")) NA_real_ else round(as.numeric(ROC$auc), 5)
    }
  }
  #
  uncertainty_stats <- data.frame()
  for (i in seq_along(quantiles_sd)) {
    subset_data <- OOFforecast %>%
      dplyr::filter((MEAN <= quantiles_mean[1] | MEAN >= quantiles_mean[2]) |
                      ((MEAN > quantiles_mean[1] & MEAN < quantiles_mean[2]) &
                         SD <= quantiles_sd[i]))
    # subset_data <- OOFforecast %>% dplyr::filter(SD <= quantiles_sd[i])
    # set.seed(123456)
    uncertainty_stats <- dplyr::bind_rows(
      uncertainty_stats,
      data.frame(UNCERTAINTY = quantiles_sd[i],
                 DATA_COUNT = nrow(subset_data),
                 PERCENT_DATA_REMOVED = round(1 - nrow(subset_data) / nrow(OOFforecast), 5),
                 # STAT_RND = calculate_stats(OOFforecast %>%
                 #                              dplyr::sample_n(nrow(subset_data))),
                 STAT = calculate_stats(subset_data)))
    rm(subset_data)
  }; rm(i) #; set.seed(Sys.time())
  uncertainty_stats <- uncertainty_stats %>%
    tibble::rownames_to_column(var = "QUANTIL") %>%
    dplyr::mutate(QUANTIL = as.numeric(sub("%", "", QUANTIL)) / 100,
                  STAT_IMPROVEMENT = STAT - dplyr::lead(STAT, 1L),
                  COVERAGE_NORM = 1 - (PERCENT_DATA_REMOVED - min(PERCENT_DATA_REMOVED)) /
                    (max(PERCENT_DATA_REMOVED) - min(PERCENT_DATA_REMOVED)),
                  STAT_NORM = (STAT - min(STAT)) / (max(STAT) - min(STAT)),
                  MEAN_25 = quantiles_mean[1],
                  MEAN_75 = quantiles_mean[2])
  #
  uncertainty_stats <- uncertainty_stats %>%
    mutate(SCORE = STAT_NORM^(1 - 0.10) * COVERAGE_NORM^0.10,   # coverage_weights 10%
           TOP5 = SCORE >= sort(SCORE, decreasing = TRUE)[1]) %>%
    mutate(SCORE = STAT_NORM^(1 - 0.25) * COVERAGE_NORM^0.25,   # coverage_weights 25%
           TOP5 = ifelse(TOP5, TOP5, SCORE >= sort(SCORE, decreasing = TRUE)[1])) %>%
    mutate(SCORE = STAT_NORM^(1 - 0.50) * COVERAGE_NORM^0.50,   # coverage_weights 50%
           TOP5 = ifelse(TOP5, TOP5, SCORE >= sort(SCORE, decreasing = TRUE)[1])) %>%
    mutate(SCORE = STAT_NORM^(1 - 0.75) * COVERAGE_NORM^0.75,   # coverage_weights 75%
           TOP5 = ifelse(TOP5, TOP5, SCORE >= sort(SCORE, decreasing = TRUE)[1])) %>%
    mutate(SCORE = STAT_NORM^(1 - 0.90) * COVERAGE_NORM^0.90,   # coverage_weights 90%
           TOP5 = ifelse(TOP5, TOP5, SCORE >= sort(SCORE, decreasing = TRUE)[1])) %>%
    mutate(label = ifelse(TOP5, paste0(QUANTIL * 100, "%\n",
                                       round(PERCENT_DATA_REMOVED,3 ) * 100, "%"), NA))
  #
  saveRDS(uncertainty_stats, paste0(path_output_best, "Uncertainty_stats.rds"))
  assign('uncertainty_stats', uncertainty_stats, envir = .GlobalEnv)
  #
  #
  uncertainty <- dplyr::bind_rows(OOFforecast %>%
                                    dplyr::mutate(DATA = "train") %>%
                                    dplyr::select(DATA, id, MEAN = MEAN,
                                                  UNCERTAINTY = SD),
                                  TESTforecast %>%
                                    dplyr::mutate(DATA = "test") %>%
                                    dplyr::select(DATA, id, MEAN = MEAN,
                                                  UNCERTAINTY = SD))
  assign('uncertainty', uncertainty, envir = .GlobalEnv)
  #
  #
  # Plot ####
  #
  p <- ggplot2::ggplot(uncertainty_stats, ggplot2::aes(x = QUANTIL, y = STAT)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(color = TOP5), na.rm = TRUE) +
    # ggplot2::geom_line(aes(y = STAT_RND), na.rm = TRUE, colour = "grey40") +
    # ggplot2::geom_point(aes(y = STAT_RND), na.rm = TRUE, colour = "grey40") +
    ggrepel::geom_text_repel(ggplot2::aes(label = label), nudge_y = 0.002, size = 3, na.rm = TRUE) +
    ggplot2::scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6),
                                labels = scales::percent) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
    ggplot2::labs(title = "Modell performance at different uncertainty-thresholds",
                  x = "Uncertainty-threshold (Quantile)",
                  y = ifelse(objective == "regression", "R2", "AUC")) +
    ggplot2::theme(legend.position = "none"); print(p)
  ggplot2::ggsave(paste0(path_output_best, "Uncertainty.png"),
                  plot = p, width = 9.92, height = 5.3)
}

