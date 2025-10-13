#' SRxgboost_check_uncertainty
#'
#' Plots model results for a selected model.
#'
#' @param lauf character
#' @param quantiles vector, default: seq(0.8, 1, by = 0.005)
#'
#' @return uncertainty stats, list with ids and plot
#'
#' @export
SRxgboost_check_uncertainty <- function(lauf, quantiles = seq(0.8, 1, by = 0.005)) {
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
  SRxgboost_get_OOFforecast_TESTforecast(lauf = lauf, top_rank = 10, ensemble = FALSE)
  objective <- readRDS(paste0(path_output_data, "objective.rds"))
  if (length(y_OOF$y) == nrow(OOFforecast)) {
    train_pr_oof <- as.data.frame(cbind(y_OOF$y, pr = OOFforecast[, 2]))
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
  mat <- as.matrix(OOFforecast[, 2:ncol(OOFforecast)])
  OOFforecast <- OOFforecast %>%
    dplyr::mutate(MEAN = rowMeans(mat, na.rm = TRUE),
                  SD = matrixStats::rowSds(mat, na.rm = TRUE),
                  SD_PERC = SD / MEAN,
                  y = y_OOF$y)
  #
  mat <- as.matrix(TESTforecast[, 2:ncol(TESTforecast)])
  TESTforecast <- TESTforecast %>%
    dplyr::mutate(MEAN = rowMeans(mat, na.rm = TRUE),
                  SD = matrixStats::rowSds(mat, na.rm = TRUE),
                  SD_PERC = SD / MEAN) %>%
    { if (exists("y_test")) dplyr::mutate(., y = y_test$y) else . }
  #
  quantile_values <- round(quantile(OOFforecast$SD_PERC, probs = quantiles), 6)
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
  for (i in seq_along(quantile_values)) {
    subset_data <- OOFforecast %>% dplyr::filter(SD_PERC <= quantile_values[i])
    uncertainty_stats <- dplyr::bind_rows(
      uncertainty_stats,
      data.frame(UNCERTAINTY = quantile_values[i],
                 DATA_COUNT = nrow(subset_data),
                 PERCENT_DATA_REMOVED = round(1 - nrow(subset_data) / nrow(OOFforecast), 5),
                 STAT = calculate_stats(subset_data)))
    rm(subset_data)
  }; rm(i)
  uncertainty_stats <- uncertainty_stats %>%
    tibble::rownames_to_column(var = "QUANTIL") %>%
    dplyr::mutate(QUANTIL = as.numeric(sub("%", "", QUANTIL)) / 100,
                  STAT_IMPROVEMENT = STAT - dplyr::lead(STAT, 1L),
                  IMPROVEMENT_RATE = round(STAT_IMPROVEMENT / PERCENT_DATA_REMOVED, 5),
                  TOP5 = IMPROVEMENT_RATE >= sort(IMPROVEMENT_RATE, decreasing = TRUE)[5],
                  TOP5 = ifelse(IMPROVEMENT_RATE > 0, TOP5, FALSE),
                  TOP5 = tidyr::replace_na(TOP5, FALSE),
                  label = ifelse(TOP5, paste0(QUANTIL * 100, "%"), NA))
  saveRDS(uncertainty_stats, paste0(path_output_best, "Uncertainty_stats.rds"))
  assign('uncertainty_stats', uncertainty_stats, envir = .GlobalEnv)
  #
  #
  uncertainty <- dplyr::bind_rows(OOFforecast %>%
                                    dplyr::mutate(DATA = "train") %>%
                                    dplyr::select(DATA, id, UNCERTAINTY = SD_PERC),
                                  TESTforecast %>%
                                    dplyr::mutate(DATA = "test") %>%
                                    dplyr::select(DATA, id, UNCERTAINTY = SD_PERC))
  assign('uncertainty', uncertainty, envir = .GlobalEnv)
  #
  #
  # Plot ####
  #
  p <- ggplot2::ggplot(uncertainty_stats, ggplot2::aes(x = QUANTIL, y = STAT)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(color = TOP5), na.rm = TRUE) +
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

