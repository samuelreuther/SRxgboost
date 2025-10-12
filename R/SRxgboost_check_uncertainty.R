#' SRxgboost_check_uncertainty
#'
#' Plots model results for a selected model.
#'
#' @param lauf character
#'
#' @return uncertainty stats and plot
#'
#' @export
SRxgboost_check_uncertainty <- function(lauf) {
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
  assign('train_pr_oof', train_pr_oof, envir = .GlobalEnv)
  if (file.exists(paste0(path_output_data, "y_test.rds"))) {
    y_test <- readRDS(paste0(path_output_data, "y_test.rds"))
    test_pr <- as.data.frame(cbind(y_test, pr_test = TESTforecast[, 2]))
    assign('test_pr', test_pr, envir = .GlobalEnv)
  }
  #
  #
  # Data prep ####
  #
  mat <- as.matrix(OOFforecast[, 2:ncol(OOFforecast)])
  OOFforecast <- OOFforecast %>%
    dplyr::mutate(mean = rowMeans(mat, na.rm = TRUE),
                  sd = matrixStats::rowSds(mat, na.rm = TRUE),
                  sd_perc = sd / mean,
                  y = y_OOF$y)
  # OOFforecast <- OOFforecast %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(mean = mean(dplyr::c_across(2:ncol(.)), na.rm = TRUE),
  #          sd = sd(dplyr::c_across(2:ncol(.)), na.rm = TRUE)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(sd_perc = sd / mean) %>%
  #   dplyr::mutate(y = y_OOF$y)
  mat <- as.matrix(TESTforecast[, 2:ncol(TESTforecast)])
  TESTforecast <- TESTforecast %>%
    dplyr::mutate(mean = rowMeans(mat, na.rm = TRUE),
                  sd = matrixStats::rowSds(mat, na.rm = TRUE),
                  sd_perc = sd / mean) %>%
    { if (exists("y_test")) dplyr::mutate(., y = y_test$y) else . }
  # TESTforecast <- TESTforecast %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(mean = mean(dplyr::c_across(2:ncol(.)), na.rm = TRUE),
  #          sd = sd(dplyr::c_across(2:ncol(.)), na.rm = TRUE)) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(sd_perc = sd / mean) %>%
  #   { if (exists("y_test")) dplyr::mutate(., y = y_test$y) else . }
  #
  quantiles <- round(quantile(OOFforecast$sd_perc, probs = seq(0, 1, by = 0.01)), 5)
  #
  calculate_stats <- function(df) {
    if (objective == "regression") {
      as.numeric(round(stats::cor(df$y, df[, 2])^2, 4))
    } else if (objective == "classification") {
      ROC <- pROC::roc(response = df$y, predictor = df[, 2], algorithm = 2,
                       levels = c(0, 1), direction = "<")
      round(as.numeric(ROC$auc), 4)
    }
  }
  #
  uncertainty <- data.frame()
  for (i in seq_along(quantiles)) {
    subset_data <- OOFforecast %>% dplyr::filter(sd_perc <= quantiles[i])
    uncertainty <- dplyr::bind_rows(
      uncertainty,
      data.frame(UNCERTAINTY = quantiles[i],
                 DATA_COUNT = nrow(subset_data),
                 PERCENT_DATA_REMOVED = round(1 - nrow(subset_data) / nrow(OOFforecast), 4),
                 STAT = calculate_stats(subset_data)))
    rm(subset_data)
  }; rm(i)
  uncertainty <- uncertainty %>%
    tibble::rownames_to_column(var = "QUANTIL") %>%
    dplyr::mutate(QUANTIL = as.numeric(sub("%", "", QUANTIL)) / 100,
                  STAT_IMPROVEMENT = STAT - dplyr::lead(STAT, 1L),
                  IMPROVEMENT_RATE = round(STAT_IMPROVEMENT / PERCENT_DATA_REMOVED, 4),
                  TOP5 = IMPROVEMENT_RATE >= sort(IMPROVEMENT_RATE, decreasing = TRUE)[5],
                  TOP5 = replace_na(TOP5, FALSE),
                  label = ifelse(TOP5, paste0(QUANTIL * 100, "%"), NA))
  saveRDS(uncertainty, paste0(path_output_best, "Uncertainty.rds"))
  #
  #
  # Plot ####
  #
  p <- ggplot2::ggplot(uncertainty, ggplot2::aes(x = QUANTIL, y = STAT)) +
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
    theme(legend.position = "none"); print(p)
  ggplot2::ggsave(paste0(path_output_best, "Uncertainty.png"),
                  plot = p, width = 9.92, height = 5.3)
  #
  suppressWarnings(rm(SummaryCV, OOFforecast, TESTforecast, SummaryCV_META, y_OOF, y_test))
  return(uncertainty)
}

