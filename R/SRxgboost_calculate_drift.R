#' SRxgboost_calculate_drift
#'
#' Calculates drift for example between train and test-data or on residuals.
#'
#' @param data_old data.frame
#' @param data_new data.frame
#' @param n_bins integer
#' @param n_plots integer
#' @param name character
#'
#' @return several files in folder
#'
#' @export
SRxgboost_calculate_drift <- function(data_old,
                                      data_new,
                                      n_bins = 10,
                                      n_plots = 10,
                                      name = "") {
  # downsample
  if (nrow(data_old) > 10000) {
    set.seed(12345)
    data_old <- data_old %>% dplyr::sample_n(10000)
    set.seed(Sys.time())
  }
  if (nrow(data_new) > 10000) {
    set.seed(12345)
    data_new <- data_new %>% dplyr::sample_n(10000)
    set.seed(Sys.time())
  }
  #
  # variables present in both datasets
  joint_var <- intersect(colnames(data_new), colnames(data_old))
  data_old <- data_old %>% dplyr::select(dplyr::one_of(joint_var))
  data_new <- data_new %>% dplyr::select(dplyr::one_of(joint_var))
  #
  # data clean up
  data_old <- SRfunctions::SR_feat_eng_factors(data_old,
                                               make_na_explicit = TRUE,
                                               combine_rare_levels = TRUE, prop = 0.005)
  data_new <- SRfunctions::SR_feat_eng_factors(data_new,
                                               make_na_explicit = TRUE,
                                               combine_rare_levels = TRUE,
                                               use_other_df = data_old)
  #
  # distances between variables
  distances <- numeric(length(joint_var))
  names(distances) <- joint_var
  #
  for (i in seq_along(joint_var)) {
    distances[i] <- SRxgboost_calculate_distance(data_old[,i], data_new[,i], bins = n_bins)
  }
  #
  # stats
  df <- data.frame(variables = names(distances), drift = distances) %>%
    dplyr::mutate(stars = paste0(ifelse(drift > 0.1 & drift < 0.2, ".", ""),
                                 ifelse(drift > 0.2, "*", ""),
                                 ifelse(drift > 0.3, "*", ""),
                                 ifelse(drift > 0.4, "*", "")),
                  stars = factor(stars, levels = c("***", "**", "*", ".", "")))
  class(df) <- c("covariate_drift", "data.frame")
  saveRDS(df, paste0(ifelse(name == "", "", name), "Data Drift.rds"))
  #
  # plots
  for (plot_i in min(n_plots, length(joint_var)):1) {
    # get variable
    variable <- df %>%
      dplyr::arrange(-drift) %>%
      dplyr::slice(plot_i)
    variable_name <- variable %>%
      dplyr::pull(variables) %>%
      as.character()
    # count df_old and data_new
    suppressWarnings(
      temp <- data_old %>%
        {if (SRfunctions::SR_is_number(data_old[, variable_name]) &
             length(unique(data_old[, variable_name])) > 20)
          dplyr::mutate(., !!variable_name := cut(.[, variable_name],
                                                  breaks = pretty(.[, variable_name],
                                                                  n_bins),
                                                  include.lowest = TRUE, dig.lab = 10,
                                                  ordered_result = TRUE))
          else .} %>%
        dplyr::count(!!variable_name := .[, variable_name]) %>%
        dplyr::mutate(df_old = n / sum(n)) %>%
        dplyr::full_join(data_new %>%
                           {if (SRfunctions::SR_is_number(data_old[, variable_name]) &
                                length(unique(data_old[, variable_name])) > 20)
                             dplyr::mutate(., !!variable_name := cut(.[, variable_name],
                                                                     breaks = pretty(data_old[, variable_name],
                                                                                     n_bins),
                                                                     include.lowest = TRUE,
                                                                     dig.lab = 10,
                                                                     ordered_result = TRUE))
                             else .} %>%
                           dplyr::count(!!variable_name := .[, variable_name]) %>%
                           dplyr::mutate(df_new = n / sum(n)),
                         by = variable_name) %>%
        dplyr::mutate(., !!variable_name :=
                        SRfunctions::SR_feat_eng_numeric(.[, variable_name],
                                                         replace_NA_special_value = TRUE)[, 1]) %>%
        dplyr::mutate(df_old = tidyr::replace_na(df_old, 0),
                      df_new = tidyr::replace_na(df_new, 0)))
    # plot
    p <- temp %>%
      dplyr::select(-n.x, -n.y) %>%
      reshape2::melt(id = variable_name) %>%
      ggplot2::ggplot(ggplot2::aes(x = .[, 1], y = value, colour = variable,
                                   group = variable)) +
      ggplot2::geom_line(na.rm = TRUE) +
      ggplot2::geom_point(na.rm = TRUE) +
      {if (!SRfunctions::SR_is_number(data_old[, variable_name]))
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0,
                                                           vjust = 0.5))} +
      ggplot2::theme(legend.position = "top") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
      ggplot2::labs(x = variable_name, y = "Frequency", colour = "",
                    title = "Vergleich der Häufigkeiten",
                    subtitle = paste0("Drift = ",
                                      round(df$drift[df$variables == variable_name], 3)))
    print(p)
    ggplot2::ggsave(paste0(ifelse(name == "", "", name), "Data Drift ",
                           plot_i, " ", variable_name, ".png"),
                    width = 9.92, height = 5.3)
    # clean up
    rm(variable, temp, p)
  }; rm(plot_i)
  #
  # plot overall
  p <- df %>%
    dplyr::arrange(-drift) %>%
    {if (nrow(.) > 30) dplyr::slice(., 1:30) else .} %>%
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(variables, -drift), y = drift, fill = stars)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5),
                   legend.position = "top") +
    ggplot2::scale_fill_brewer(palette = "OrRd", direction = -1) +
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6), limits = c(0, 1)) +
    ggplot2::labs(x = variable_name, y = "Frequency", fill = "Significance", title = "Data Drift")
  print(p)
  ggplot2::ggsave(paste0(ifelse(name == "", "", name), "Data Drift 0 Overall.png"),
                  width = 9.92, height = 5.3)
  #
  # return df
  return(df)
}
