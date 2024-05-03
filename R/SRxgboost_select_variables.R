#' SRxgboost_select_variables
#'
#' Take the results of a previous run and deduct relevant variables:
#' - by comparing the information gain against the mean of random_numeric,
#'   random_binary and random_category variables created in 'SRxgboost_data_prep'
#' - removing from highly correlated variables (default threshold = 0.9 like the
#'   default value in recipes::step_corr()) the variable with lower information gain
#' Requirements of previous run with all variables provided at 'lauf_all_variables':
#' - run the following 3 functions:
#'   - 'SRxgboost_data_prep(..., add_random_variables = TRUE)'
#'   - 'SRxgboost_run' and
#'   - 'SRxgboost_plots'
#' Returns a data.frame with relevant variables and their variable importance
#'
#' @param lauf_all_variables character
#' @param threshold_cor numeric (default = 0.9)
#'
#' @return data.frame with relevant variables and their variable importance
#'
#' @export
SRxgboost_select_variables <- function(lauf_all_variables,
                                       threshold_cor = 0.9) {
  ### checks
  #
  # check path_output exists
  if (!exists("path_output")) {
    cat("Error: 'path_output' is missing. \n")
    return(invisible(NULL))
  }
  #
  # check lauf_all_variables ends with ".csv"
  if (!grepl('.csv$', lauf_all_variables))
    lauf_all_variables <- paste0(lauf_all_variables, ".csv")
  #
  #
  #
  ### load data
  #
  # load importance_matrix from provided lauf_all_variables
  importance_matrix <-
    utils::read.table(paste0(path_output, gsub(".csv", "/", lauf_all_variables),
                             "Best Model/VarImp 0.csv"),
                      header = TRUE, sep = ";", dec = ",")
  # check
  if (sum(grepl("random", importance_matrix$Feature)) == 0) {
    # this happens if the model is very simple because of few training data!
    # stop("SRxgboost_select_variables: Randomly generated variables are expected!\n",
    #      "The function 'SRxgboost_data_prep' for lauf_all_variables = '", lauf_all_variables,
    #      "needs to be generated with parameter 'add_random_variables = TRUE' ")
    warning("SRxgboost_select_variables: Randomly generated variables are expected!\n",
            "The function 'SRxgboost_data_prep' for lauf_all_variables = '", lauf_all_variables,
            "needs to be generated with parameter 'add_random_variables = TRUE' \n",
            "If the model is very simple because of few training data, this might ",
            "happen by chance anyway. \n",
            " => threshold_gain will be set arbitrary to 0.001")
    threshold_gain <- 0.001
  } else {
    threshold_gain <- mean(importance_matrix$Gain[grepl("random", importance_matrix$Feature)])
  }
  #
  # load prepared training data for correlation analysis
  datenModell <- readRDS(paste0(path_output, gsub(".csv", "/", lauf_all_variables),
                                "/Data/datenModell.rds"))
  #
  #
  #
  ### select variables with higher Gain than mean Random_...-variables
  #
  sel_vars <- importance_matrix %>%
    dplyr::mutate(Select = ifelse(Gain >= threshold_gain &
                                    !grepl("random", Feature),
                                  TRUE, FALSE))
  #
  #
  #
  ### deselect highly correlated variables
  #
  # calculate correlation
  SRfunctions::SR_correlation_plot(datenModell %>%
                                     dplyr::select(dplyr::all_of(sel_vars$Feature)))
  #
  # determine variables with high correlaction
  cor_deselection <- cor_matrix %>%
    data.frame() %>%
    tibble::rownames_to_column(var = "Var1") %>%
    reshape2::melt(id = "Var1", variable.name = "Var2", value.name = "Correlation") %>%
    dplyr::filter(abs(Correlation) >= threshold_cor) %>%
    dplyr::filter(Var1 != Var2) %>%
    dplyr::mutate(Var1_ = as.character(Var1),
                  Var2_ = as.character(Var2)) %>%
    dplyr::mutate(Var1 = ifelse(Var1_ < Var2_, Var1_, Var2_),
                  Var2 = ifelse(Var2_ < Var1_, Var1_, Var2_)) %>%
    dplyr::distinct(Var1, Var2, Correlation) %>%
    dplyr::left_join(sel_vars %>%
                       dplyr::select(Feature, Gain_Var1 = Gain),
                     by = c("Var1" = "Feature")) %>%
    dplyr::left_join(sel_vars %>%
                       dplyr::select(Feature, Gain_Var2 = Gain),
                     by = c("Var2" = "Feature")) %>%
    dplyr::mutate(Gain_Diff = abs(Gain_Var1 - Gain_Var2),
                  Remove = ifelse(Gain_Var1 < Gain_Var2, Var1, Var2),
                  Notremove = ifelse(Gain_Var1 < Gain_Var2, Var2, Var1),
                  Correlation_with = paste0(round(Correlation, 3), " with ", Notremove)) %>%
    arrange(-Gain_Diff)
  #
  # check that not 2+ interacting variables get removed
  cor_deselection_clean <- data.frame()
  for (i in unique(cor_deselection$Remove)) {
    cor_deselection_clean <-
      dplyr::bind_rows(cor_deselection_clean,
                       cor_deselection %>%
                         dplyr::filter(Remove == i) %>%
                         dplyr::filter(!Notremove %in% cor_deselection_clean$Remove) %>%
                         dplyr::distinct(Remove, .keep_all = TRUE))
  }; rm(i)
  #
  # remove highly correlated variables from sel_vars
  sel_vars <- sel_vars %>%
    dplyr::left_join(cor_deselection_clean %>%
                       dplyr::select(Remove,
                                     Highest_Correlation = Correlation,
                                     Highest_Correlation_with = Notremove),
                     by = c("Feature" = "Remove")) %>%
    dplyr::mutate(Select = ifelse(!is.na(Highest_Correlation), FALSE, Select)) %>%
    dplyr::mutate(Feature = gsub("_LabelEnc", "", Feature))
  #
  # export result
  saveRDS(sel_vars, paste0(path_output, gsub(".csv", "/", lauf_all_variables),
                           "Best Model/0 Variable Selection.rds"))
  #
  # plot result
  sel_vars_temp <- sel_vars %>%
    dplyr::mutate(Feature = factor(Feature,
                                   levels = rev(stats::reorder(sel_vars$Feature,
                                                               sel_vars$Gain))),
                  Select = factor(Select, levels = c("FALSE", "TRUE"))) %>%
    dplyr::slice(1:(min(nrow(sel_vars), 30))) %>%
    dplyr::select(Feature, Gain, Frequency, Select, Highest_Correlation,
                  Highest_Correlation_with)
  p <- sel_vars_temp %>%
    dplyr::select(Feature, Gain, Frequency, Highest_Correlation) %>%
    reshape2::melt(id.vars = "Feature") %>%
    dplyr::left_join(sel_vars_temp %>%
                       dplyr::select(Feature, Select),
                     by = "Feature") %>%
    dplyr::left_join(sel_vars_temp %>%
                       dplyr::select(Feature, Highest_Correlation_with),
                     by = "Feature") %>%
    dplyr::mutate(Highest_Correlation_with = ifelse(variable == "Highest_Correlation",
                                                    Highest_Correlation_with, "")) %>%
    ggplot2::ggplot(ggplot2::aes(y = value, x = Feature, fill = Select,
                                 label = Highest_Correlation_with)) +
    ggplot2::geom_bar(position = "dodge", stat = "identity", na.rm = TRUE) +
    ggplot2::geom_text(ggplot2::aes(y = value / 2), na.rm = TRUE, size = 2) +
    ggplot2::labs(x = "Variable", y = "") +
    ggplot2::scale_y_continuous(labels = scales::percent, breaks = scales::pretty_breaks(4)) +
    ggplot2::scale_fill_discrete(drop = FALSE) +
    ggplot2::facet_grid(~variable, scales = "free") +
    ggplot2::coord_flip() +
    ggplot2::guides(fill = "none")
  print(p)
  ggplot2::ggsave(plot = p,
                  filename = paste0(path_output, gsub(".csv", "/", lauf_all_variables),
                                    "Best Model/0 Variable Selection.png"),
                  width = 9.92, height = 5.3)  # 4.67
  #
  #
  #
  ### clean up
  #
  suppressWarnings(try(rm(importance_matrix, datenModell, cor_matrix,
                          cor_deselection, cor_deselection_clean, sel_vars_temp)))
  suppressWarnings(try(rm(cor_matrix, envir = .GlobalEnv)))
  # suppressWarnings(try(rm(cor_matrix, inherits = TRUE)))
  invisible(gc())
  #
  #
  #
  ### return sel_vars
  return(sel_vars)
}

