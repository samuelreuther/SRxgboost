#' SRxgboost_select_variables
#'
#' Take the results of a previous run and deduct relevant variables:
#' - by comparing the information gain against the mean of random_numeric,
#'   random_binary and random_category variables created in 'SRxgboost_data_prep'
#' - removing from highly correlated variables (default threshold = 0.8) the
#'   variable with lower information gain
#' Requirements of previous run with all variables provided at 'lauf_all_variables':
#' - run the following 3 functions:
#'   - 'SRxgboost_data_prep(..., add_random_variables = TRUE)'
#'   - 'SRxgboost_run' and
#'   - 'SRxgboost_plots'
#' Returns a data.frame with relevant variables and their variable importance
#'
#' @param lauf_all_variables character
#' @param threshold_cor numeric (default = 0.8)
#'
#' @return data.frame with relevant variables and their variable importance
#'
#' @export
SRxgboost_select_variables <- function(lauf_all_variables,
                                       threshold_cor = 0.8) {
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
  ### load importance_matrix from provided lauf_all_variables
  #
  # load data
  importance_matrix <-
    utils::read.table(paste0(path_output, gsub(".csv", "/", lauf_all_variables),
                             "Best Model/0 Variable importance.csv"),
                      header = TRUE, sep = ";", dec = ",")
  #
  # check
  if (sum(grepl("random", importance_matrix$Feature)) == 0) {
    stop("SRxgboost_select_variables: Randomly generated variables are expected!\n",
         "The function 'SRxgboost_data_prep' for lauf_all_variables = '", lauf_all_variables, "'\n",
         "needs to be generated with parameter 'add_random_variables = TRUE'!")
  }
  #
  #
  #
  ### select variables with higher Gain than mean Random_...-variables
  #
  sel_vars <- importance_matrix %>%
    dplyr::mutate(Random = grepl("random", Feature)) %>%
    dplyr::mutate(Select = ifelse(Gain >= mean(.$Gain[.$Random]) &
                                    !Random,
                                  TRUE, FALSE)) %>%
    # dplyr::filter(Gain >= mean(.$Gain[.$Random]),
    #               !Random) %>%
    dplyr::select(-Random)
  #
  #
  #
  ### deselect highly correlated variables
  #
  # load prepared training data
  datenModell <- readRDS(paste0(path_output, gsub(".csv", "/", lauf_all_variables),
                                "/Data/datenModell.rds"))
  #
  # calculate correlation
  SRfunctions::SR_correlation_plot(datenModell %>%
                                     dplyr::select(dplyr::all_of(sel_vars$Feature)))
  #
  # determine variables with high correlaction
  cor_deselection <- cor_matrix %>%
    data.frame() %>%
    tibble::rownames_to_column(var = "Var1") %>%
    reshape2::melt(id = "Var1", variable_name = "Var2") %>%
    dplyr::filter(value >= threshold_cor) %>%
    dplyr::filter(Var1 != Var2) %>%
    dplyr::mutate(Var1_ = as.character(Var1),
                  Var2_ = as.character(Var2)) %>%
    dplyr::mutate(Var1 = ifelse(Var1_ < Var2_, Var1_, Var2_),
                  Var2 = ifelse(Var2_ < Var1_, Var1_, Var2_)) %>%
    dplyr::distinct(Var1, Var2, value) %>%
    dplyr::left_join(sel_vars %>%
                       dplyr::select(Feature, Gain_Var1 = Gain),
                     by = c("Var1" = "Feature")) %>%
    dplyr::left_join(sel_vars %>%
                       dplyr::select(Feature, Gain_Var2 = Gain),
                     by = c("Var2" = "Feature")) %>%
    dplyr::mutate(Gain_Diff = abs(Gain_Var1 - Gain_Var2),
                  Remove = ifelse(Gain_Var1 < Gain_Var2, Var1, Var2),
                  Notremove = ifelse(Gain_Var1 < Gain_Var2, Var2, Var1),
                  Correlation = paste0(round(value, 3), " with ", Notremove)) %>%
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
                       dplyr::select(Remove, Correlation),
                     by = c("Feature" = "Remove")) %>%
    dplyr::mutate(Select = ifelse(!is.na(Correlation),
                                  FALSE, Select)) %>%
    # dplyr::filter(!Feature %in% cor_deselection_clean$Remove) %>%
    dplyr::mutate(Feature = gsub("_LabelEnc", "", Feature))
  #
  #
  #
  ### clean up
  #
  suppressWarnings(try(rm(importance_matrix, datenModell, cor_matrix,
                          cor_deselection, cor_deselection_clean)))
  suppressWarnings(try(rm(cor_matrix, envir = .GlobalEnv)))
  # suppressWarnings(try(rm(cor_matrix, inherits = TRUE)))
  invisible(gc())
  #
  #
  #
  ### return sel_vars
  return(sel_vars)
}

