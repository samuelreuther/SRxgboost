#' SRxgboost_calculate_distance
#'
#' Calculates the distance of two vectors.
#'
#' @param lauf character
#' @param rank integer
#' @param plots boolean
#' @param min_rel_Gain numeric
#' @param sample integer
#' @param pdp_sample integer
#' @param pdp_cuts integer
#' @param n_core character
#' @param silent boolean
#'
#' @return numeric value
#'
#' @export
SRxgboost_calculate_distance <- function(variable_old,
                                         variable_new,
                                         bins = 5) {
  if ("character" %in% class(variable_old)) {
    variable_old <- factor(as.character(variable_old))
    variable_new <- factor(as.character(variable_new))
  }
  if ("factor" %in% class(variable_old)) {
    variables <- unique(as.character(c(levels(variable_old), levels(variable_new))))
    variable_old <- factor(as.character(variable_old), levels = variables)
    variable_old <- forcats::fct_explicit_na(variable_old)
    variable_new <- factor(as.character(variable_new), levels = variables)
    variable_new <- forcats::fct_explicit_na(variable_new)
    after_cuts <- c(variable_old, variable_new)
  } else {
    if (length(unique(variable_old)) <= 20) {
      after_cuts <- c(variable_old, variable_new)
    } else {
      after_cuts <- cut(rank(c(variable_old, variable_new)),
                        breaks = unique(stats::quantile(rank(c(variable_old, variable_new)),
                                                        probs = seq(0, 1.01, by = 1/5),
                                                        na.rm = TRUE)),
                        ordered_result = TRUE, dig.lab = 10, include.lowest = TRUE,
                        labels = FALSE)
      # after_cuts <- cut(rank(c(variable_old, variable_new)), breaks = bins)
    }
  }
  #
  after_cuts_table <- table(after_cuts, c(rep(1, length(variable_old)),
                                          rep(2, length(variable_new))))
  mat <- matrix(c(prop.table(after_cuts_table, 2)), nrow = 2, byrow = TRUE)
  # assign("drift_table", after_cuts_table, envir = .GlobalEnv)
  # assign("drift_distance", mat, envir = .GlobalEnv)
  #
  # return distance
  1 - sum(apply(mat, 2, min))
}
