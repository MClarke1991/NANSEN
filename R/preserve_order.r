## Copyright 2019 Matthew A. Clarke, Fisher Lab, UCL <matthew.clarke@ucl.ac.uk>

#' Set a column as a factor, which the levels in the current order
#'
#' Use to preserve the ordering of a data frame, so that when it is passed
#' to a ggplot function the plotting preserves that order
#'
#' @param df dataframe in desired ordering
#' @param col column name which determines the order
#'
#' @return dataframe where col has been frozen in desired order as a factor
#'
#' @export
preserve_order <- function(df, col) {
  df[[col]] <- factor(df[[col]], levels = unique(df[[col]]))
  return(df)
}