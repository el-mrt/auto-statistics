#' Function to find out, if the Task is Classification or Regression
#'
#' @param df input data frame
#' @param target_var target variable
#' @param classif_threshold max number of unique values for detecting a classification
#' @examples
#' identify_CR(mtcars, "mpg")
#' identify_CR(iris, 5)
#'
#' @return returns a string of `regr` or `classif`
#'
#' @export
#'

identify_CR <- function(df, target_var, classif_threshold = 6){

  #test if target variable is a string or number
  if (is.numeric(target_var)) {
    if (target_var > ncol(df)) {
      stop("target variable out of bounds")
    }
    target_vector <- df[, target_var]
  } else if (is.character(target_var)) {
    if (! target_var %in% colnames(df)) {
      stop("target variable does not exist in data frame")
    }
    target_vector <- df[[{{ target_var }}]]
  } else stop("Input for target variable not supported")

  #transforms to ordered / factor if necessary
  target_vector <- var_format(target_vector, classif_threshold)

  #test classification or regression
  if (is.numeric(target_vector)){
    return("regr")
  } else return("classif")
}
