#' Function that given a dataframe and variable creates an mlr3 task
#'
#' @param df input data frame
#' @param target_var target variable
#' @param type type of task. ´regr´ or ´classif´. if ´NULL´ the type is estimated.
#' @param rm_na_target remove NAs from target column?
#'
#' @examples
#' create_task(iris, "Species")
#' create_task(mtcars, 1)
#'
#' @return returns a mlr3 task object
#'
#' @export
#'
#' @import mlr3verse
#'

create_task <- function(df, target_var, type = NULL, rm_na_target = TRUE){
  if(rm_na_target){
    df <- df[!is.na(df[[{{ target_var }}]]), ]
  }
  if(is.null(type)){
    type <- identify_CR(df, target_var)
  }

  #check for numeric input of target variable
  if (is.numeric(target_var)) {
    target_var <- colnames(df)[target_var]
  }

  if (type == "regr") {
    task <- mlr3::as_task_regr(df, target = target_var)
  } else if (type == "classif") {
    #check two class vs multi class classification
    if (length(unique(target_var)) == 2) {
      positive <- levels(df[[{{ target_var }}]])[1]

      task <- mlr3::as_task_classif(df, target = target_var, positive = positive)
    } else{
      task <- mlr3::as_task_classif(df, target = target_var)
    }
  } else stop("variable type not supported")

  return(task)
}
