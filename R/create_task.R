#' Function that given a dataframe
#'
#' @param df input data frame
#' @param target_var target variable
#'
#' @examples
#' create_task(iris, "Species")
#' create_task(mtcars, 1)
#'
#' @return returns a mlr3 task object
#'
#' @export
#'
#' @import mlr3
#'

create_task <- function(df, target_var){
  type <- identify_CR(df, target_var)

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
