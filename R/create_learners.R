#' Function that given a task returns a suitable list of learners
#'
#' @param task mlr3 task
#' @param vec_learners vector of choosen learners
#'
#' @examples
#' create_learners(tsk("iris"))
#' create_learners(tsk("bike_sharing"))
#' create_learners(task = tsk("german_credit"), vec_learners = c("rf"))
#'
#' @return list of mlr3 learners
#'
#' @export
#'
#' @import mlr3verse e1071 ranger xgboost kknn
#'

create_learners <- function(task, vec_learners = NULL, hpo = TRUE){
  cr <- task$task_type

  make_name <- function(cr, lrn){
    paste0(cr, ".", lrn)
  }

  if (is.null(vec_learners)) {
    lst <- list(
      make_name(cr, "kknn"),
      make_name(cr, "svm"),
      make_name(cr, "ranger"),
      make_name(cr, "xgboost")
    )
  } else{
    lst <- list()

    for (l in vec_learners) {
      mlr_name <- switch(l,
                         knn = make_name(cr, "kknn"),
                         svm = make_name(cr, "svm"),
                         rf = make_name(cr, "ranger"),
                         xgb = make_name(cr, "xgboost")
      )
      lst <- c(lst, mlr_name)
    }
  }

  learners <- lrns(lst)

  return(learners)
}
