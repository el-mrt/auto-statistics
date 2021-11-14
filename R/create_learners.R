#' Function that given a task returns a suitable list of learners
#'
#' @param task mlr3 task
#' @param vec_learners vector of choosen learners
#' @param hpo choose if searchspaces for hyperparameter optimization should be initialized
#'
#' @examples
#' create_learners(tsk("iris"))
#' create_learners(tsk("bike_sharing"))
#' create_learners(task = tsk("german_credit"), vec_learners = c("rf"), hpo = FALSE)
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

  if (hpo == TRUE) {
    for (l in learners) {
      id <- sub(paste0(cr, "."), "",l$id)

      if (id == "kknn") {
        l$param_set$values$k <- to_tune(1, round(task$nrow / 10))
      } else if (id == "svm") {
        l$param_set$values$cost <- to_tune(1e-10, 10, logscale = TRUE)
      } else if (id == "ranger") {
        l$param_set$values$min.node.size <- to_tune(1, 5)
      } else if (id == "xgboost") {
        l$param_set$values$eta <- to_tune(0,1)
      }
    }
  }

  return(learners)
}
