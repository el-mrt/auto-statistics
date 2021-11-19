#' Function that given a list of learners, defines suitable search_space for HPO
#'
#' @param task taskk
#' @param learners list of learners
#'
#' @examples
#'
#' @return list of mlr3 learners with parameter search_space
#'
#' @export
#'
#' @import mlr3verse
#'

create_search_space <- function(task, learners){
  cr <- task$task_type

  for (l in learners) {
    id <- sub(paste0(cr, "."), "",l$id)

    if (id == "kknn") {
      l$param_set$values$k <- to_tune(1, round(sqrt(task$nrow)))
    } else if (id == "svm") {
      l$param_set$values$cost <- to_tune(1e-10, 10, logscale = TRUE)
    } else if (id == "ranger") {
      l$param_set$values$min.node.size <- to_tune(1, 5)
    } else if (id == "xgboost") {
      l$param_set$values$eta <- to_tune(0, 1)
    }
  }

  return(learners)
}

lrns <- create_learners(tsk("iris"))
lrns_t <- create_search_space(tsk("iris"), lrns)
lrns_t
