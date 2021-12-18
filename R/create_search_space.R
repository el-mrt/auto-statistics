#' Function that given a list of learners, defines suitable search_space for HPO
#'
#' @param task taskk
#' @param learners list of learners
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
      if (cr == "regr") { # type has to be set in order to tune cost
        l$param_set$values$type <- "eps-regression"
      } else {
        l$param_set$values$type <- "C-classification"
      }
      l$param_set$values$cost <- to_tune(1e-10, 10, logscale = TRUE)
      l$param_set$values$kernel <- "radial" # kernel has to be set in order to tune gamma
      l$param_set$values$gamma <- to_tune(1e-10, 10, logscale = TRUE)
    } else if (id == "ranger") {
      # mtry has issues with feature filter-- filter might return small subset, which would be OOB for param search
      # l$param_set$values$mtry <- to_tune(1, round(task$ncol/2))
      l$param_set$values$num.trees <- to_tune(1,1e4)
      l$param_set$values$min.node.size <- to_tune(1, 5)
    } else if (id == "xgboost") {
      l$param_set$values$eta <- to_tune(0, 1)
      l$param_set$values$nrounds <- to_tune(1, 1e4)
    }
  }

  return(learners)
}
