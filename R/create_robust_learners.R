#' Function that given a list of learners and a task creates a robust graphlearners
#'
#' @param task task
#' @param learners list of learners
#'
#' @return an mlr3 graph learner
#'
#' @export
#'
#' @import mlr3verse
#'

create_robust_learners <- function(task, learners){

  create_graph <- function(task, learner){
    graph <- ppl("robustify",
                 task = task,
                 learner = learner) %>>%
      learner

    gl <- GraphLearner$new(graph)
    return(gl)
  }

  g_learner <- lapply(learners, create_graph, task = task)

  return(g_learner)
}
