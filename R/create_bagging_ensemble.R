#' Function that given a list of learners and a task creates an ensemble learner using a bagging approack
#'
#' @param task tesk
#' @param learners learners
#'
#' @return bagging_ensemble
#'
#' @export
#'
#' @import mlr3verse
#'

create_bagging_ensemble <- function(learners, task_type){

  len <- length(learners)

  learners_c <- create_clones(learners)

  gu_learners <- gunion(learners_c)

  average <- switch (task_type,
                     regr = "regravg",
                     classif = "classifavg")

  po_avg <- po(average, innum = len)

  graph <- gu_learners %>>% po_avg

  bagging_ensemble <- as_learner(graph)

  bagging_ensemble$id <- "bagging_ensemble"

  return(bagging_ensemble)
}
