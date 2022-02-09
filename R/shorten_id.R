#' Function that given a list of learners shortens their id to exclude everything before "regr." or "classif." depending on task type
#'
#' @param learners list of learners
#' @param task_type task type
#'
#' @return learners with shortened ids
#'
#' @export
#'
#' @import mlr3verse
#'

shorten_id <- function(learners, task_type){
s_id <- function(learner, task_type){
  learner$id <- switch (task_type,
                        regr = sub(".*regr.", "", learner$id),
                        classif = sub(".*classif.", "", learner$id)
  )
  return(learner)
}

learners <- lapply(learners, s_id, task_type = task_type)

return(learners)
}
