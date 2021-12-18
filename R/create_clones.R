#' Function that given a list of learners, creates a list of cloned learners
#'
#' @param learners learners
#'
#' @return list of cloned learners
#'
#' @export
#'
#' @import mlr3verse
#'

create_clones <- function(learners){
  clone <- function(learner){
    learner_c <- learner$clone()

    return(learner_c)
  }

  learners_c <- lapply(learners, clone)

  return(learners_c)
}
