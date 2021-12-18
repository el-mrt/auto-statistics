#' Function that given a list of graph learners, an inner resampling, a measure, a terminator and a tuner creates a list of auto tuner learners
#'
#' @param learners list of graph_learners
#' @param inner_resampling inner resampling strategy
#' @param measure measure
#' @param terminator termination criterion
#' @param tuner tuning procedure#'
#'
#' @return an mlr3 graph learner
#'
#' @export
#'
#' @import mlr3verse


create_auto_tuner <- function(learner, inner_resampling, measure, terminator, tuner){

  create_auto_tuner_single_learner <- function(learner, inner_resampling, measure, terminator, tuner){
    AutoTuner$new(
      learner = learner,
      resampling = inner_resampling,
      measure = measure,
      terminator = terminator,
      tuner = tuner
    )
  }

  at_learners <- lapply(learner, create_auto_tuner_single_learner,
         inner_resampling = inner_resampling,
         measure = measure,
         terminator = terminator,
         tuner = tuner)

  return(at_learners)
}
