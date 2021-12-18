#' Function that given a list of learners and a task and further information creates an ensemble random forrest learner using a stacking approack
#'
#' @param task tesk
#' @param type type
#' @param learners learners
#' @param feature_filter feature_filter
#' @param inner_resampling inner resampling
#' @param measure measure
#' @param terminator terminator
#' @param tuner tuner
#'
#' @return stacking_ensemble
#'
#' @export
#'
#' @import mlr3verse
#'

create_stacking_ensemble <- function(task, type, learners, feature_filter, inner_resampling, measure, terminator, tuner){

  len <- length(learners)

  learners_c <- create_clones(learners)

  po_cv <- function(learner){
    learner_po_cv <- po("learner_cv", learner)

    return(learner_po_cv)
  }

  lrns_po_cv <- lapply(learners_c, po_cv)

  level_0 <- gunion(list(lrns_po_cv, po("nop")))

  combined <- level_0 %>>% po("featureunion", innum = len + 1)

  #### get rf autotuner
  rf_base <- create_learners(task, "rf")
  rf_ss <- create_search_space(task, rf_base)
  rf_rob <- create_robust_learners(task, rf_ss)

  if (feature_filter != "no") {
    rf_ff <- create_feature_filter(task, type, rf_rob, feature_filter)
    rf_at <- create_auto_tuner(rf_ff, inner_resampling, measure, terminator,tuner)
  } else {
    rf_at <- create_auto_tuner(rf_rob, inner_resampling, measure, terminator,tuner)
  }

  random_forrest <- rf_at[[1]]
  random_forrest$id <- "random_forrest"

  #### end rf autotuner

  level_1 <- combined %>>% rf_at

  stacking_ensemble <- as_learner(level_1)

  stacking_ensemble$id <- "stacking_ensemble"

  return(stacking_ensemble)
}
