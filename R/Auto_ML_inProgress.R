#' Function that applies Auto ML Pipeline
#'
#' @param df dataframe
#' @param target_var target variable
#'
#' @examples
#'
#' @return
#'
#' @export
#'
#' @import mlr3verse
#'



# task <- create_task(iris, "Species")
# learner <- create_learners(task, vec_learners = c("rf"))
# resampling <- create_resampling(task, strat = "holdout", params = list(0.8))
# measure <- create_measure(task)
# learner <- create_search_space(task = task, learners = learner)
# terminator <- create_terminator(vec_terminators = list(c("eval", 10), c("rt", 1000)))
# tuner <- create_tuner()
# outer_resampling <- create_resampling(task)
#
# at <- AutoTuner$new(
#   learner = learner[[1]],
#   resampling = resampling,
#   measure = measure,
#   terminator = terminator,
#   tuner = tuner)
#
# rr <- resample(task, at, outer_resampling, store_models = TRUE)
#
#
# rr$aggregate()
#
# rr$score() %>% as.data.table() %>% View("score")
