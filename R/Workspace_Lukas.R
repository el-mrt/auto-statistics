# library(tidyverse)
# library(mlr3verse)
# library(autoStatistics)
#
# rm(param_list)
#
#
# load("C:/Users/lukas/Desktop/test.RData")
# t1 <- param_list
#
# load("C:/Users/lukas/Desktop/test_auto.RData")
# t2 <- param_list
#
# load("C:/Users/lukas/Desktop/test_ohne_ho.RData")
# t3 <- param_list
#
# auto_to_null <- function(x){
#   if ("R6" %in% class(x)) {
#     return(x)
#   }
#
#   if ("auto" %in% x) {
#     x <- NULL
#   }
#   return(x)
# }
#
# lapply(t1, auto_to_null)
# lapply(t2, auto_to_null)
# lapply(t3, auto_to_null)

#####

# # task <- create_task(iris, "Species")
# task <- tsk("german_credit")
# measure <- create_measure(task, "classif.acc")
# outer_resampling <- create_resampling(task, strat = "cv", params = list(folds = 10))
#
# #### HPO FALSE
#
# l <- create_learners(task)
# l <- create_graph_learner(task, l)
#
# #### HPO TRUE
#
# inner_resampling <- create_resampling(task, strat = "holdout", params = list(0.8))
# terminator <- create_terminator(vec_terminators = list(c("eval", 3000)))
# tuner <- create_tuner("cmaes")
#
# l <- create_learners(task)
# l <- create_search_space(task, l)
# l <- create_auto_tuner(l, inner_resampling, measure, terminator, tuner)
# l <- create_graph_learner(task, l)
#
# #### end if else
#
# l <- c(l, lrn("classif.featureless"))
#
#
# design <- benchmark_grid(
#   task = task,
#   resamplings = outer_resampling,
#   learners = l
# )
#
# bmr <- benchmark(design)
#
# bmr %>% autoplot(measure = measure)
# #
# kknn <- lrn("classif.kknn")
# kknn$param_set$values$k <- to_tune(1,7)
# kknn_tuned <- AutoTuner$new(
#   learner = kknn,
#   resampling = inner_resampling,
#   measure = measure,
#   terminator = terminator,
#   tuner = tuner
# )
#
# kknn_tuned



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

