# library(tidyverse)
# library(mlr3verse)
# library(autoStatistics)
#
# # rm(list = ls())
#
# load("C:/Users/lukas/Desktop/param_list.RData")
#
# param_list
#
# param_list$na_imp <- "auto"
# param_list$learners <- c("knn", "svm", "xgb", "ranger")
# param_list$terminator <- list(c("rt", 120), c("evals", 30))
# param_list$o.resampling$params <- 5
# param_list$o.resampling$method <- "cv"
# param_list$i.resampling$params <- 2
# param_list$i.resampling$method <- "cv"
# param_list$hpo_base_learner <- TRUE
# param_list$feature_filter <- "information_gain"
# param_list$incl_featureless <- TRUE
# param_list$tuning_method <- "cmaes"
#
# t1 <- perform_auto_ml(param_list)
#
# bmr$score() %>% as.data.table  %>% arrange(regr.mse) %>% View("bmr")
#
# score <- bmr$score() %>% as.data.table %>% arrange(regr.mse)
#
# View(score, "score")
#
# score$learner[[1]]$archive %>% as.data.table %>% arrange(regr.rmse) %>% View("l1_archive")
# score$learner[[2]]$archive %>% as.data.table %>% arrange(regr.rmse) %>% View("l2_archive")
# score$learner[[1]]$archive
#
# score$learner[[1]]$learner$param_set$values
# score$learner[[2]]$learner$param_set$values
# score$learner[[3]]$param_set$values
#
# l1 <- score$learner[[1]]$base_learner(recursive = 0)
# l2 <- score$learner[[2]]$base_learner(recursive = 0)
# l3 <- score$learner[[3]]$base_learner(recursive = 0)
#
# l1$id <- "l1"
# l2$id <- "l2"
# l3$id <- "l3"
#
# ensemble <- GraphLearner$new(gunion(list(l1,l2,l3)) %>>% po("regravg", innum = 3))
#
# e_lrns <- list(
#   l1,l2,l3,ensemble, lrn("regr.featureless")
# )
#
#
# bmr_ensemble <- benchmark_grid(task, resamplings = rsmp("cv", folds = 20), learners = e_lrns) %>% benchmark()
#
# bmr_ensemble %>% autoplot(measure= measure)
# bmr$score(measures = measure)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
