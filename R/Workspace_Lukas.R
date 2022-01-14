# TODO
# given param_list and the bmr, bmr_best - write a markdown document that encapsulates the important information
# in best_benchmark -> change names form l1, ..., ln to original learner name- if name occurs multiple times: append 1, ..., m
#
# library(tidyverse)
# library(mlr3verse)
# library(autoStatistics)
# rm(list = ls())
#
# load("C:/Users/lukas/Desktop/param_list.RData")
#
# param_list
#
# param_list$na_imp <- "auto"
# param_list$learners <- c("rf", "svm", "xgb", "knn")
# param_list$terminator <- list(c("eval", 1))
# param_list$o.resampling$params <- 0.8
# param_list$o.resampling$method <- "holdout"
# param_list$i.resampling$params <- 0.8
# param_list$i.resampling$method <- "holdout"
# param_list$hpo_base_learner <- FALSE
# param_list$feature_filter <- "information_gain"
# param_list$incl_featureless <- FALSE
# param_list$tuning_method <- "random_search"
# param_list$ensemble <- c("stacking")
# param_list$incl_at <- FALSE
# param_list$n_best <- 5
#
# save(param_list, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Applications in Data Analytics/RData_files/param_list.RData")
#
# t1 <- perform_auto_ml(param_list)
#
# # save(t1, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Applications in Data Analytics/RData_files/pam_004.RData")
#
# ####
#
# t1$bmr %>% autoplot(measure = t1$measure)
# t1$bmr_best %>% autoplot(measure = t1$measure)
#
# score <- t1$bmr_best$score(t1$measure) %>% as.data.table %>% arrange(!!sym(t1$measure$id))
#
# score$learner[[1]]$predict(param_list$task)
#
