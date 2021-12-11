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
# t1$bmr %>% autoplot(measure = t1$measure)
#
# save(t1, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Applications in Data Analytics/RData_files/t1.RData")
#
# t1$measure$id
#
# aggregate <- t1$bmr$aggregate(measures = t1$measure) %>% as.data.table
# aggregate$resample_result[[1]]$score()$learner[[1]]$archive %>% as.data.table %>% arrange(regr.rmse)
#
#
#
#
# score <- t1$bmr$score() %>% as.data.table %>% arrange(regr.mse)
# archive <- score$learner[[1]]$archive %>% as.data.table %>% arrange(regr.rmse)
# archive[1]
#
#
#
#
#
#















