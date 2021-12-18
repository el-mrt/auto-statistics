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
# param_list$terminator <- list(c("eval", 5))
# param_list$o.resampling$params <- 3
# param_list$o.resampling$method <- "cv"
# param_list$i.resampling$params <- 3
# param_list$i.resampling$method <- "cv"
# param_list$hpo_base_learner <- TRUE
# param_list$feature_filter <- "information_gain"
# param_list$incl_featureless <- TRUE
# param_list$tuning_method <- "random_search"
# param_list$ensemble <- c("stacking", "bagging")
#
# t1 <- perform_auto_ml(param_list)
#
# # save(t1, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Applications in Data Analytics/RData_files/pam_002.RData")
#
# t1$bmr %>% autoplot(measure = t1$measure)
#
#
# # # save(t1, file = "C:/Users/lukas/Desktop/Master/2021-2022 WiSe/Applications in Data Analytics/RData_files/pam_001.RData")
