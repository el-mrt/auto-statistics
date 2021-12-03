# learners ----------------------------------------------------------------
available_learners <- list(
  "regr" = c("Auto" = "auto","KNN" = "kknn", "Random Forest" = "ranger", "SVM" = "svm", "XGBoost" = "xgboost"),
  "classif" = c("Auto" = "auto", "KNN" = "kknn", "Random Forest" = "ranger", "SVM" = "svm", "XGBoost" = "xgboost")
)
cat("available_learners loaded\n")


# fs ----------------------------------------------------------------------

available_feature_filter <- list(
  "regr" = c("Auto" = "auto", "None" = "none", "Featureless" = "regr.featureless", "Rpart" = "regr.rpart", "XGBoost" = "regr.xgboost"),
  "classif" = c("Auto" = "auto", "None" = "none", "Featureless" = "classif.featureless", "Rpart" = "classif.rpart", "XGBoost" = "classif.xgboost")
)
cat("available_fs loaded\n")

# NA imp ------------------------------------------------------------------

available_na_imp <- c("Auto" = "auto", "Omit" = "omit")
cat("available_na_imp loaded\n")

available_measure <- list(
  "regr" = c("Auto" = "auto", "rmse" = "regr.rmse", "mse" = "regr.mse", "mae" = "regr.mae"),
  "classif" = c("Auto" = "auto", "Accurity" = "classif.acc", "Balanced Accuracy" = "classif.bacc")
)

available_tuning_methods <- c("Auto" = "auto", "grid search" = "grid_search", "cmaes" = "cmaes", "random search" = "random_search")

# pre var importance ------------------------------------------------------------------------------------------------------------------

pre_feature_import_filter <- list(
  "regr" = list(
    mlr3filters::flt("information_gain"),
    mlr3filters::flt("importance", learner = mlr3::lrn("regr.rpart")),
    #mlr3filters::flt("relief"),
    mlr3filters::flt("performance", learner = mlr3::lrn("regr.rpart"))
  ),
  "classif" = list(
    mlr3filters::flt("information_gain"),
    mlr3filters::flt("importance", learner = mlr3::lrn("classif.rpart")),
    #mlr3filters::flt("relief"),
    mlr3filters::flt("performance", learner = mlr3::lrn("classif.rpart"))
  )
)


# app settings ------------------------------------------------------------

available_fonts <- c("serif", "sans","mono", "Times")
