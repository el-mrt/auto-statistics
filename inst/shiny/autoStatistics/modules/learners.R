# learners ----------------------------------------------------------------
available_learners <- list(
  "regr" = c("Auto" = "auto","KNN" = "regr.kknn", "Random Forest" = "regr.ranger", "SVM" = "regr.svm", "XGBoost" = "regr.xgboost"),
  "classif" = c("Auto" = "auto", "KNN" = "classif.kknn", "Random Forest" = "classif.ranger", "SVM" = "classif.svm", "XGBoost" = "classif.xgboost")
)
cat("available_learners loaded\n")


# fs ----------------------------------------------------------------------

available_fs <- list(
  "regr" = c("Auto" = "auto", "None" = "none", "Featureless" = "regr.featureless", "Rpart" = "regr.rpart", "XGBoost" = "regr.xgboost"),
  "classif" = c("Auto" = "auto", "None" = "none", "Featureless" = "classif.featureless", "Rpart" = "classif.rpart", "XGBoost" = "classif.xgboost")
)
cat("available_fs loaded\n")

# NA imp ------------------------------------------------------------------

available_na_imp <- c("Auto" = "auto", "Omit" = "omit", "Mean" = "mean", "Mode" = "mode", "Histogram" = "hist")
cat("available_na_imp loaded\n")
