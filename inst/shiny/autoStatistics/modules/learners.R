# learners ----------------------------------------------------------------
available_learners <- list(
  "regr" = c("Automatic" = "auto","KNN" = "knn", "Random Forest" = "rf", "SVM" = "svm", "XGBoost" = "xgb"),
  "classif" = c("Automatic" = "auto", "KNN" = "knn", "Random Forest" = "rf", "SVM" = "svm", "XGBoost" = "xgb")
)
cat("available_learners loaded\n")


# fs ----------------------------------------------------------------------

available_feature_filter <- list(
  "regr" = c("Automatic" = "auto", "None" = "none", "Featureless" = "regr.featureless", "Rpart" = "regr.rpart", "XGBoost" = "regr.xgboost"),
  "classif" = c("Automatic" = "auto", "None" = "none", "Featureless" = "classif.featureless", "Rpart" = "classif.rpart", "XGBoost" = "classif.xgboost")
)

available_feature_filter <- list(
  "regr" = c("None" = "no", "Information Gain" = "information_gain"),
  "classif" = c("None" = "no", "Importance" = "importance", "Information Gain" = "information_gain", "mrmr" = "mrmr")
)
cat("available_fs loaded\n")

# ensemble ----------------------------------------------------------------------
available_ensemble <- c("None" = "no", "Bagging" = "bagging", "Stacking" = "stacking", "Both" = "both")


cat("available_ensemble loaded\n")
# NA imp ------------------------------------------------------------------

available_na_imp <- c("Automatic" = "auto", "Omit" = "omit")
cat("available_na_imp loaded\n")

available_measure <- list(
  "regr" = c("Automatic" = "auto", "rmse" = "regr.rmse", "mse" = "regr.mse", "mae" = "regr.mae"),
  "classif" = c("Automatic" = "auto", "Accurity" = "classif.acc", "Balanced Accuracy" = "classif.bacc")
)

available_tuning_methods <- c("Automatic" = "auto", "grid search" = "grid_search", "random search" = "random_search")

# pre var importance ------------------------------------------------------------------------------------------------------------------

pre_feature_import_filter <- list(
  "regr" = list(
    mlr3filters::flt("information_gain"),
    #mlr3filters::flt("importance", learner = mlr3::lrn("regr.rpart")),
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


# report dicts ------------------------------------------------------------
dict_desc_report <- list(
  "NA_none" = "There is no missing data within the feature %s and the importance rank of the feature is %s.",
  "NA_imp" = "%s NA entries exist within the feature %s and the importance rank is %s.",
  "NA_nimp_few_missing" = "%s NA entries exist within the feature %s and the importance rank is %s.",
  "NA_nimp_lot_missing" = "%s NA entries exist within the feature %s and the importance rank is %s.",
  "NA_other" = "%s NA entries exist within the feature %s and the importance rank is %s.",
  "COR_MEDIUM" = "The feature %s has **medium correlations** with the following features: %s.",
  "COR_LARGE" = "The features %s has **strong correlations** with the following features: %s.",
  "NO_COR" = "The feature %s has **no medium or strong correlation** with other features."
)


