cat("global called")

# load libraries ----------------------------------------------------------------------------------------------------------------------

library("shiny")
library("DT")
library("ggplot2")
library("RColorBrewer")
library("shinydashboard")
library("dplyr")


# load all files from modules folder --------------------------------------------------------------------------------------------------



modules_path <- system.file("shiny", "autoStatistics", "modules", package = "autoStatistics")

#modules_path <- "./modules"

files <- list.files(modules_path, full.names = TRUE)
lapply(files, source)


# shiny options -----------------------------------------------------------------------------------------------------------------------

options(shiny.maxRequestSize = 30*1024^2) # set max size of uploaded file to 30 Mb
options(htmlwidgets.TOJSON_ARGS = list(na = 'string')) # show NAs in


# reactive Values ---------------------------------------------------------
user_file <- reactiveVal(NULL, "user_file")
user_data <- reactiveVal(NULL, "user_data")
target_column <- reactiveVal(NULL, "targ_col")
factor_columns <- reactiveVal(NULL, "factor_cols")
task_type <- reactiveVal(NULL, "task_type")
user_task_old <- reactiveVal(NULL, "user_task")
fct_col_warn_text <- reactiveVal(NULL)
missing_comb <- reactiveValues(
  combinations = NULL,
  use = NULL,
  names = NULL,
  text = NULL
)


# warning transform numeric to factor
UserWarning <- R6::R6Class(
  classname = "UserWarning",
  public = list(
    id = NULL,
    is_active = FALSE,
    text = "",
    additional_params = vector(mode = "list"),
    initialize = function(id, is_active = FALSE, text = "", additional_params = vector(mode = "list")){
    }
    )
)

fct_col_warn <- reactiveValues(
  is_active = FALSE,
  text = "",
  col_name = "",
  col_data = NULL
)
# task
user_task <-  reactiveValues(
  type = NULL,
  task = NULL,
  learners = NULL,
  base_learners = NULL,
  i.resampling = NULL,
  o.resamping = NULL,
  measure = NULL,
  ensemble = NULL,
  ensemble_n_best = 5,
  feature_filter = NULL,
  na = NULL,
  tuning = NULL,
  tuning_method = NULL,
  terminator = NULL,
  incl_featureless = FALSE,
  hpo_base_learner = FALSE,
  include_at = TRUE
)
# plots shown to the user
user_plot <- reactiveValues(
  descr_hist = NULL,
  descr_scatter = NULL,
  na_per_col = NULL,
  na_comb = NULL,
  na_dist = NULL,
  cor_plot = NULL
)
user_tables <- reactiveValues(
  feature_imp = NULL,
  stat_summary = NULL
)


app_settings <- reactiveValues(
  plot_color_set = "Set2",
  plot_color_miss_custom = c("#377EB8", "#BD3631"),
  plot_download_dpi = 300,
  plot_download_width = 1920,
  plot_download_height = 1080,
  plot_download_format = "pdf",
  plot_download_text_size = 4,
  plot_download_text_font = "serif",
  report_thresh_na = 0.03,
  report_thres_import = 0.5
)
# results of tuning
results <- reactiveValues(
  param_list = NULL,
  bmr_result = NULL
)

# report stuff ---------------------------------------------------------------

report_plots <- reactiveValues(
  custom_report = list("plot" = vector(mode = "list", length = 0L),
                       "plot_name" = vector(mode = "list", length = 0L))
)
report_tables <- reactiveValues(
  custom_report = vector(mode = "list", length = 0L),
  descriptive = vector(mode = "list", length = 0L),
  ml = vector(mode = "list", length = 0L)
)
report_text <- reactiveValues(
  custom_report = vector(mode = "list", length = 0L),
  descriptive = vector(mode = "list", length = 0L),
  ml = vector(mode = "list", length = 0L)
)

report_settings <- reactiveValues(
  type = NULL,
  append_custom = FALSE
)
cur_report <- reactiveValues(
  type = NULL,
  path = NULL
)
custom_report_content <- reactiveValues(
  content = NULL
)



# log level ---------------------------------------------------------------

requireNamespace("lgr")

logger = lgr::get_logger("mlr3")
logger$set_threshold("info")

cat("global.R called\n")
