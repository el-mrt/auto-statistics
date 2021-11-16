cat("global called")

# load libraries ----------------------------------------------------------------------------------------------------------------------

library("shiny")
library("DT")
library("ggplot2")
library("RColorBrewer")
library("shinydashboard")


# load all files from modules folder --------------------------------------------------------------------------------------------------


#modules_path <- system.file("shiny", "autoStatistics", "modules", package = "autoStatistics")
modules_path <- "./modules"

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

fct_col_warn <- reactiveValues(
  text = "",
  col_name = "",
  col_data = NULL
)
user_task <-  reactiveValues(
  type = NULL,
  task = NULL,
  learners = NULL,
  ensemble = NULL,
  fs = NULL,
  na = NULL,
  tuning = NULL,
)




cat("global.R called\n")
