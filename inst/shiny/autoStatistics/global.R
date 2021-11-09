cat("global called")

# DEV loading files -------------------------------------------------------------------------------------------------------------------

#modules_path <- "./modules"
#files <- list.files(modules_path, pattern = ".R", full.names = TRUE, ignore.case = TRUE)
#lapply(files, source)



# load libraries ----------------------------------------------------------------------------------------------------------------------

library("shiny")
library("DT")
library("ggplot2")
library("RColorBrewer")
library("shinydashboard")


# load all files from modules folder --------------------------------------------------------------------------------------------------

<<<<<<< HEAD
modules_path <- system.file("shiny", "autoStatistics", "modules", package = "autoStatistics")
#modules_path <- "./modules"
=======
#modules_path <- system.file("shiny", "autoStatistics", "modules", package = "autoStatistics")
modules_path <- "./modules"
>>>>>>> 96e240bf3a7575cbce6499630959d316d7731f5c
files <- list.files(modules_path, full.names = TRUE)
lapply(files, source)


# shiny options -----------------------------------------------------------------------------------------------------------------------

options(shiny.maxRequestSize = 30*1024^2) # set max size of uploaded file to 30 Mb



# reactive Values ---------------------------------------------------------

user_data <- reactiveVal(NULL, "user_data")
target_column <- reactiveVal(NULL, "targ_col")
factor_columns <- reactiveVal(NULL, "reactive_cols")



cat("global.R called\n")
