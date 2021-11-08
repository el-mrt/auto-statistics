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

modules_path <- system.file("shiny", "autoStatistics", "modules", package = "autoStatistics")
files <- list.files(modules_path, full.names = TRUE)
lapply(files, source)


# shiny options -----------------------------------------------------------------------------------------------------------------------

options(shiny.maxRequestSize = 30*1024^2) # set max size of uploaded file to 30 Mb



# reactive Values ---------------------------------------------------------

user_data <- reactiveVal(NULL, "user_data")
target_column <- reactiveVal(NULL, "targ_col")



cat("global.R called\n")
