library("shiny")
library("DT")
library("ggplot2")
library("RColorBrewer")


# load all files from modules folder

modules_path <- system.file("shiny", "autoStatistics", "modules", package = "autoStatistics")
files <- list.files(modules_path, full.names = TRUE)
lapply(files, source)
