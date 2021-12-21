#' #' R6 Class for missing combinations
#' #' The class has the field for the combinations and custom plot function
#' #' @importFrom R6 R6Class
#' #' @importFrom dplyr top_n
#' #' @importFrom stringr str_replace_all
#' #' @import ggplot2
#' #'
#' ReportPlot <- R6::R6Class("ReportPlot", list(
#'   #' @field title dataframe with missing combinations
#'   #' @field descr_key dataframe with missing combinations
#'   #' @field plot dataframe with missing combinations
#'   title = NULL,
#'   descr_key = NULL,
#'   plot = NULL,
#'   #' @description
#'   #' Creates a new ReportPlot Object
#'   #' @param title title of the plot
#'   #' @param descr_key description of the plot
#'   #' @param plot plot to display
#'   initialize = function(title = NULL, descr_key = NULL, descr_dict = dict_rep_desc, plot = NULL){
#'     self$title <- title
#'     self$descr_key <- descr_key
#'     self$descr_dict <- descr_dict
#'     self$plot <- plot
#'   },
#'   #' @description
#'   #' generates the description for the plot
#'   #' @param descr_key key of the description in the dict
#'   #' @param descr_dict dict of description
#'   get_description(){
#'
#'   }
#' ))
