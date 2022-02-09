#' R6 Class for missing combinations
#' The class has the field for the combinations and custom plot function
#' @importFrom R6 R6Class
#' @importFrom dplyr top_n
#' @importFrom stringr str_replace_all
#' @import ggplot2
#' @export
ReportPlot <- R6::R6Class("ReportPlot", list(
  #' @field title dataframe with missing combinations
  #' @field descr_key dataframe with missing combinations
  #' @field plot dataframe with missing combinations
  title = NULL,
  descr_key = NULL,
  plot = NULL,
  #' @description
  #' Creates a new ReportPlot Object
  #' @param title title of the plot
  #' @param descr_key description of the plot
  #' @param plot plot to display
  #' @param descr_dict dict with description strings
  initialize = function(title = NULL, descr_key = NULL, descr_dict = NULL, plot = NULL){
    self$title <- title
    self$descr_key <- descr_key
    self$descr_dict <- descr_dict
    self$plot <- plot
  },
  #' @description
  #' generates the description for the plot
  #' @param descr_key key of the description in the dict
  #' @param descr_dict dict of description
  get_description = function(){
  }
))


#' Function for creating na text part of descriptive report
#'
#' @param feature name of the feature
#' @param imp_tbl table of the calculated importance
#' @param task_obj current task object
#' @param na_threshold to control the threshold for when a feature has many NAs. If NULL it uses app settings.
#' @param imp_threshold to control the threshold for when a feature is important. If NULL it uses app settings.
#'
#' @return returns string
#' @export
generate_descr_report_text_na <- function(feature, imp_tbl, task_obj, na_threshold = NULL, imp_threshold = NULL){
  if(!feature %in% imp_tbl[["feature"]])
    return("")

  tryCatch({
    if(is.null(na_threshold)){na_threshold <- app_settings$report_thresh_na}
    if(is.null(imp_threshold)){imp_threshold <- app_settings$report_thres_import}

    imp_tbl_feature <- imp_tbl[imp_tbl[["feature"]] == feature, ]
    n_na <- imp_tbl_feature[["NAs"]]
    imp_rank <- imp_tbl_feature[["mean"]]

    na_text <-
    na_prob <- n_na/task_obj[["nrow"]]
    imp_perc <- imp_rank/nrow(imp_tbl)
  },
  error=function(cond){
    print(paste0("ERROR WHILE SETUP VARS FOR NA TEXT GENERATION: ", cond))
  })


  # case 1: no NAs
  if(n_na < 1){
    na_text <- autoStatistics::generate_text(dict_desc_report[["NA_none"]], list(feature, imp_rank))
  }
  # case 2: important & many NAs
  else if((imp_perc <= imp_threshold)){
    na_text <- autoStatistics::generate_text(dict_desc_report[["NA_imp"]], list(n_na, feature, imp_rank))
  }
  # case 3: not important & many NAs
  else if((imp_perc > imp_threshold) && (na_prob > na_threshold)){
    na_text <- autoStatistics::generate_text(dict_desc_report[["NA_nimp_lot_missing"]], list(n_na, feature, imp_rank))
  }
  # case 4: not important & few NAs
  else if((imp_perc > imp_threshold) && (na_prob <= na_threshold)){
    na_text <- autoStatistics::generate_text(dict_desc_report_na_text[["NA_nimp_few_missing"]], list(n_na, feature, imp_rank))
  }
  # case 5: else
  else{
    na_text <- autoStatistics::generate_text(dict_desc_report_na_text[["NA_other"]], list(n_na, feature, imp_rank))
  }
  return(na_text)
}

#' Function for creating correlation text part of descriptive report
#' @param cor_matrix correlation matrix
#' @param feature feature name
#'
#' @return returns string
#' @export
generate_descr_report_cor <- function(cor_matrix, feature, cor_thresholds = c(0.3,0.5)){
  list_return <- vector(mode = "list", length = 0L)

  diag(cor_matrix) <- 0  # exlude correlation of 1 with itself
  cor_matrix <- as.data.frame(cor_matrix)
  #View(as.data.frame(cor_matrix))
  # if feature is not numeric

  if(!feature %in% names(cor_matrix)){
    warning("feature is not numeric")
    list_return <- autoStatistics::appendList(list_return, " ", "cor_text")
    return(list_return)
  }
  # get cor values of feature
  cor_feature <- cor_matrix[feature]
  # check for medium correlation
  medium_cor_index <- which((cor_feature >= cor_thresholds[1]) & (cor_feature < cor_thresholds[2]))
  medium_cor_names <- paste(names(cor_matrix)[medium_cor_index], collapse = ", ")

  # check for strong correlation
  strong_cor_index <- which((cor_feature >= cor_thresholds[2]))
  strong_cor_names <- paste(names(cor_matrix)[strong_cor_index], collapse = ", ")

  # generate text
  final_string <- "ERROR"

  if((length(medium_cor_index) < 1) & (length(strong_cor_index) < 1)){
    print("CASE 1")
    final_string <- autoStatistics::generate_text(dict_desc_report[["NO_COR"]], list(feature))
  }else if((length(medium_cor_index) > 0) & (length(strong_cor_index) > 0)){
    print("CASE 2")
    final_string <- paste(autoStatistics::generate_text(dict_desc_report[["COR_MEDIUM"]], list(feature, medium_cor_names)),
                          autoStatistics::generate_text(dict_desc_report[["COR_LARGE"]], list(feature, strong_cor_names)), sep = "\n")
  }
  else if(length(strong_cor_index) < 1){
    print("CASE 3")
    final_string <- autoStatistics::generate_text(dict_desc_report[["COR_MEDIUM"]], list(feature, medium_cor_names))
  }else if(length(medium_cor_index) < 1){
    print("CASE 4")
    print(strong_cor_names)
    final_string <- autoStatistics::generate_text(dict_desc_report[["COR_LARGE"]], list(feature, strong_cor_names))
  }else{
    warning("ERROR")
  }

  return(final_string)
}

#' Function for creating statistical text part of descriptive report
#'
#' @param data your data
#' @param feature feeature name
#' @examples
#' generate_descr_report_tbl_stat(data = iris, feature = "Sepal.Length")
#' @import xtable
#' @return data.frame
#' @export
generate_descr_report_tbl_stat <- function(data, feature){
  stat_tbl <- summary(data[[{{ feature }}]])
  stat_df <- data.frame(matrix(nrow = 1, ncol = length(stat_tbl)))
  names(stat_df) <- names(stat_tbl)
  stat_df[1, ] <- unname(stat_tbl)
  return(stat_df)
}



#' Function to generate the text with sprintf
#' @param base_text text from the corresponding dict
#' @param text_args list of args to replace the placeholders
#' @return string
#' @export
generate_text <- function(base_text, text_args){
  return(do.call(sprintf, c(base_text, text_args)))
}


#' R6 Class for missing combinations
#' The class has the field for the combinations and custom plot function
#' @importFrom R6 R6Class
#' @importFrom dplyr top_n
#' @importFrom stringr str_replace_all
#' @import ggplot2
#' @importFrom knitr kable
#' @export
ReportContent <- R6::R6Class("ReportContent", list(
  #' @field id id of object
  #' @field type of content currently: text, ggplot, cor_matrix
  #' @field content content
  id = NULL,
  type = NULL,
  content = NULL,
  #' @description
  #' Creates a new ReportContent Object
  #' @param id id of object
  #' @param type type of content. currently: text, ggplot, cor_matrix, dataframe
  #' @param content content
  initialize = function(id = NULL, type = NULL, content = NULL){
    self$id <- id
    self$type <- type
    self$content <- content
  },
  #' @description
  #' prints the object for the different types
  print_report = function(){
    if(self$type == "text"){
      if(!is.list(self$content)){
        return(cat(self$content))
      }else{
        return(cat(""))
      }
      }
    else if(self$type == "ggplot"){
      return(print(self$content))

    }
    else if(self$type == "cor_matrix"){
      return(corrplot::corrplot(self$content))
    }
    else if(self$type == "dataframe"){
      return(print(knitr::kable(self$content, "pipe", align = "l")))
    }
  }
))


