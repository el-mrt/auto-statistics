#' Function insert a line break into a string after n chars
#'
#' @param x string
#' @param n insert line break after n chars
#'
#' @return new string with line breaks #377EB8, #E41A1C
#'
#' @examples
#' insert_line_break(x = "very long string", n = 10)
#'
#' @importFrom stringr str_replace_all
#'
#' @export


insert_line_break <- function(x, n = 30){
  new_string <- stringr::str_replace_all(x, paste0("(.{",n,"})"), "\\1\n")
  return(new_string)
}


#' Function to run the provided shiny App in this package
#'
#'
#' @importFrom shiny runApp
#' @export


run_shiny <- function(){
  shiny_path <- system.file("shiny", "autoStatistics", package = "autoStatistics")
  if(shiny_path == "")
    stop("couldnt find the path to the shiny application. Try reinstalling `autoStatistics`", .call = FALSE)

  shiny::shinyAppDir(shiny_path)
}

#' Function to print some logs into the console
#' @export
#' @param message message print to console
#' @param lvl level for the message
debug_console <- function(message, lvl = "INFO"){
  cur_time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  cat(sprintf("%s [%s]: %s \n", cur_time, lvl, message))

}

#' Function to generate an error string
#'
#' @param error_list list with the errors
#' @param err_name name of the error within the list
#' @param cond passed Condition
#'
#' @return string
#'
#' @import shiny
#' @export


render_error <- function(err_name, cond = NULL, error_list = list_error_mess){
  if(is.null(cond)){cond <- ""}

  error_string <- paste(error_list[[err_name]])
  output_html <- shiny::HTML(paste0(
    '<p style="color:red; background:#f0f0f0; border-radius: 10px;padding: 10px;">',
    error_string, '<br>&emsp;', cond, '</p>'))

  return(shiny::renderUI({output_html}))
}


#' Function for updating column
#' @param data your data
#' @param old_cols vector of names of old factor columns
#' @param new_cols vector of names of new factor columns
#'
#' @return dataframe
#' @export

update_factor_cols <- function(data, old_cols, new_cols){
  col_diff <- c(setdiff(old_cols, new_cols), setdiff(new_cols, old_cols)) # check difference

  for(col in col_diff){
    if(col %in% new_cols){
      # if col is in new cols
      tryCatch(
        {data[[{{ col }}]] <- as.factor(data[[{{ col }}]])},
        error = function(cond) print(cond),
        warning = function(cond) print(cond)
      )
    }else{
      # if col is just in old cols
      tryCatch(
        {data[[{{ col }}]] <- as.numeric(levels(data[[{{ col }}]]))[data[[{{ col }}]]]},
        error = function(cond) print(cond),
        warning = function(cond) print(cond)
      )
    }
  }
  cols_is_factor <- sapply(data, is.factor) # check which are factors
  new_factors <- names(cols_is_factor[cols_is_factor]) # get names of new factors
  return(list(
    data = data,
    new_factors_names = new_factors
  ))
}


