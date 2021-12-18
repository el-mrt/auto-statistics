#' Function that given a param_list iterates tróugh the list and sets every entry to NULL if "auto" was specified
#'
#' @param param_list param_list
#'
#' @return param_list
#'
#' @export
#'
#' @import mlr3verse
#'

auto_to_null <- function(param_list){

  auto_to_null_entry <- function(list_entry){
    if ("R6" %in% class(list_entry)) {
      return(list_entry)
    }

    if ("auto" %in% list_entry) {
      list_entry <- NULL
    }
    return(list_entry)
  }

  param_list <- lapply(param_list, auto_to_null_entry)
}
