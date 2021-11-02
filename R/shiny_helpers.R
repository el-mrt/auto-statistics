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
