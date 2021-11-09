#' Function to print some information in the console.
#'
#' @param message message to show in the console
#' @param lvl level
#'
#' @export
#'

debug_console <- function(message, lvl = "INFO"){
  cur_time <- format(Sys.time(), "%y/%m/%d %H:%M:%S")
  sprintf("%s [%s]: %s", cur_time, lvl, message)
}
