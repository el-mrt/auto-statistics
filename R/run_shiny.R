#' Function to run the provided shiny App in this package
#'
#'
#' @importFrom shiny runApp
#' @export


run_shiny <- function(){
  shiny_path <- system.file("shiny", "autoStatistics", package = "autoStatistics")
  if(shiny_path == "")
    stop("couldnt find the path to the shiny application. Try reinstalling `autoStatistics`", .call = FALSE)

  shiny::shinyAppDir(shiny_path, display.mode = "normal")
}
