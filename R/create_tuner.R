#' Function that given a tuner returns an mlr tuner object
#'
#' @param tuning_method tuner
#'
#' @examples
#' create_tuner()
#' create_tuner(tuning_method = "cmaes")
#'
#' @return tuning method
#'
#' @export
#'
#' @import mlr3verse adagio
#'

create_tuner <- function(tuning_method = NULL){
  if (is.null(tuning_method)) {
    tuner <- tnr("random_search")
  } else {
    tuner <- switch (tuning_method,
      cmaes = tnr("cmaes"),
      grid_search = tnr("grid_search"),
      random_search = tnr("random_search")
    )
  }

  return(tuner)
}
