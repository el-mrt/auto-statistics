#' Function that outputs a MLR resampling object, igven specification
#'
#' @param strat resampling strategy
#' @param params list of parameters
#'
#' @examples
#' holdout <- list(ration = 0.8)
#' create_resampling("holdout", holdout)
#'
#' cv <- list(folds = 2)
#' create_resampling("cv", cv)
#'
#' repeated_cv <- list(folds = 2, repeats = 10)
#' create_resampling("repeated_cv", repeated_cv)
#'
#' @return resampling object
#'
#' @export
#'
#' @import mlr3

create_resampling <- function(strat, params){

  resampling <- switch(strat,
         holdout = mlr3::rsmp(strat, ratio = params[[1]]),
         cv = mlr3::rsmp(strat, folds = params[[1]]),
         repeated_cv = mlr3::rsmp(strat, folds = params[[1]], repeats = params[[2]]),
         bootstrap = mlr3::rsmp(strat, ratio = params[[1]], repeats = params[[2]])
         )

  return(resampling)
}
