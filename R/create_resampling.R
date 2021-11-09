#' Function that outputs a MLR resampling object, given specification
#'
#' @param task mlr3 task
#' @param strat resampling strategy
#' @param params list of parameters
#'
#' @examples
#' task <- tsk("iris)
#'
#' holdout <- list(ration = 0.8)
#' create_resampling(task, "holdout", holdout)
#'
#' cv <- list(folds = 2)
#' create_resampling(task,"cv", cv)
#'
#' repeated_cv <- list(folds = 2, repeats = 10)
#' create_resampling(task, "repeated_cv", repeated_cv)
#'
#' @return resampling object
#'
#' @export
#'
#' @import mlr3verse

create_resampling <- function(task, strat = NULL, params = NULL){

  if (is.null(strat)) {
    if (task$nrow < 100) {
      resampling <- rsmp("loo")
    } else if (task$nrow < 500) {
      resampling <- rsmp("repeated_cv")
    } else {
      resampling <- rsmp("cv")
    }
  } else {
    resampling <- switch(strat,
                         holdout = rsmp(strat, ratio = params[[1]]),
                         cv = rsmp(strat, folds = params[[1]]),
                         repeated_cv = rsmp(strat, folds = params[[1]], repeats = params[[2]]),
                         bootstrap = rsmp(strat, ratio = params[[1]], repeats = params[[2]])
    )
  }

  return(resampling)
}
