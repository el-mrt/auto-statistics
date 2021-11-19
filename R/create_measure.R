#' Function that given "classif" or "regr" returns a suitable measure
#'
#' @param task mlr3 task
#' @param measure user defined measure
#'
#' @examples
#' create_measure(tsk("iris"))
#' create_measure(tsk("german_credit"))
#' create_measure(tsk("bike_sharing"))
#'
#' @return mlr3 measure
#'
#' @export
#'
#' @import mlr3verse
#'

create_measure <- function(task, measure = NULL){

  if (is.null(measure)) {
    cr <- task$task_type

    if (cr == "regr") {
      type <- cr
    } else {
      type <- task$properties
    }

    msr <- switch (type,
                   regr = msr("regr.rmse"),
                   twoclass = msr("classif.auc"),
                   multiclass = msr("classif.bacc"),
    )
  } else {
    msr <- msr(measure)

    if (task$task_type != msr$task_type) {
      stop("measure not supported")
    }

    if (task$properties == "multiclass") {
      if (msr$task_properties == "twoclass") {
        stop("measure not supported")
      }
    }
  }

  return(msr)
}
