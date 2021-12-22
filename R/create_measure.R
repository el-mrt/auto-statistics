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
#' @import mlr3
#'

create_measure <- function(task, measure = NULL){

  cr <- task$task_type

  if (is.null(measure)) {

    if (cr == "regr") {
      type <- cr
    } else {
      type <- task$properties
    }

    meas <- switch (type,
                   regr = msr("regr.rmse"),
                   twoclass = msr("classif.bacc"),
                   multiclass = msr("classif.bacc")
    )
  } else {
    meas <- msr(measure)

    if (task$task_type != meas$task_type) {
      stop("measure not supported")
    }

    if (cr == "classif") {
      if (task$properties == "multiclass") {
        if (meas$task_properties == "twoclass") {
          stop("measure not supported")
        }
      }
    }

  }

  return(meas)
}
