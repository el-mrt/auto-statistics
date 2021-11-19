#' Function that given a task returns a suitable list of learners
#'
#' @param vec_terminators list of termination criteria
#'
#' @examples
#' create_terminator()
#'
#' vec_terminators <- list(c("eval", 10), c("rt", 1000))
#' create_terminator(vec_terminators = vec_terminators)
#'
#' @return list of termination
#'
#' @export
#'
#' @import mlr3verse
#'

create_terminator <- function(vec_terminators = NULL){
  if (is.null(vec_terminators)) {
    terminator <- trm("combo",
      list(
        trm("evals", n_evals = 1000),
        trm("run_time", secs = 1800)
      ),
      any = TRUE
    )
  } else {

    lst <- list()

    for (t in vec_terminators) {
      term <- switch (t[1],
        rt = trm("run_time", secs = as.numeric(t[2])),
        eval = trm("evals", n_evals = as.numeric(t[2]))
      )

      lst <- c(lst, term)
    }

    terminator <- trm("combo", lst, any = TRUE)
  }

  return(terminator)
}
