#' Function that given a task and an na option "omit" overwrites that task with a new task of which the backends na's are omitted
#'
#' @param task task
#' @param na_option na_option
#'
#' @examples
#'
#' @return task
#'
#' @export
#'
#' @import mlr3verse
#' @importFrom stats na.omit
#'

na_omit_task <- function(task, na_option){
  if (!is.null(na_option)) {
    if (na_option == "omit") {
      df <- na.omit(as.data.table(task))
      target <- task$target_names

      task <- create_task(df, target)
    }
  }

  return(task)
}
