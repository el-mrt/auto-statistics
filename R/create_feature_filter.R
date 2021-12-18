#' Function that adds a feature filter to the learner
#'
#' @param task task
#' @param type type
#' @param learners list of learners
#' @param filter filter
#'
#' @return an mlr3 graph learner
#'
#' @export
#'
#' @import mlr3verse
#'

create_feature_filter <- function(task, type, learners, filter){

  ff <- flt(filter)

  ff_po <- po("filter",
              ff,
              filter.nfeat = to_tune(1,
                                     task$ncol,
                                     logscale = TRUE))

  add_ff <- function(learner, filter){
    graph <- filter %>>% learner

    gl <- GraphLearner$new(graph)

    return(gl)
  }

  lrns_ff <- lapply(learners, add_ff, filter = ff_po)

  return(lrns_ff)
}

