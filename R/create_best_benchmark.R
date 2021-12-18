#' Function that given a benchmark result, measure, task_type, task number of learners and resampling strategy outputs a new benchmark, containing the best learners, as well an ensemble of those
#'
#' @param task task
#' @param bmr benchmark results
#' @param measure measure
#' @param task_type task type
#' @param n_best number of choosen learners
#' @param resampling resampling strategy - needs to be mlr3 object for now
#'
#' @return returns a benchmarking result
#'
#' @export
#'
#' @import mlr3verse
#'

create_best_benchmark <- function(task, bmr, measure, n_best, ensemble, resampling = NULL){
  task_type <- task$task_type

  if (is.null(resampling)) {
    resampling <- rsmp("cv")
  }

  bmr_scores <- arrange(as.data.table(bmr$score(measure)), !!sym(measure$id))

  n_best <- min(c(n_best, nrow(bmr_scores))) # could think about taking only learners within a certain percentile

  lrns <- vector(mode = "list", length = n_best) # allocate memory for list of length n_best

  # save top n learners from first benchmarking
  for (i in 1:n_best) {
    lrns[[i]] <- bmr_scores$learner[[i]]$base_learner(recursive = 0)
  }

  # change to unique id`s
  for (i in 1:length(lrns)) {
    lrns[[i]]$id <- paste0("l", i)
  }

  if (ensemble) {
    # detect keyword for pipeoperator
    average <- switch (task_type,
                       regr = "regravg",
                       classif = "classifavg")

    gl_ensemble <- GraphLearner$new(
      gunion(lrns) %>>% po(average, innum = n_best)
    )
    gl_ensemble$id <- "ensemble"

    lrns <- c(lrns, gl_ensemble)
  }

  design <- benchmark_grid(task, lrns, resampling)

  bmr_ensemble <- benchmark(design, store_models = TRUE)

  return(bmr_ensemble)
}
