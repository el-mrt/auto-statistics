#' Function that applies Auto ML Pipeline given a list of parameter
#'
#' @param param_list
#'
#' @examples
#'
#' @return
#'
#' @export
#'
#' @import mlr3verse R6
#'
#'
#'

perform_auto_ml <- function(param_list){

  auto_to_null <- function(x){
    if ("R6" %in% class(x)) {
      return(x)
    }

    if ("auto" %in% x) {
      x <- NULL
    }
    return(x)
  }

  param_list <- lapply(param_list, auto_to_null)

  is_ensemble_input <- param_list$ensemble
  terminator_input <- param_list$terminator
  measure_input <- param_list$measure
  na_input <- param_list$na

  if (is.null(param_list$o.resampling)) {
    o.resampling_strat_input <- NULL
    o.resampling_params_input <- NULL
  } else {
    o.resampling_strat_input <- param_list$o.resampling[[1]]
    o.resampling_params_input <- param_list$o.resampling[[2]]
  }

  if (is.null(param_list$i.resampling)) {
    i.resampling_strat_input <- NULL
    i.resampling_params_input <- NULL
  } else {
    i.resampling_strat_input <- param_list$i.resampling[[1]]
    i.resampling_params_input <- param_list$i.resampling[[2]]
  }


  tuner_input <- param_list$tuning_method
  is_hpo_input <- param_list$tuning
  task_type_input <- param_list$type
  learners_input <- param_list$learners
  # feature_filter_input <- param_list$feature_filter
  include_featureless_input <- param_list$incl_featureless

  task <- param_list$task
  measure <- create_measure(task, measure_input)
  outer_resampling <- create_resampling(task, strat = o.resampling_strat_input, params = o.resampling_params_input)

  l_base <- create_learners(task, vec_learners = learners_input)

  if (is_hpo_input) { # with HPO
    if (is.null(i.resampling_strat_input)) { # check if an inner resampling method is specified
      inner_resampling <- rsmp("holdout")
    } else {
      inner_resampling <- create_resampling(task, strat = i.resampling_strat_input, params = i.resampling_params_input)
    }

    terminator <- create_terminator(vec_terminators = terminator_input)
    tuner <- create_tuner(tuner_input)

    l_w_sp <- create_search_space(task, l_base)
    atl <- create_auto_tuner(l_w_sp, inner_resampling, measure, terminator, tuner)

    learners <- create_graph_learners(task, atl)

  } else { # without HPO
    learners <- create_graph_learners(task, l_base)
  }

  if (include_featureless_input) {
    featureless <- paste0(task_type_input, ".", "featureless")

    learners <- c(learners, lrn(featureless))
  }

  design <- benchmark_grid(
    task = task,
    resamplings = outer_resampling,
    learners = learners
  )

  bmr <- benchmark(design)

  return(bmr)
}
