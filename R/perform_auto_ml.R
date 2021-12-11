#' Function that applies Auto ML Pipeline given a list of parameter
#'
#' @param param_list param_list
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

  # if "auto" is specified in param_list it is changed to NULL
  param_list <- auto_to_null(param_list)

  na_input <- param_list$na
  task <- param_list$task

  # if na_input is "omit" the task is overwriten with the same task but no NA's
  task <- na_omit_task(task, na_input)

  is_ensemble_input <- param_list$ensemble
  terminator_input <- param_list$terminator
  measure_input <- param_list$measure

  # inner and outer resamplings are specified in sub lists
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
  feature_filter_input <- param_list$feature_filter
  include_featureless_input <- param_list$incl_featureless
  hpo_bl_input <- param_list$hpo_base_learner

  measure <- create_measure(task, measure_input)
  outer_resampling <- create_resampling(
    task,
    strat = o.resampling_strat_input,
    params = o.resampling_params_input
  )

  l_base <- create_learners(
    task,
    vec_learners = learners_input
  )

  if (is_hpo_input) { # with HPO
    if (is.null(i.resampling_strat_input)) { # check if an inner resampling method is specified
      inner_resampling <- rsmp("holdout")
    } else {
      inner_resampling <- create_resampling(
        task,
        strat = i.resampling_strat_input,
        params = i.resampling_params_input)
    }

    terminator <- create_terminator(vec_terminators = terminator_input)
    tuner <- create_tuner(tuner_input)

    l_w_sp <- create_search_space(task, l_base)

    gl_robust <- create_robust_learners(task, l_w_sp)

    if (feature_filter_input != "no") { # choose if feature filtering is applied
      gl_ff <- create_feature_filter(task,
                                     task_type_input,
                                     gl_robust,
                                     feature_filter_input)

      learners <- create_auto_tuner(gl_ff,
                                    inner_resampling,
                                    measure,
                                    terminator,
                                    tuner)
    } else {
      learners <- create_auto_tuner(gl_robust,
                                    inner_resampling,
                                    measure,
                                    terminator,
                                    tuner)
    }

    # should base learners be included in HPO benchmark
    if (hpo_bl_input) {
      hpo_l_base <- create_learners(task, learners_input)
      hpo_gl_base <- create_graph_learners(task, hpo_l_base)
      learners <- c(learners, hpo_gl_base)
    }
  } else { # without HPO
    learners <- create_graph_learners(task, l_base)
  }

  if (include_featureless_input) {
    featureless <- paste0(task_type_input, ".", "featureless")

    learners <- c(learners, lrn(featureless))
  }

  learners <- shorten_id(learners, task_type_input)

  design <- benchmark_grid(
    task = task,
    resamplings = outer_resampling,
    learners = learners
  )

  bmr <- benchmark(design, store_models = TRUE)

  #initialize output list
  output_list <- list(bmr = NULL, bmr_best = NULL, measure = NULL)

  output_list$measure <- measure

  output_list$bmr <- bmr

  return(output_list)
}
