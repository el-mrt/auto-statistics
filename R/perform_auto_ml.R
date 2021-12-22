#' Function that applies Auto ML Pipeline given a list of parameter
#'
#' @param param_list param_list
#'
#' @return benchmark results and measure
#'
#' @export
#'
#' @import mlr3verse R6


perform_auto_ml <- function(param_list){
  print(param_list)

  # if "auto" is specified in param_list it is changed to NULL
  param_list <- auto_to_null(param_list)

  # load task and na_input to apply NA strategy
  na_input <- param_list$na
  task <- param_list$task

  # if na_input is "omit" the task is overwriten with the same task but no NA's
  task <- na_omit_task(task, na_input)

  # outer resamplings are specified in sub lists
  if (is.null(param_list$o.resampling)) {
    o.resampling_strat_input <- NULL
    o.resampling_params_input <- NULL
  } else {
    o.resampling_strat_input <- param_list$o.resampling[[1]]
    o.resampling_params_input <- param_list$o.resampling[[2]]
  }
  message("outer resampling extracted")

  # inner resamplings are specified in sub lists
  if (is.null(param_list$i.resampling)) {
    i.resampling_strat_input <- NULL
    i.resampling_params_input <- NULL
  } else {
    i.resampling_strat_input <- param_list$i.resampling[[1]]
    i.resampling_params_input <- param_list$i.resampling[[2]]
  }
  message("inner resampling extracted")

  # read in other objects from param list
  tuner_input <- param_list$tuning_method
  is_hpo_input <- param_list$tuning
  task_type_input <- param_list$type
  learners_input <- param_list$learners
  feature_filter_input <- param_list$feature_filter
  include_featureless_input <- param_list$incl_featureless
  hpo_bl_input <- param_list$hpo_base_learner
  incl_at_input <- param_list$incl_at
  n_best_input <- param_list$n_best
  ensemble_input <- param_list$ensemble
  terminator_input <- param_list$terminator
  measure_input <- param_list$measure
  message("other params extracted")

  # create measure and outer resampling
  measure <- autoStatistics::create_measure(task, measure_input)
  outer_resampling <- autoStatistics::create_resampling(
    task,
    strat = o.resampling_strat_input,
    params = o.resampling_params_input
  )
  message("measure created")

  # create base learners - will be transformed eather with or without HPO
  l_base <- autoStatistics::create_learners(
    task,
    vec_learners = learners_input
  )
  message("base learners created")

  if (is_hpo_input) { # with HPO
    if (is.null(i.resampling_strat_input)) { # check if an inner resampling method is specified
      inner_resampling <- rsmp("holdout")
    } else { # choose inner resampling method only if applicabe
      inner_resampling <- autoStatistics::create_resampling(
        task,
        strat = i.resampling_strat_input,
        params = i.resampling_params_input)
    }

    # create terminator and tuner for all resamplings of autotuners later on
    terminator <- autoStatistics::create_terminator(vec_terminators = terminator_input)
    tuner <- autoStatistics::create_tuner(tuner_input)

    # add search space into learners
    l_w_sp <- autoStatistics::create_search_space(task, l_base)

    # adds robustify pipeline
    gl_robust <- autoStatistics::create_robust_learners(task, l_w_sp)

    # choose if feature filtering is applied - output of if statement is always object names learners_at
    if (feature_filter_input != "no") {
      # add feature filter with tunable parameter nfeat
      gl_ff <- autoStatistics::create_feature_filter(task,
                                     task_type_input,
                                     gl_robust,
                                     feature_filter_input)

      # transform graphlearner into auto tuner
      learners_at <- autoStatistics::create_auto_tuner(gl_ff,
                                    inner_resampling,
                                    measure,
                                    terminator,
                                    tuner)
    } else {
      # transform graphlearner into auto tuner
      learners_at <- autoStatistics::create_auto_tuner(gl_robust,
                                    inner_resampling,
                                    measure,
                                    terminator,
                                    tuner)
    }
    message("test1")
    # should ensemble models be created, if so, which?
    if (!("no" %in% ensemble_input)) {

      # initialize empty list
      ensemble_learners <- NULL

      if ("stacking" %in% ensemble_input) { # create stacking ensemble

        stacking_ensemble <- autoStatistics::create_stacking_ensemble(task = task,
                                                      type = task_type_input,
                                                      learners = learners_at,
                                                      feature_filter = feature_filter_input,
                                                      inner_resampling = inner_resampling,
                                                      measure = measure,
                                                      terminator = terminator,
                                                      tuner = tuner)

        ensemble_learners <- c(ensemble_learners, list(stacking_ensemble))
      }

      if ("bagging" %in% ensemble_input) { # create bagging ensemble
        bagging_ensemble <- autoStatistics::create_bagging_ensemble(learners = learners_at,
                                                    task_type = task_type_input)

        ensemble_learners <- c(ensemble_learners, list(bagging_ensemble))
      }
    }
    message("test2")

    # should base learners be included in HPO benchmark
    if (hpo_bl_input) {
      hpo_l_base <- autoStatistics::create_learners(task, learners_input)
      hpo_gl_base <- autoStatistics::create_robust_learners(task, hpo_l_base)
    }
    message("test3")
    # determine which learners will be included; initialize emply list
    learners <- NULL

    # TODO maybe here we get a list from shiny for all possible types
    # for now all are stored individually
    # appends applicable learner into the overall list "learners"
    if (incl_at_input) {
      learners <- c(learners, learners_at)
    }
    message("test4.1")
    if (hpo_bl_input) {
      learners <- c(learners, hpo_gl_base)
    }
    message("test4.2")
    if (!("no" %in% ensemble_input)) {
      learners <- c(learners, ensemble_learners)
    }
    message("test4.3")


    # if no elements are selected - throw error
    if(is.null(learners)) stop("no learners selected")


  } else { # without HPO
    # add robustify pipeline to learner
    learners <- autoStatistics::create_robust_learners(task, l_base)
  }
  message("test5")

  if (include_featureless_input) { # appends featureless learner if desired
    featureless <- paste0(task_type_input, ".", "featureless")

    learners <- c(learners, lrn(featureless))
  }
  message("test6")

  # removes all preprocessing steps from the learner$id and just returns regr.xxx(.tuned) /classif.xxx(.tuned)
  learners <- shorten_id(learners, task_type_input)
  message("test7")

  # creates design for benchmark
  design <- benchmark_grid(task = task,
                           resamplings = outer_resampling,
                           learners = learners)
  message("test8")
  # store_models needs to be true, so that best configurations can be extracted
  bmr <- benchmark(design, store_models = TRUE)
  message("test9")

  # could include n_best as input from shiny app, for now fixed at 5
  bmr_best <- autoStatistics::create_best_benchmark(task = task,
                                    bmr = bmr,
                                    measure = measure,
                                    n_best = n_best_input)
  message("test10")
  #initialize output list
  output_list <- list(bmr = NULL, bmr_best = NULL, measure = NULL)

  output_list$measure <- measure

  output_list$bmr_best <- bmr_best

  output_list$bmr <- bmr
  message("test11")

  return(output_list)
}
