
auto_ml_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinybusy::add_busy_spinner(spin = "fading-circle"),
      column(2,
             h3("Settings"),
             actionButton(ns("start"), "Start"),
             downloadButton(ns("download_result"), "Download result"),

             uiOutput(ns("task_na")), hr(),
             uiOutput(ns("task_learner")),
             uiOutput(ns("ensemble_n_best")),
             hr(),
             uiOutput(ns("task_resampling")),
             fluidRow(column(4, conditionalPanel(condition = "input.task_resampling != 'auto'", tagList(uiOutput(ns("task_resampling_one_param"))), ns = ns), style = "margin-left:15px;"),
                      column(4, conditionalPanel(condition = "input.task_resampling == 'repeated_cv' || input.task_resampling == 'bootstrap'", tagList(uiOutput(ns("task_resampling_param_two"))), ns = ns))
             ),

             uiOutput(ns("task_measure")),
             hr(),
             uiOutput(ns("task_tuning")),
             conditionalPanel(condition = "input.task_tuning == true",
                              tagList(
                                uiOutput(ns("task_ensemble")),
                                uiOutput(ns("task_include_at")),
                                uiOutput(ns("hpo_base_learner")),
                                uiOutput(ns("task_feature")),
                                uiOutput(ns("task_featureless")),
                                uiOutput(ns("task_term_runtime")),
                                uiOutput(ns("task_term_evals")),
                                uiOutput(ns("task_tuning_method")),
                                uiOutput(ns("task_resample_inner")),
                                uiOutput(ns("task_resample_inner_param"))),
                              ns = ns
             )
      ),
      column(9,
             h3("Result"),
             fluidRow(
               column(12,
                      plotOutput(ns("bmr_result"))
               )
             ),
             fluidRow(
               column(12,
                      plotOutput(ns("bmr_result_ensemble")))
             )
      )
    )
  )
}

auto_ml_server <- function(id, user_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # NAs----
    output$task_na <- renderUI({
      selectInput(ns("task_na"), "Imputation of missings", choices = available_na_imp, selected = "auto")
    })
    observeEvent(input$task_na, {
      user_task$na <- input$task_na
      autoStatistics::debug_console(sprintf("NA imputation changed. New Value: %s", user_task$na))
    })

    # feature_filter----
    output$task_feature <- renderUI({
      if(is.null(user_task$type)){
        selectInput(ns("task_feature"), "Feature Selection", choices = c("None"), selected = "None")
      }else{
        selectInput(ns("task_feature"), "Feature Selection", choices = available_feature_filter[[user_task$type]], selected = "no", multiple = TRUE)
      }
    })
    observeEvent(input$task_feature, {
      user_task$feature_filter <- input$task_feature
      autoStatistics::debug_console(paste(c("feature_filter updated. New feature_filter are: ", user_task$feature_filter), collapse = ","))
    })

    # learner----
    output$task_learner <- renderUI({

      if(is.null(user_task$type)){
        selectInput(ns("task_learner"), "Learners",
                    choices = c("Auto"), selected = "auto")
      }else{
        selectInput(ns("task_learner"), "Learners", multiple = TRUE,
                    choices = available_learners[[user_task$type]], selected = "auto")
      }
    })

    observeEvent(input$task_learner, {
      # update learner if auto selected
      if(is.null(user_task$type)){
        updateSelectInput(session, "task_learner", "Learners", choices = c("Auto" = "auto"), selected = "auto")
      }else if((c("auto") %in% input$task_learner) && !(c("auto") %in% user_task$learners)){
        updateSelectInput(session, "task_learner", "Learners", choices = available_learners[[user_task$type]], selected = "auto")
      }else if(length(input$task_learner) > 1){
        updateSelectInput(session, "task_learner", "Learners", choices = available_learners[[user_task$type]],
                          selected = input$task_learner[!input$task_learner %in% c("auto")])
      }

      user_task$learners <- input$task_learner
      autoStatistics::debug_console(paste(c("learners updated. New Learners are: ", user_task$learners), collapse = ", "))
    })
    output$hpo_base_learner <- renderUI({
      checkboxInput(ns("hpo_base_learner"), "additional base learners", value = FALSE)
    })
    observeEvent(input$hpo_base_learner, {
      user_task$hpo_base_learner <- input$hpo_base_learner
    })
    # ensemble----
    output$task_ensemble <- renderUI({
      selectInput(ns("task_ensemble"), label = "ensemble", choices = available_ensemble, selected = "no")
    })
    observeEvent(input$task_ensemble, {
      if(input$task_ensemble == "both"){
        user_task$ensemble <- c("stacking", "bagging")
      }else{
        user_task$ensemble <- input$task_ensemble
      }
      autoStatistics::debug_console(sprintf("ensemble learner changed. New Value: %s", user_task$ensemble))
    })
    output$ensemble_n_best <- renderUI({
      numericInput("ensemble_n_best", "benchmark n best learners", 5, 1, 100, 1)
    })
    observeEvent(input$ensemble_n_best, {
      user_task$ensemble_n_best <- input$ensemble_n_best
      autoStatistics::debug_console(sprintf("n_ensemble  changed. New Value: %s", user_task$ensemble_n_best))
    })
    # featureless ----
    output$task_featureless <- renderUI({
      checkboxInput(ns("task_featureless"), "use featureless learner", value = FALSE)
    })
    observeEvent(input$task_featureless, {
      user_task$incl_featureless <- input$task_featureless
      autoStatistics::debug_console(sprintf("task_featureless learner changed. New Value: %s", user_task$incl_featureless))
    })
    # include_at -----
    output$task_include_at <- renderUI({
      checkboxInput(ns("task_include_at"), "include Autotuner", TRUE)
    })
    observeEvent(input$task_include_at, {
      user_task$include_at <- input$task_include_at
      autoStatistics::debug_console(sprintf("task_include_at changed. New Value: "), user_task$include_at)
    })


    # tuning----
    output$task_tuning <- renderUI({
      checkboxInput(ns("task_tuning"), label = HTML(paste0("<b>Perform Hyperparameter Tuning</b>")), value = FALSE)
    })
    observeEvent(input$task_tuning, {
      user_task$tuning <- input$task_tuning
      autoStatistics::debug_console(sprintf("tuning changed. New Value: %s", user_task$tuning))
      if(!user_task$tuning){
        user_task$i.resampling = NULL
        user_task$terminator = NULL
        user_task$tuning_method = NULL
      }
    })


    # measure
    # tuning method ####
    output$task_tuning_method <- renderUI({
      selectInput(ns("task_tuning_method"), "Method", available_tuning_methods)
    })
    observeEvent(input$task_tuning_method, {
      user_task$tuning_method <- input$task_tuning_method
      autoStatistics::debug_console(sprintf("task_tuning_method changed. New Value: "), user_task$tuning_method)
    })



    # resampling ####
    ## OUTER
    output$task_resampling <- renderUI({
      selectInput(ns("task_resampling"),"outer resampling", choices = c("Automatic" = "auto", "Holdout" = "holdout", "CV" = "cv", "Repeated-CV" = "repeated_cv", "Bootstrap" = "bootstrap"),selected = "auto")
    })
    output$task_resampling_one_param <- renderUI({
      fluidRow(
        numericInput(ns("task_resampling_first_param"), "first_param", 1, 1, 100, 1)
      )
    })
    output$task_resampling_param_two <- renderUI({
      fluidRow(
        numericInput(ns("task_resampling_second_param"), "second param", 1,1,10,1)
      )
    })
    # update inputs
    observeEvent(input$task_resampling, {
      if(input$task_resampling %in% c("holdout", "bootstrap")){
        updateNumericInput(session, "task_resampling_first_param", "Ratio", 0.8, 0.1, 1, 0.01)
      }
      else if(input$task_resampling %in% c("cv", "repeated_cv")){
        updateNumericInput(session, "task_resampling_first_param", "Folds", 3, 1, 100, 1)
      }
      if(input$task_resampling %in% c("boostrap", "repeated_cv")){
        updateNumericInput(session, "task_resampling_second_param", "Repeats", 2, 1, 100, 1)
      }
    })
    # update reactive values
    observeEvent(c(input$task_resampling_first_param, input$task_resampling_second_param),{
      user_task$o.resampling <- list("method" = input$task_resampling,
                                     "params" = c(input$task_resampling_first_param, input$task_resampling_second_param))
    })
    ## INNER
    output$task_resample_inner <- renderUI({
      selectInput(ns("task_resample_inner"), "inner resampling", choices = c("Holdout" = "holdout", "CV" = "cv"))
    })

    output$task_resample_inner_param <- renderUI({
      numericInput(ns("task_resample_inner_param"), "first param", 3, 1, 100, 1)
    })
    # update inputs
    observeEvent(input$task_resample_inner, {
      if(input$task_resample_inner == "holdout"){
        updateNumericInput(session, "task_resample_inner_param", "Ratio", 0.8, 0.1, 1, 0.01)
      }
      else if(input$task_resample_inner == "cv"){
        updateNumericInput(session, "task_resample_inner_param", "Folds", 3, 1, 100, 1)
      }
    })
    # update reactive Values
    observeEvent(c(input$task_resample_inner, input$task_resample_inner_param), {
      user_task$i.resampling <- list("method" = input$task_resample_inner, "params" = c(input$task_resample_inner_param))
      autoStatistics::debug_console("user_task$i.resampling updated")
      print(user_task$i.resampling)
    })


    # measure ####
    output$task_measure <- renderUI({
      if(is.null(user_task$type)){
        selectInput(ns("task_measure"), "Measure", choices = c("Automatic"), selected = "Auto")
      }else{
        selectInput(ns("task_measure"), "Measure", choices = available_measure[[user_task$task$task_type]])
      }
      observeEvent(input$task_measure, {user_task$measure <- input$task_measure})
      autoStatistics::debug_console(sprintf("user_task$measure. New Value: "), user_task$measure)
    })



    # terminator####
    output$task_term_runtime <- renderUI({
      fluidRow(column(1, checkboxInput(ns("task_term_runtime"), "", value = FALSE), style = "margin-top: 20px;"),
               column(8, numericInput(ns("task_term_runtime_param"), "Terminator - runtime", min = 1, max = 1000000, value = 120)))
    })
    output$task_term_evals <- renderUI({
      fluidRow(column(1, checkboxInput(ns("task_term_eval"), "", value = FALSE), style = "margin-top: 20px;"),
               column(8, numericInput(ns("task_term_eval_param"), "Terminator - evals", min = 1, max = 100000, value = 10)))
    })

    observeEvent(c(input$task_term_runtime, input$task_term_runtime_param, input$task_term_eval, input$task_term_eval_param), {
      list_term <- vector(mode = "list")
      if(input$task_term_runtime){
        list_term[["runtime"]] <- c("rt", input$task_term_runtime_param)
      }
      if(input$task_term_eval){
        list_term[["eval"]] <- c("eval", input$task_term_eval_param)
      }
      if(!input$task_term_runtime && !input$task_term_eval){
        list_term <- "auto"
      }
      user_task$terminator <- list_term
    })

    # start----
    observeEvent(input$start, {
      req(user_task$task)
      shinybusy::show_spinner() # show the spinner
      param_list <- list(
        "task" = user_task$task,
        "type" = user_task$type,
        "learners" = user_task$learners,
        "ensemle" = user_task$ensemble,
        "n_best" = user_task$ensemble_n_best,
        "o.resampling" = user_task$o.resampling,
        "i.resampling" = user_task$i.resampling,
        "measure" = user_task$measure,
        "feature_filter" = user_task$feature_filter,
        "na_imp" = user_task$na,
        "tuning" = user_task$tuning,
        "tuning_method" = user_task$tuning_method,
        "terminator" = user_task$terminator,
        "incl_featureless" = user_task$incl_featureless,
        "hpo_base_learner" = user_task$hpo_base_learner,
        "incl_at" = user_task$include_at
      )
      results$param_list <- param_list

      tryCatch({
        results$bmr_result <- autoStatistics::perform_auto_ml(param_list)
      }, error=function(cond){
        message(paste("ERROR BENCHMARK:", cond))
      })

      shinybusy::hide_spinner() # hide spinner
      print(results$bmr_result)
      #save("bmr_result", file = "bmr_result.Rdata")
      shinybusy::hide_spinner()
    })

    # Results ----------------------------------------------------------------
    output$bmr_result <- renderPlot({
      req(results$bmr_result)
      cur_plot <- ggplot2::autoplot(results$bmr_result$bmr, measure = results$bmr_result$measure) +
        theme_minimal() +
        theme(axis.text=element_text(size=12))
      return(cur_plot)
    })
    output$bmr_result_ensemble <- renderPlot({
      req(results$bmr_result)
      cur_plot <- ggplot2::autoplot(results$bmr_result$bmr_best, measure = results$bmr_result$measure) +
        theme_minimal() +
        theme(plot.title = element_blank(), axis.text=element_text(size=14))
      return(cur_plot)
      })

    output$download_result <- downloadHandler(
      filename = function() {
        paste0("bmr_result", ".rds")
      },
      content = function(con) {
        saveRDS(results$bmr_result, con)
      }
    )
  })
}
