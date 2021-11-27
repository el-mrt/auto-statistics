
auto_ml_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2,
             h3("Settings"),
             actionButton(ns("start"), "Start"),
             uiOutput(ns("task_na")), hr(),
             uiOutput(ns("task_learner")),
             uiOutput(ns("task_ensemble")), hr(),
             uiOutput(ns("task_resampling")),
             fluidRow(column(4, conditionalPanel(condition = "input.task_resampling != 'auto'", tagList(uiOutput(ns("task_resampling_one_param"))), ns = ns), style = "margin-left:15px;"),
                      column(4, conditionalPanel(condition = "input.task_resampling == 'repeated_cv' || input.task_resampling == 'bootstrap'", tagList(uiOutput(ns("task_resampling_param_two"))), ns = ns))
                      ),

             uiOutput(ns("task_measure")), hr(),
             uiOutput(ns("task_feature")),
             uiOutput(ns("task_tuning")),
             conditionalPanel(condition = "input.task_tuning == true",
               tagList(
                 uiOutput(ns("task_term_runtime")),
                 uiOutput(ns("task_term_evals")),
                 uiOutput(ns("task_tuning_method"))),
               ns = ns
             )
      ),
      column(9,
             h3("Result")
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

    # FS----
    output$task_feature <- renderUI({
      if(is.null(user_task$type)){
        selectInput(ns("task_feature"), "Select Feature Selection", choices = c(""), selected = "")
      }else{
        selectInput(ns("task_feature"), "Select Feature Selection", choices = available_fs[[user_task$type]], selected = "auto", multiple = TRUE)
      }
    })
    observeEvent(input$task_feature, {
      user_task$fs <- input$task_feature
      autoStatistics::debug_console(paste(c("FS updated. New FS are: ", user_task$fs), collapse = ","))
    })

    # learner----
    output$task_learner <- renderUI({
      print(paste0("=======:", user_task$type))
      if(is.null(user_task$type)){
        selectInput(ns("task_learner"), "Learners",
                    choices = c(""), selected = "")
      }else{
        selectInput(ns("task_learner"), "Learners", multiple = TRUE,
                    choices = available_learners[[user_task$type]], selected = "auto")
      }
    })
    observeEvent(input$task_learner, {
      user_task$learners <- input$task_learner
      autoStatistics::debug_console(paste(c("learners updated. New Learners are: ", user_task$learners), collapse = ", "))
    })
    # ensemble----
    output$task_ensemble <- renderUI({
      checkboxInput(ns("task_ensemble"), label = "use ensemble learner", value = FALSE)
    })
    observeEvent(input$task_ensemble, {
      user_task$ensemble <- input$task_ensemble
      autoStatistics::debug_console(sprintf("ensemble learner changed. New Value: %s", user_task$ensemble))
    })

    # tuning----
    output$task_tuning <- renderUI({
      checkboxInput(ns("task_tuning"), label = HTML(paste0("<b>Perform Hyperparameter Tuning</b>")), value = TRUE)
    })
    observeEvent(input$task_tuning, {
      user_task$tuning <- input$task_tuning
      autoStatistics::debug_console(sprintf("tuning changed. New Value: %s", user_task$tuning))
    })


    # measure
    # tuning method ####
    output$task_tuning_method <- renderUI({
      selectInput(ns("task_tuning_method"), "Method", available_tuning_methods)
    })
    observeEvent(input$task_tuning_method, {
      user_task$tuning_method <- input$task_tuning_method
    })
    # resampling ####
    output$task_resampling <- renderUI({
      selectInput(ns("task_resampling"),"Resampling", choices = c("Auto" = "auto", "Holdout" = "holdout", "CV" = "cv", "Repeated-CV" = "repeated_cv", "Bootstrap" = "bootstrap"),selected = "auto")
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
     user_task$resampling <- list("method" = input$task_resampling,
                                  "params" = c(input$task_resampling_first_param, input$task_resampling_second_param))
   })

    # measure ####
   output$task_learner <- renderUI({
     if(is.null(user_task$type)){
       selectInput(ns("task_measure"), "Measure", choices = c(""), selected = "")
     }else{
       selectInput(ns("task_measure"), "Measure", choices = available_measure[[user_task$task$task_type]])
     }
   })

   observeEvent(input$task_measure, {user_task$measure <- input$task_measure})

    # terminator####
   output$task_term_runtime <- renderUI({
     fluidRow(column(1, checkboxInput(ns("task_term_runtime"), "", value = FALSE)),
              column(8, numericInput(ns("task_term_runtime_param"), "runtime", min = 1, max = 1000000, value = 120)))
   })
   output$task_term_evals <- renderUI({
     fluidRow(column(1, checkboxInput(ns("task_term_eval"), "", value = FALSE)),
              column(8, numericInput(ns("task_term_eval_param"), "evals", min = 1, max = 100000, value = 10)))
     })
   observeEvent(c(input$task_term_runtime, input$task_term_runtime_param, input$task_term_eval, input$task_term_eval_param), {
     list_term <- vector(mode = "list")
     if(input$task_term_runtime){
       list_term[["runtime"]] <- c("rt", input$task_term_runtime_param)
     }
     if(input$task_term_eval){
       list_term[["eval"]] <- c("eval", input$task_term_eval_param)
     }
     user_task$terminator <- list_term
   }



   )

    # start----
    observeEvent(input$start ,{
      req(user_task$task)
      param_list <- list(
        "learners" = user_task$learners,
        "ensemle" = user_task$ensemble,
        "resampling" = user_task$resampling,
        "measure" = user_task$measure,
        "fs" = user_task$fs,
        "na_imp" = user_task$na,
        "tuning" = user_task$tuning,
        "tuning_method" = user_task$tuning_method,
        "terminator" = user_task$terminator
      )
      print(reactiveValuesToList(user_task))
      t1 <- reactiveValuesToList(user_task)
    })
  })
}
