
auto_ml_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3,
             h3("Settings"),
             uiOutput(ns("task_type")),
             uiOutput(ns("task_na")),
             uiOutput(ns("task_learner")),
             uiOutput(ns("task_ensemble")),
             uiOutput(ns("task_feature")),
             uiOutput(ns("task_tuning")),
             conditionalPanel(condition = "input.task_tuning == true",
               tagList(
                 fluidRow(column(1, style = "margin-top: 20px;",
                                 checkboxInput("term_runtime", "", value = FALSE)),
                          column(8,
                                 numericInput("term_runtime_param", "runtime", min = 0, max = 10000000, value = 1800))
                                 )
                                 ,
                 fluidRow(column(1,style = "margin-top: 20px;",
                                 checkboxInput("term_evals", "", value = FALSE)),
                          column(8,
                                 numericInput("term_eval_param", "evals", min = 0, max = 10000000, value = 1000))
                                 )

               ), ns = ns
             ),
      column(9,
             h3("Result")
             )
    )
  )
  )
}

auto_ml_server <- function(id, user_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # task type----
    output$task_type <- renderUI({
      req(task_type())
      selectInput(ns("task_type"), "Select task type", choices = c("To be removed"))
    })

    # NAs----
    output$task_na <- renderUI({
      selectInput(ns("task_na"), "Select Imputation of missings", choices = available_na_imp, selected = "auto")
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
      if(is.null(user_task$type)){
        selectInput(ns("task_learner"), "Select learners",
                    choices = c(""), selected = "")
      }else{
        selectInput(ns("task_learner"), "Select learners", multiple = TRUE,
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
      checkboxInput(ns("task_tuning"), label = "Perform Hyperparameter Tuning", value = FALSE)
    })


    observeEvent(input$task_tuning, {

    })

  })
}
