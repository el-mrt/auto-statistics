
auto_ml_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
             h3("Settings"),
             uiOutput(ns("task_type")),
             uiOutput(ns("task_na")),
             uiOutput(ns("task_learner")),
             uiOutput(ns("task_feature")),
             uiOutput(ns("task_tuning")),
             uiOutput(ns("task_ensemble"))
             ),
      column(8,
             h3("Result")
             )
    )
  )
}

auto_ml_server <- function(id, user_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # task type
    output$task_type <- renderUI({
      selectInput(ns("task_type"), "Select task type", choices = c("Regression" = "regr", "Classification" = "classif"), selected = "Regression")
    })
    # NAs
    output$task_na <- renderUI({
      selectInput(ns("task_na"), "Select Imputation of missings", choices = c("Auto" = "auto", "Omit" = "omit", "Mean" = "mean", "Mode" = "mode", "Histogram" = "hist"),
                  selected = "auto")
    })
    # learner
    output$task_learner <- renderUI({
      selectInput(ns("task_learner"), "Select learners", multiple = TRUE,
                  choices = c(
                    "Auto" = "auto",
                    "KNN" = "kknn",
                    "Random Forest" = "ranger",
                    "SVM" = "svm",
                    "XGBoost" = "xgboost"
                  ), selected = "auto")
    })
    #


    #ensemble
    output$task_ensemble <- renderUI({
      checkboxInput(ns("task_ensemble"), label = "Ensemble Learner?", value = FALSE)
    })


  })
}
