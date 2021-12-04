
data_man_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(style = "height: 40px; margin-bottom: 0px;",
      column(4, h3("Remove Column")), column(4, h3("Remove NAs")), column(4, h3("Replace Values"))
    ),
    fluidRow(hr(), style = "height:10px; margin-top: 0px; margin-bottom: 30px;"),
    fluidRow(
      column(4,
             uiOutput(ns("select_col_remove")),
             uiOutput(ns("btn_remove"))),
      column(4,
             uiOutput(ns("select_col_na")),
             fluidRow(column(2, uiOutput(ns("btn_naomit"))), column(2,uiOutput(ns("btn_naomit_all"))), column(8))
             )
    ),
    fluidRow(
      feature_importance_ui(ns("feature_imp"))
    )

  )
}

data_man_server <- function(id, user_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # select col remove ----
    output$select_col_remove <- renderUI({
      validate(need(user_data(), message = "upload your data"))
      selectInput(ns("select_col_remove"), "select column", choices = names(user_data()))
    })
    # remove btn ----
    output$btn_remove <- renderUI({
      validate(need(user_data(), message = FALSE))
      actionButton(ns("btn_remove"), "Remove")
      })
    observeEvent(input$btn_remove, {
      tryCatch(
        {
          print(input$select_col_remove)
          temp_data <- user_data()
          temp_data <- temp_data[, -which(names(temp_data) %in% c(input$select_col_remove))]
          user_data(temp_data)
          if(input$select_col_remove == target_column()){
            target_column(names(user_data())[1])
          }
          temp_fct <- factor_columns()
          temp_fct <- temp_fct[!temp_fct %in% c(input$select_col_remove)]
          factor_columns(temp_fct)

          },error=function(cond){
          message(paste("error while deleting row: ", cond))
        },warning=function(cond){
          message(paste("warn while deleting row: ", cond))
        }
      )

      # get task type
      tryCatch({
        user_task$type <- autoStatistics::identify_CR(user_data(), target_column())
        autoStatistics::debug_console(sprintf("new task type detected: %s", user_task$type))
      }, error=function(cond){
        message(paste("error while detect the task type when removing a column: ", cond))
      })
      # create task
      tryCatch(
        {
          temp_data <- user_data()
          temp_data <- temp_data[!is.na(temp_data[[{{ target_column() }}]]), ]

          user_task$task <- autoStatistics::create_task(temp_data, target_column(), user_task$type)
          autoStatistics::debug_console(sprintf("new task created with type: %s", user_task$type))
          print(user_task$task)
        },
        error=function(cond){
          message(paste("Error while creating  when removing a column: ", cond))
        }
      )
    })
    # remove NAS----
    output$select_col_na <- renderUI({
      validate(need(user_data(), message = "upload your data"))
      selectInput(ns("select_col_na"), "select column", choices = names(user_data()))
    })
    output$btn_naomit <- renderUI({
      validate(need(user_data(), message = FALSE))
      actionButton(ns("btn_naomit"), "omit")
    })
    output$btn_naomit_all <- renderUI({
      validate(need(user_data(), message = FALSE))
      actionButton(ns("btn_naomit_all"), "omit all")
    })
    observeEvent(input$btn_naomit,{
      temp_data <- user_data()
      temp_data <- temp_data[!is.na(temp_data[[{{ input$select_col_na }}]]), ]
      user_data(temp_data)
      # get task type
      tryCatch({
        user_task$type <- autoStatistics::identify_CR(user_data(), target_column())
        autoStatistics::debug_console(sprintf("new task type detected: %s", user_task$type))
      }, error=function(cond){
        message(paste("error while detect the task type when removing NAs from a certain column: ", cond))
      })
      # create task
      tryCatch(
        {
          temp_data <- user_data()
          temp_data <- temp_data[!is.na(temp_data[[{{ target_column() }}]]), ]

          user_task$task <- autoStatistics::create_task(temp_data, target_column(), user_task$type)
          autoStatistics::debug_console(sprintf("new task created with type: %s", user_task$type))
        },
        error=function(cond){
          message(paste("Error while creating  when removing NAs from a certain column: ", cond))
        }
      )

    })
    # omit all####
    observeEvent(input$btn_naomit_all, {
      temp_data <- user_data()
      user_data(na.omit(temp_data))
      # get task type
      tryCatch({
        user_task$type <- autoStatistics::identify_CR(user_data(), target_column())
        autoStatistics::debug_console(sprintf("new task type detected: %s", user_task$type))
      }, error=function(cond){
        message(paste("error while detect the task type when removing NAs from all columns: ", cond))
      })
      # create task
      tryCatch(
        {
          temp_data <- user_data()
          temp_data <- temp_data[!is.na(temp_data[[{{ target_column() }}]]), ]

          user_task$task <- autoStatistics::create_task(temp_data, target_column(), user_task$type)
          autoStatistics::debug_console(sprintf("new task created with type: %s", user_task$type))
        },
        error=function(cond){
          message(paste("Error while creating  when removing NAs from all columns: ", cond))
        }
      )
    })
    feature_importance_server("feature_imp", user_task = user_task$task, user_filters = pre_feature_import_filter)
  })
}
