cat("data... called\n")


# data_upload -------------------------------------------------------------------------------------------------------------------------


data_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      verbatimTextOutput(ns("session_id"))
    ),
    fluidRow(
      uiOutput(ns("error_message_upload"))
    ),
    fluidRow(
      column(width = 2,
             fileInput(ns("file"), "Upload Data"),
             textInput(ns("sep"), "seperator", value = ","),
             checkboxInput(ns("header"), "Has Header?", value = TRUE),
             textInput(ns("NA_string"), "NA string", value = "NaN"),
             textInput(ns("dec_symbol"), "decimal symbol", value = "."),
             uiOutput(ns("target_col"))
      ),
      column(width = 10,
             DTOutput(ns("table")))
      ),
    fluidRow(hr()),
    fluidRow(
      column(width = 2,
             h4("Factors"),
             uiOutput(ns("fct_cols"))
             ),
      column(width = 10,
             fluidRow(
               h4("")
               )
      )
    )
  )
}


data_upload_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    #debug
    output$session_id <- renderPrint({
      paste0("session id:  ", session$token)
    })

    # upload data
    observeEvent(input$file, {
      tryCatch(
        {
          user_file(input$file$datapath)
          autoStatistics::debug_console(sprintf("new file uploaded: ", input$file$name))
        },
        error=function(cond){
          output$error_message_upload <- renderUI({autoStatistics::render_error("UPLOAD", cond)})
        }
      )

    })
    # read data into dataframe
    observeEvent(c(input$file,input$sep,input$header,input$NA_string,input$dec_symbol), {
      req(user_file())
      tryCatch(
        {
          temp <- switch(tools::file_ext(user_file()),
                         txt = read.table(file = user_file(), header = input$header, sep = input$sep,
                                          na.strings = input$NA_string, dec = input$dec_symbol),
                         csv = read.csv(file = user_file(), header = input$header, sep = input$sep,
                                        na.strings = input$NA_string, dec = input$dec_symbol))
          user_data(temp)
          autoStatistics::debug_console(sprintf("Data loaded into a dataframe"))

        },
        error = function(cond){
          output$error_message_upload <- renderUI({autoStatistics::render_error("FILETOTABLE", cond)})
        }
      )
    # detect factor columns
      tryCatch(
        {
          col_types <- lapply(names(user_data()), function(col_name){
            autoStatistics::identify_CR(user_data(), col_name, 10)
          })
          col_types <- as.vector(unlist(col_types))
          factor_col_index <- which(col_types == "classif")
          factor_columns(names(user_data()[factor_col_index]))
          # update columns from numeric to factors
          temp_data <- user_data()
          for(col in factor_columns()){
            temp_data[[{{ col }}]] <- as.factor(temp_data[[{{ col }}]])
          }
          user_data(temp_data)

        }, error = function(cond){
          message(paste0(cond))
        }
      )
    })
    # render DT
    output$table <- renderDT({
      validate(need(user_data(), message = "upload your data"))
      user_data()
      })

    # select target column and create task
    output$target_col <- renderUI({
      if(is.null(target_column())){target_column(names(user_data())[1])}
      selectInput(ns("target_col"), "select Target Column", names(user_data()), selected = target_column())
    })


    observeEvent(input$target_col, {
      req(user_data())
      target_column(input$target_col) # update target column

      # detect type of task
      task_type(autoStatistics::identify_CR(user_data(), target_column()))
      autoStatistics::debug_console(sprintf("new task type detected: %s", task_type()))
      # create task
      tryCatch(
        {
          req(task_type())
          user_task(autoStatistics::create_task(user_data(), target_column(), task_type()))
          autoStatistics::debug_console(sprintf("new task created with type: %s", user_task()$task_type))
        },
        error=function(cond){
          message(paste("test: ", cond))
        }
      )

    })

    # select factor cols
    output$fct_cols <- renderUI({
      req(user_data())
      selectInput(ns("fct_cols"), "select factor columns", choices = names(user_data()), multiple = TRUE, selected = factor_columns())
    })
    observeEvent(input$fct_cols, {

      if(length(setdiff(factor_columns(), input$fct_cols)) > 0 ||
         length(setdiff(input$fct_cols, factor_columns())) > 0){

        temp_fct_updated <- autoStatistics::update_factor_cols(user_data(), old_cols = factor_columns(), new_cols = input$fct_cols)
        user_data(temp_fct_updated$data)
        factor_columns(temp_fct_updated$new_factors_names)
      }


    })
})
}


# data_fct_warn -----------------------------------------------------------------------------------------------------------------------
data_factor_warn_ui <- function(id){

}

data_factor_warn_server <- function(id) {
  moduleServer(id, function(input, output, session){


  })
}
