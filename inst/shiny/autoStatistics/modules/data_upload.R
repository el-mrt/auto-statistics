cat("data... called\n")


data_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
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
             uiOutput(ns("target_col")),
             hr(),
             h4("Factors"),
             uiOutput(ns("fct_cols"))

      ),
      column(width = 10,
             DTOutput(ns("table")))
      )
  )
}


data_upload_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # upload data
    listen_upload <- reactive({
      list(input$file, input$sep, input$header, input$NA_string, input$dec_symbol)
    })
    output$table <- renderDT({
      validate(need(user_data(), message = "upload your data"))
      user_data()
    })
    observeEvent(listen_upload(), {
      req(input$file)
      tryCatch(
        {
          autoStatistics::debug_console(sprintf("File changed: %s", input$file$name))
          temp <- switch(tools::file_ext(input$file$datapath),
                         txt = read.table(file = input$file$datapath, header = input$header, sep = input$sep,
                                          na.strings = input$NA_string, dec = input$dec_symbol),
                         csv = read.csv(file = input$file$datapath, header = input$header, sep = input$sep,
                                        na.strings = input$NA_string, dec = input$dec_symbol))
          user_data(temp)
          # detect factor cols
          #n_cols <- ncol(user_data())
          #is_col_fct <- vector(length = n_cols)
          #for(i in seq(n_cols)){
          #  is_col_fct[i] <- autoStatistics::identify_CR(user_data(), i)
          #}
          #is_col_fct[is_col_fct == "classif"] <- TRUE
          #is_col_fct[is_col_fct == "regr"] <- FALSE
          #fct_cols <- which(is_col_fct == TRUE)
          #factor_columns(names(user_data())[fct_cols])

        },
      error=function(cond){
        # update header checkbox to avoid that the table is getting stuck -> remove when error message is shown to the user
        updateCheckboxInput(inputId = "header", label = "Has Header?!", value = FALSE)
        output$error_message_upload <-
        output$error_message_upload <- autoStatistics::render_error("FILETOTABLE", cond = cond)
          #renderText({autoStatistics::print_error("FILETOTABLE", cond)})
      })
    })


    # select target column and create task
    output$target_col <- renderUI({
      selectInput(ns("target_col"), "select Target Column", names(user_data()))
    })
    observeEvent(input$target_col, {
      target_column(input$target_col)
      req(user_data())
      # detect type of task
      task_type(autoStatistics::identify_CR(user_data(), input$target_col))
      autoStatistics::debug_console(sprintf("new task type detected: %s", task_type()))
      # create task
      tryCatch(
        {
          user_task(autoStatistics::create_task(user_data(), input$target_col, task_type()))
          autoStatistics::debug_console(sprintf("new created: %s", user_task()))
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
      # update reactive val with factor cols
      factor_columns(input$fct_cols)
      temp_data <- user_data()
      temp_data[input$fct_cols] <- lapply(temp_data[input$fct_cols], as.factor)
      user_data(temp_data)
    })
})
}


