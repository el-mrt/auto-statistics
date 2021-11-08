cat("data... called\n")


data_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 2,
             fileInput(ns("file"), "Upload Data"),
             textInput(ns("sep"), "seperator", value = ","),
             checkboxInput(ns("header"), "Has Header?", value = TRUE),
             textInput(ns("NA_string"), "NA string", value = "NaN"),
             textInput(ns("dec_symbol"), "decimal symbol", value = "."),
             uiOutput(ns("target_col"))

      ),
      column(width = 8,
             textOutput(ns("debug_file_type")),
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

    observeEvent(listen_upload(), {
      req(input$file)

      tryCatch(
        {
          debug_console(sprintf("File changed: %s", input$file$name))
          temp <- switch(tools::file_ext(input$file$datapath),
                         txt = read.table(file = input$file$datapath, header = input$header, sep = input$sep,
                                          na.strings = input$NA_string, dec = input$dec_symbol),
                         csv = read.csv(file = input$file$datapath, header = input$header, sep = input$sep,
                                        na.strings = input$NA_string, dec = input$dec_symbol))
          user_data(temp)
        },
      error=function(cond){
        # update header checkbox to avoid that the table is getting stuck -> remove when error message is shown to the user
        updateCheckboxInput(inputId = "header", label = "Has Header?!", value = FALSE)
        message(paste("test: ", cond))
      })
    })

    output$table <- renderDT({
      validate(need(user_data(), message = "upload your data"))
      user_data()
    })

    # select target column
    output$target_col <- renderUI({
      selectInput(ns("target_col"), "select Target Column", names(user_data()))
    })
    observeEvent(input$target_col, {
      target_column(input$target_col)
    })
})
}


