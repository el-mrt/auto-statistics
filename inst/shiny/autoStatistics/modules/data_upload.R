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
    user_file <- reactive({
      validate(need(input$file, message = FALSE))
      input$file
      })
    file_ext <- reactive({tools::file_ext(user_file()$datapath)})
    output$debug_file_type <- renderText({print(file_ext())})

    raw_data <- reactive({
      if(file_ext() == "csv"){
        user_df <- read.csv(file = user_file()$datapath, header = input$header, sep = input$sep, na.strings = input$NA_string, dec = input$dec_symbol)
      }
      else if(file_ext() == "txt"){
        user_df <- read.table(file = user_file()$datapath, header = input$header, sep = input$sep, na.strings = input$NA_string, dec = input$dec_symbol)
      }
      user_df
    })
    output$table <- renderDT({raw_data()})
    # ToDo: select target column
    output$target_col <- renderUI({
      selectInput(ns("target_col"), "select Target Column", names(raw_data()))
    })


    return(reactive({list(
      "data" = raw_data(),
      "target_col" = input$target_col
      )}))
})
}

