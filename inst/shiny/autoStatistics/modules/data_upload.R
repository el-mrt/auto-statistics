cat("data... called\n")


data_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4,
             fileInput(ns("file"), "Upload Data"),
             textInput(ns("sep"), "seperator", value = ";"),
             checkboxInput(ns("header"), "Has Header?"),
             textInput(ns("NA_string"), "NA string", value = "NA"),
             textInput(ns("dec_symbol"), "decimal symbol", value = ".")

      ),
      column(width = 8,
             textOutput(ns("debug_file_type")),
             DTOutput(ns("table")))
    )
  )
}


data_upload_server <- function(id){
  moduleServer(id, function(input, output, session){
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

})
}

