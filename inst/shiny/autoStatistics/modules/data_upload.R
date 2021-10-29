cat("data... called\n")


data_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 4,
             fileInput(ns("file"), "Upload Data"),
             textInput(ns("sep"), "seperator", value = ";"),
             checkboxInput(ns("header"), "Has Header?"),

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
        read.csv(file = user_file()$datapath, header = input$header, sep = input$sep)
    })
    output$table <- renderDT({raw_data()})

})
}

