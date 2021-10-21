


file_input_ui <- function(id){
  ns <- NS(id)

  fluidRow(
    column(4,
           tagList(
             fileInput(ns("file"), "upload your data"),
             textOutput(ns("file_type")),
             uiOutput(ns("file_settings"))
    )),
    column(8,
           tagList(
             DTOutput(ns("preview_data"))
           ))
  )
}


file_input_server <- function(id){
  moduleServer(id, function(input, output, session){

    user_file <- reactive({
      validate(need(input$file, message = FALSE))
      input$file
    })

    output$file_type <- renderText({
      tools::file_ext(user_file()$name)
    })
    output$file_settings <- renderUI({
      ns <- session$ns
      if(tools::file_ext(user_file()$name) == "csv"){
        tagList(
          checkboxInput(ns("header"), label = "has header?",value = FALSE),
          textInput(ns("sep"), label = "seperator", value = ";")
        )
      }
    })
    data_file <- reactive({
      df <- read.csv(user_file()$datapath,
                     header = input$header,
                     sep = input$sep)
      df
    })
    output$preview_data <- renderDT(data_file())

  })
}

