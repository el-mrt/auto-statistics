


report_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2,
             uiOutput(ns("report_type")),
             actionButton(ns("report_generate"), "Generate")
             ),
      column(10,
             h3("Preview Report")
             )
    )
  )
}

report_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$report_type <- renderUI({
      selectInput(ns("report_type"), label = "report type", choices = "custom")
    })

    observeEvent(input$report_generate, {
      print(length(report_plots$custom_report$plot))
      View(report_plots$custom_report$plot)
      print(report_plots$custom_report$plot_name)
    })
  })
}
