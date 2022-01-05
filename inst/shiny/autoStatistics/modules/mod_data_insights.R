


data_insight_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(2,
                    h3("Correlation within the Data"),
                    br(),
                    uiOutput(ns("cor_method")),
                    uiOutput(ns("cor_type"))
                    ),
             column(8, style = "height:1100px;", offset = 2,
                    plotOutput(ns("cor_plot"))
             )),
    fluidRow(column(2)),
    fluidRow(feature_importance_ui(ns("feature_imp")))
  )
}



data_insight_server <- function(id, user_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$cor_method <- renderUI({
      selectInput(ns("cor_method"), "Method", choices = c("Numbers" = "number", "Sqaures" = "square", "Ellipses" = "ellipse", "Shades" = "shade", "Colors" = "color", "Pies" = "pie"))
    })
    output$cor_type <- renderUI({
      selectInput(ns("cor_type"), "Type", choices = c("Full" = "full", "Lower" = "lower", "Upper" = "upper"))
    })


# cor plot ----------------------------------------------------------------
    output$cor_plot <- renderPlot({
      req(user_data())

      user_plot$cor_plot <- plot_cor_server("cor_plot", data = user_data(), method = input$cor_method, type = input$cor_type)
      return(user_plot$cor_plot)
    },
    height = 1100, width = 1100
    )

# feature imp -------------------------------------------------------------

    feature_importance_server("feature_imp", user_task = user_task$task, user_filters = pre_feature_import_filter)
  })
}

