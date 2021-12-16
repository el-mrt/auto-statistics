


data_insight_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(column(12, h3("Coorelation within the Data"))),
    fluidRow(column(2,
                    uiOutput(ns("cor_method")),
                    uiOutput(ns("cor_type"))
                    ),
             column(8, style = "height:800px;", offset = 2,
                    plotOutput(ns("cor_plot"))
                    )),
    fluidRow(
             feature_importance_ui(ns("feature_imp"))
    )
    )
}



data_insight_server <- function(id, user_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$cor_method <- renderUI({
      selectInput(ns("cor_method"), "method", choices = c("Numbers" = "number", "Sqaures" = "square", "Ellipses" = "ellipse", "Shades" = "shade", "Colors" = "color", "Pies" = "pie"))
    })
    output$cor_type <- renderUI({
      selectInput(ns("cor_type"), "type", choices = c("Full" = "full", "Lower" = "lower", "Upper" = "upper"))
    })


# cor plot ----------------------------------------------------------------
    output$cor_plot <- renderPlot({
      req(user_data())
      # only numeric data
      #numeric_cols <- unname(sapply(user_data(), is.numeric))
      numeric_cols <- sapply(user_data(), function(x){
        if(is.numeric(x) & (!is.factor(x))){
          return(TRUE)
        }else{
          return(FALSE)
        }
      })

      cor_data <- user_data()[,numeric_cols]
      cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")
      #print(cor_matrix)
      cur_plot <- corrplot::corrplot(cor_matrix, method = input$cor_method, type = input$cor_type)

      cur_plot
    },height = 800, width = 1000
    )

# feature imp -------------------------------------------------------------

    feature_importance_server("feature_imp", user_task = user_task$task, user_filters = pre_feature_import_filter)

  })
}

