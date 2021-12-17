# histogram
# scatterplot target var
# conditonal density
# summary table
# correlation



descriptive_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2,
             uiOutput(ns("selected_feature"))
             )
    ),
    fluidRow(column(12,h3("Histogram"))),
    fluidRow(
      column(2,
             uiOutput(ns("bin_width")),
             ),
      column(10,
             plotOutput(ns("hist"))
             )),
    fluidRow(save_plot_ui(ns("save_descr_hist"))),
    fluidRow(column(12,h3("Scatterplot - Target Feature"))),

    fluidRow(
      column(2,
             uiOutput(ns("point_size"))
             ),
      column(10,
             plotOutput(ns("scatter")))
             ),
    fluidRow(save_plot_ui(ns("save_descr_scatter"))),
    fluidRow(column(12, h3("statistic summary"))),
    fluidRow(column(2),
             column(10,verbatimTextOutput(ns("stat_table"))))
    )
}

descriptive_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$selected_feature <- renderUI({
      selectInput(ns("selected_feature"), "select feature", choices = names(user_data()), multiple = FALSE)
    })
    output$bin_width <- renderUI({
      sliderInput(ns("bin_width"), "bin width", min = 0.001, max = 1, value = 0.3, step = 0.001)
    })


    # hist####
    output$hist <- renderPlot({
      req(input$selected_feature)

      plot_color <- RColorBrewer::brewer.pal(n = 3, name = app_settings$plot_color_set)[1]
      if(is.factor(user_data()[[{{ input$selected_feature }}]])){
        cur_plot <- ggplot(user_data(), aes(x = get(input$selected_feature), fill = TRUE)) +
          geom_bar(na.rm = TRUE) +
          scale_fill_manual(values = plot_color) +
          labs(x = input$selected_feature) +
          theme_minimal()
      }else{
        cur_plot <- ggplot(user_data(), aes(x = get(input$selected_feature), fill = TRUE)) +
          geom_histogram(binwidth = input$bin_width) +
          scale_fill_manual(values = plot_color) +
          labs(x = input$selected_feature) +
          theme_minimal()
      }

      user_plot$descr_hist <- cur_plot
      cur_plot
    })
    # scatter ####
    output$point_size <- renderUI({
      numericInput(ns("point_size"), "point size", value = 2, step = 1, min = 1, max = 100)
    })

    output$scatter <- renderPlot({
      req(input$selected_feature)

      plot_color <- RColorBrewer::brewer.pal(n = 3, name = app_settings$plot_color_set)[1]
      cur_plot <- ggplot(user_data(),aes(x = get(target_column()),y = get(input$selected_feature)))+
        geom_point(color = plot_color, size = input$point_size) +
        theme_minimal()

      user_plot$descr_scatter <- cur_plot
      cur_plot
    })

    # statistics
    output$stat_table <- renderPrint({
      req(input$selected_feature)
      summary(user_data()[[input$selected_feature]])
      })
    # save btns####
    save_plot_server("save_descr_hist", report = "custom",
                     plot_save = reactive({user_plot$descr_hist}), plot_width = app_settings$plot_download_width, plot_height = app_settings$plot_download_height, plot_dpi = app_settings$plot_download_dpi)
    save_plot_server("save_descr_scatter", report = "custom",
                     plot_save = reactive({user_plot$descr_scatter}), plot_width = app_settings$plot_download_width, plot_height = app_settings$plot_download_height, plot_dpi = app_settings$plot_download_dpi)
  })
}
