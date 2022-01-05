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
    fluidRow(column(2, save_table_ui(ns("save_tbl_stat_summary")), style = "margin-left:-30px;"),
             column(10,verbatimTextOutput(ns("stat_table"))))
    )
}

descriptive_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$selected_feature <- renderUI({
      selectInput(ns("selected_feature"), "select feature", choices = names(user_data()), multiple = FALSE)
    })

    # hist####
    output$bin_width <- renderUI({
      sliderInput(ns("bin_width"), "bin width", min = 0.001, max = 1, value = 0.3, step = 0.001)
    })
    output$hist <- renderPlot({
      plot_color <- RColorBrewer::brewer.pal(n = 3, name = app_settings$plot_color_set)[1]
      user_plot$descr_hist <- plot_hist_server("pot_hist", data = user_data(), feature = input$selected_feature, user_color = plot_color, user_binwidth = input$bin_width)
      return(user_plot$descr_hist)
    })

    # scatter####
    output$point_size <- renderUI({
      numericInput(ns("point_size"), "point size", value = 2, step = 1, min = 1, max = 100)
    })

    output$scatter <- renderPlot({
      plot_color <- RColorBrewer::brewer.pal(n = 3, name = app_settings$plot_color_set)[1]
      user_plot$descr_scatter <- plot_scatter_server("plot_scatter", data = user_data(), target_feature = target_column(),
                                                     selected_feature = input$selected_feature, user_color = plot_color, point_size = input$point_size)
      return(user_plot$descr_scatter)
    })

    # statistic summary ####
    output$stat_table <- renderPrint({
      req(input$selected_feature)
      user_tables$stat_summary <- stat_summary_server("stat_summary", user_data(), input$selected_feature)
      return(user_tables$stat_summary)
      })
    # save btns####
    save_plot_server("save_descr_hist", report = "custom",
                     plot_save = reactive({user_plot$descr_hist}), plot_width = app_settings$plot_download_width, plot_height = app_settings$plot_download_height, plot_dpi = app_settings$plot_download_dpi)
    save_plot_server("save_descr_scatter", report = "custom",
                     plot_save = reactive({user_plot$descr_scatter}), plot_width = app_settings$plot_download_width, plot_height = app_settings$plot_download_height, plot_dpi = app_settings$plot_download_dpi)
    save_table_server("save_tbl_stat_summary", report = "custom", tbl = user_tables$stat_summary, tbl_name = paste0("summary_", input$selected_feature))
  })
}
