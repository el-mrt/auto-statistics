

settings_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(style = "margin-left: 30px;", #Plot
      h3("Plot"), hr(), h4("Colors")),
    fluidRow(style = "margin-left: 30px;", #Plot colors
      column(2, uiOutput(ns("color_set"))),
      column(2, uiOutput(ns("color_custom_missing_check")), style = "margin-top:20px; margin-right:0px;"),
      column(6, conditionalPanel(condition = "input.color_custom_missing_check == true",
                                 uiOutput(ns("color_custom_missing")), ns = ns), style= "margin-left: 0px;")
    ),
    fluidRow(h4("Download"),style = "margin-left: 30px;",
      column(1, uiOutput(ns("plot_download_width"))),
      column(1, uiOutput(ns("plot_download_height"))),
      column(1, uiOutput(ns("plot_download_dpi"))),
      column(1, uiOutput(ns("plot_download_format"))),
      column(1, uiOutput(ns("plot_download_text_size")))



      ),
    fluidRow(style = "margin-left: 30px;", #Plot Apply Button
      actionButton(ns("plot_btn"), label = "Apply"))
  )
}



setting_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # colors
    output$color_set <- renderUI({selectInput(ns("color_set"), "Color Set", choices = rownames(RColorBrewer::brewer.pal.info), selected = app_settings$plot_color_set)})
    output$color_custom_missing <- renderUI({
      fluidRow(
        column(2,textInput(ns("color_custom_missing_not_na"), "not NA", value = app_settings$plot_color_miss_custom[1])),
        column(2,textInput(ns("color_custom_missing_na"), "NA", value = app_settings$plot_color_miss_custom[2]))
      )
    })
    output$color_custom_missing_check <- renderUI({
      checkboxInput(ns("color_custom_missing_check"), "Custom Colors missings", value = ifelse(is.null(app_settings$plot_color_miss_custom), FALSE, TRUE))
    })
    # download
    output$plot_download_width <- renderUI({numericInput(ns("plot_download_width"), "width [px]", min = 0, max = 10000, value = app_settings$plot_download_width)})
    output$plot_download_height <- renderUI({numericInput(ns("plot_download_height"), "height [px]", min = 0, max = 10000, value = app_settings$plot_download_height)})
    output$plot_download_dpi <- renderUI({numericInput(ns("plot_download_dpi"), "dpi", min = 0, max = 1000, value = app_settings$plot_download_dpi)})
    output$plot_download_format <- renderUI({selectInput(ns("plot_download_format"), "format", c("PDF" = "pdf", "PNG" = "png", "JPEG" = "jpeg", "SVG" = "svg"), "pdf")})
    output$plot_download_text_size <- renderUI({numericInput(ns("plot_download_text_size"), "text size", 4, 1, 50, 1)})

    observeEvent(input$plot_btn, {
      app_settings$plot_color_set <- input$color_set
      if(input$color_custom_missing_check && (input$color_custom_missing_not_na != "") && (input$color_custom_missing_na != "")){
        app_settings$plot_color_miss_custom <- c(input$color_custom_missing_not_na, input$color_custom_missing_na)
      }else{
        app_settings$plot_color_miss_custom <- NULL
      }

      app_settings$plot_download_width <- input$plot_download_width
      app_settings$plot_download_height <- input$plot_download_height
      app_settings$plot_download_dpi <- input$plot_download_dpi
      app_settings$plot_download_format <- input$plot_download_format
      app_settings$plot_download_text_size <- input$plot_download_text_size



      print(reactiveValuesToList(app_settings))
      })





  })
}
