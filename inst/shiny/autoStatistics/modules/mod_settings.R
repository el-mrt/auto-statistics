

settings_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(style = "margin-left: 15px;",
      column(4,
             h3("Colors"),
             uiOutput(ns("color_set")),
             uiOutput(ns("color_custom_missing_check")),
             conditionalPanel(condition = "input.color_custom_missing_check == true",
                              uiOutput(ns("color_custom_missing")), ns = ns)
             ),
      column(4,
             h3("Download"),
             fluidRow(
               column(3, uiOutput(ns("plot_download_width"))),
               column(3, uiOutput(ns("plot_download_height"))),
               column(3, uiOutput(ns("plot_download_dpi"))),
               column(3, uiOutput(ns("plot_download_format")))

               ),
             fluidRow(
               column(3, uiOutput(ns("plot_download_text_size"))),
               column(3, uiOutput(ns("plot_download_text_font")))
               )
             )
    ),
    fluidRow(style = "margin-left: 30px; margin-top: 50px;", #Plot Apply Button
      actionButton(ns("plot_btn"), label = "Apply", style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
  )
}



setting_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # colors
    output$color_set <- renderUI({selectInput(ns("color_set"), "Color Set", choices = rownames(RColorBrewer::brewer.pal.info), selected = app_settings$plot_color_set)})
    output$color_custom_missing <- renderUI({
      fluidRow(
        column(3,textInput(ns("color_custom_missing_not_na"), "not NA", value = app_settings$plot_color_miss_custom[1])),
        column(3,textInput(ns("color_custom_missing_na"), "NA", value = app_settings$plot_color_miss_custom[2]))
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
    output$plot_download_text_font <- renderUI({selectInput(ns("plot_download_text_font"), "text font", choices = available_fonts)})

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
      app_settings$plot_download_text_font <- input$plot_download_text_font

      print(reactiveValuesToList(app_settings))
      })
  })
}
