


save_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(1, style = "padding-left: 30px; padding-right: 0px;",
             textInput(ns("filename"), "filename", value = ""),
             ),
      column(11, style = "padding-left: 5px;",
             downloadButton(ns("download"), label="", style="color: black; margin-top:24px; height: 35px;"),
             actionButton(ns("add_report"), label = "", icon = icon("glyphicon glyphicon-plus", lib = "glyphicon"), style="color: black; margin-top:24px; height: 35px;"))
             )
  )
}

save_plot_server <- function(id, plot_save, plot_width = 1920, plot_height = 1080, plot_dpi = 300, text_size = app_settings$plot_download_text_size, report = NULL){
  moduleServer(id, function(input, output, session){

    output$download <- downloadHandler(
      filename = function(){
        paste0(input$filename, ".", app_settings$plot_download_format)
      },
      content = function(file){
        ggsave(file, plot = plot_save() +
                 theme(axis.text=element_text(size=app_settings$plot_download_text_size, family = app_settings$plot_download_text_font),
                       axis.title=element_text(size=app_settings$plot_download_text_size, family = app_settings$plot_download_text_font)),
               width = plot_width, height = plot_height, dpi = plot_dpi, units = "px")
      }
    )

    observeEvent(input$add_report, {
      cur_report <- switch(report,
                           custom = "custom_report")
      # append plot to list
      # add name
      # check if missing name
      if(input$filename == ""){
        report_plots[[cur_report]][["plot_name"]] <- autoStatistics::appendList(report_plots[[cur_report]][["plot_name"]]," ")
        report_plots[[cur_report]][["plot"]] <- autoStatistics::appendList(
          report_plots[[cur_report]][["plot"]],
          plot_save() +
            theme(axis.text=element_text(size=app_settings$plot_download_text_size, family = app_settings$plot_download_text_font),
                  axis.title=element_text(size=app_settings$plot_download_text_size, family = app_settings$plot_download_text_font)))
      }else{
        # check if name is already in list
        if(input$filename %in% report_plots[[cur_report]][["plot_name"]]){
          plot_index <- which(input$filename == report_plots[[cur_report]][["plot_name"]])
          # replace plot
          report_plots[[cur_report]][["plot"]][[plot_index]] <-
            plot_save() +
            theme(axis.text=element_text(size=app_settings$plot_download_text_size, family = app_settings$plot_download_text_font),
                  axis.title=element_text(size=app_settings$plot_download_text_size, family = app_settings$plot_download_text_font))
        }else{
          report_plots[[cur_report]][["plot_name"]] <- autoStatistics::appendList(report_plots[[cur_report]][["plot_name"]], input$filename)
          report_plots[[cur_report]][["plot"]] <- autoStatistics::appendList(
            report_plots[[cur_report]][["plot"]],
            plot_save() +
              theme(axis.text=element_text(size=app_settings$plot_download_text_size, family = app_settings$plot_download_text_font),
                    axis.title=element_text(size=app_settings$plot_download_text_size, family = app_settings$plot_download_text_font)))
        }
      }
      })
    })
  }
