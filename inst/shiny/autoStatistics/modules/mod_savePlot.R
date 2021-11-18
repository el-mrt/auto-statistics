


save_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(1, style = "padding-left: 30px; padding-right: 0px;",
             textInput(ns("filename"), "filename", value = ""),
             ),
      column(11, style = "padding-left: 5px;",
             downloadButton(ns("download"), label="", style="color: black; margin-top:24px; height: 35px;"),
             actionButton(ns("test_btn"), label = "", icon = icon("glyphicon glyphicon-plus", lib = "glyphicon"), style="color: black; margin-top:24px; height: 35px;"))
             )
  )

}

save_plot_server <- function(id, plot_save){
  moduleServer(id, function(input, output, session){

    observeEvent(input$test_btn, {



    })
    output$download <- downloadHandler(
      filename = function(){
        paste0(input$filename)
      },
      content = function(file){
        ggsave(file, plot = plot_save())
      }
    )


  })
}
