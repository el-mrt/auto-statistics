


save_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(1,
             textInput(ns("filename"), "filename", value = "")
             ),
      column(2,
             downloadButton(ns("download"), label="hello", style="color: red; margin-top:24px; height: 35px")
             #downloadButton(ns("download"), label="")
             ),
      column(8)
    )
  )

}

save_plot_server <- function(id){
  moduleServer(id, function(input, output, session){
  })
}
