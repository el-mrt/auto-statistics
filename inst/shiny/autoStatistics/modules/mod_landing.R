

landing_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(8, offset = 3,
             img(src="ad_top_01.gif", height = "200px", width="1000px")
      )
    ),
    fluidRow(
      column(2,
            img(src="ad_02.gif", align = "left",height='300px',width='250px'),
            img(src="ad_03.gif", align = "left",height='300px',width='250px')
            ),
      column(8,
             h1("Description"),
             HTML(app_descriptions[["landing_descr"]]),
             h5(),
             HTML(app_descriptions[["landing_descr_logo"]])

             # verbatimTextOutput(app_descriptions[["landing_page"]])
             # #includeHTML("./www/landing_text.html")
             ),
      column(2,
             img(src="ad_01.gif", align = "left",height='800px',width='250px')
             )
    ),
    fluidRow(
      column(3, uiOutput("logo_tud")),
      column(2, uiOutput("logo_github"))
    )
  )
}



landing_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$logo_tud <- renderUI({
      tags$a(imageOutput("./www/Logo_TUD.png"),href="https://tu-dresden.de/bu/verkehr/ivw/bda?set_language=en")
    })


  })
}

cat("landing called!\n")
