

landing_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(style="margin-bottom:50px;",
      column(8, offset = 3,
             img(src="ad_top.gif", align = "left",height='200px',width='1100px')
             )

    ),
    fluidRow(
      column(2,
            img(src="ad_02.gif", align = "left",height='300px',width='250px'),
            img(src="ad_03.gif", align = "left",height='300px',width='250px')
            ),
      column(2, offset = 8,
             img(src="ad_01.gif", align = "left",height='800px',width='250px')
             )
    )
  )
}



landing_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
  })
}

cat("landing called!\n")
