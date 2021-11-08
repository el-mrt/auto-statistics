
data_man_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("select_col")),
    uiOutput(ns("btn_remove"))
  )
}

data_man_server <- function(id, user_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # select col
    output$select_col <- renderUI({
      validate(need(user_data(), message = FALSE))
      selectInput(ns("select_col"), "select column", choices = names(user_data()))
    })
    # remove btn
    output$btn_remove <- renderUI({
      validate(need(user_data(), message = FALSE))
      actionButton(ns("btn_remove"), "Remove")
      })
    observeEvent(input$btn_remove, {
      temp_data <- user_data()
      temp_data <- temp_data[, -which(names(temp_data) %in% c(input$select_col))]
      user_data(temp_data)



    })

  })
}
