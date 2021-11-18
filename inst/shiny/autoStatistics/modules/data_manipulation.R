
data_man_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(style = "height: 40px; margin-bottom: 0px;",
      column(4, h3("Remove Column")), column(4, h3("Remove NAs")), column(4, h3("Replace Values"))
    ),
    fluidRow(hr(), style = "height:10px; margin-top: 0px; margin-bottom: 30px;"),
    fluidRow(
      column(4,
             uiOutput(ns("select_col_remove")),
             uiOutput(ns("btn_remove"))),
      column(4,
             uiOutput(ns("select_col_na")),
             fluidRow(column(2, uiOutput(ns("btn_naomit"))), column(2,uiOutput(ns("btn_naomit_all"))), column(8))
             )
    ),
    fluidRow(
      feature_importance_ui(ns("feature_imp"))
    )

  )
}

data_man_server <- function(id, user_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # select col remove
    output$select_col_remove <- renderUI({
      validate(need(user_data(), message = "upload your data"))
      selectInput(ns("select_col_remove"), "select column", choices = names(user_data()))
    })
    # remove btn
    output$btn_remove <- renderUI({
      validate(need(user_data(), message = FALSE))
      actionButton(ns("btn_remove"), "Remove")
      })
    observeEvent(input$btn_remove, {
      temp_data <- user_data()
      temp_data <- temp_data[, -which(names(temp_data) %in% c(input$select_col_remove))]
      user_data(temp_data)
    })
    # remove NAS
    output$select_col_na <- renderUI({
      validate(need(user_data(), message = "upload your data"))
      selectInput(ns("select_col_na"), "select column", choices = names(user_data()))
    })
    output$btn_naomit <- renderUI({
      validate(need(user_data(), message = FALSE))
      actionButton(ns("btn_naomit"), "omit")
    })
    output$btn_naomit_all <- renderUI({
      validate(need(user_data(), message = FALSE))
      actionButton(ns("btn_naomit_all"), "omit all")
    })
    observeEvent(input$btn_naomit,{
      temp_data <- user_data()
      temp_data <- temp_data[!is.na(temp_data[[{{ input$select_col_na }}]]), ]
      user_data(temp_data)
    })
    observeEvent(input$btn_naomit_all, {
      temp_data <- user_data()
      user_data(na.omit(temp_data))
    })
    feature_importance_server("feature_imp", task = user_task$task, filters = pre_feature_import_filter)
  })
}
