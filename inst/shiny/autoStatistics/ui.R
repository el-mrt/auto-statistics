

ui <- navbarPage(
  title = "AutoStats",
  tabPanel("Upload Data",
           data_upload_ui("data"))
