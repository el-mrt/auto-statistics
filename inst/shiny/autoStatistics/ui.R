

ui <- navbarPage(
  title = "AutoStats",
  tabPanel("Upload Data",
           data_upload_ui("data")),
  tabPanel("Missing Data",
           missings_ui("missing_data"))
)
