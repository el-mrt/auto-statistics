

ui <- navbarPage(
  title = "AutoStats",
  tabPanel("Upload Data",
           textOutput("debug_text"),
           data_upload_ui("data")),
  tabPanel("Missing Data",
           missings_ui("missing_data"))
)
