

ui <- navbarPage(
  title = "AutoStats",
  tabPanel("Upload Data",
           data_upload_ui("data")),
  tabPanel("Missing Data",
           missings_ui("missing_data")),
  tabPanel("Edit Data",
           data_man_ui("edit_data")),
  tabPanel("autoML",
           auto_ml_ui("autoML")),
  tabPanel("Report",
           report_ui("report"))
)
