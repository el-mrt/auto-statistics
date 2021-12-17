

ui <- navbarPage(
  title = "AutoStats",
  tabPanel("Home",
           landing_ui("landing_page")),
  tabPanel("Upload Data",
           data_upload_ui("data")),
  tabPanel("Descriptive",
           descriptive_ui("descriptive")
           ),
  tabPanel("Data Insights",
           data_insight_ui("data_insights")),
  tabPanel("Missing Data",
           missings_ui("missing_data")),
  tabPanel("Edit Data",
           data_man_ui("edit_data")),
  tabPanel("autoML",
           auto_ml_ui("autoML")),
  tabPanel("Report",
           report_ui("report")),
  tabPanel("Settings",
           settings_ui("settings"))
)
