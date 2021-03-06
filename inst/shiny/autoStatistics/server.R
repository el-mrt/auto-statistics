



server <- function(input, output, session){
  landing_server("landing_page")
  data_upload_server("data")
  missings_server("missing_data", user_data = user_data, target_col = target_column)
  data_man_server("edit_data", user_data())
  auto_ml_server("autoML", user_data = user_data())
  report_server("report", user_data = user_data())
  setting_server("settings")
  descriptive_server("descriptive")
  data_insight_server("data_insights", user_data = user_data())
}
