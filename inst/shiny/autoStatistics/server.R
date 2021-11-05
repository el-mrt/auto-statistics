



server <- function(input, output, session){
  data_upload_server("data")
  output$debug_text <- renderText({
    print(paste0(c("debug: ", names(rV$user_data))))
    })
  missings_server("missing_data", user_data = rV$user_data, user_target_col = rV$target_column)

}
