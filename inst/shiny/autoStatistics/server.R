



server <- function(input, output, session){
  user_data <- data_upload_server("data")
  output$debug_text <- renderText({
    print(paste0(c("debug: ", names(user_data()))))
    })
  missings_server("missing_data", user_data = user_data)

}
