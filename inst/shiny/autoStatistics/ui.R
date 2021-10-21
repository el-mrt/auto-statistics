library("shiny")

#ui <- navbarPage("App Title",
 #                tabPanel("Data"),
  #               tabPanel("Placeholder"))


ui <- navbarPage("App Title",
                 tabPanel("Data",file_input_ui("my_file_ui")))
