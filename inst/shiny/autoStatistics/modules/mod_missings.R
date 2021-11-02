


missings_ui <- function(id){
  ns <- NS(id)
  tagList(
# UI na_per_col --------------------------------------------------------------------------------------------------------------------------
    fluidRow(
      column(4,
             uiOutput(ns("na_per_col_color")),
             uiOutput(ns("na_per_col_line_break")),
             checkboxInput(ns("na_per_col_flip_coord"), "flip Coordinates?", value = FALSE)
             ),
      column(8,
             plotOutput(ns("plot_na_per_col"), height = "500px")
             )
    ),
# UI na_distribution ---------------------------------------------------------------------------------------------------------------------
    fluidRow(
      column(4,
             uiOutput(ns("na_hist_col1")),
             uiOutput(ns("na_hist_col2"))
             ),
      column(8,
             plotOutput(ns("na_hist_plot")),
             uiOutput(ns("debug_text")),
             uiOutput(ns("debug_selected_col"))
             )
    )
  )
}



missings_server <- function(id, user_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
# server na_per_col --------------------------------------------------------------------------------------------------------------------------
    output$na_per_col_color <- renderUI({
      textInput(ns("na_per_col_color"), "Color", value = "#E41A1C")
    })
    output$na_per_col_line_break <- renderUI({
      numericInput(ns("na_per_col_line_break"), "xlabel linebreak", 30, 1, 200, 1)
    })
    output$plot_na_per_col <- renderPlot({
      # get missings per col
      missings_per_col <- sapply(user_data(), function(x){sum(is.na(x))}, simplify = TRUE)
      missings_per_col <- as.data.frame(missings_per_col)
      missings_per_col[["col_name"]] <- rownames(missings_per_col)
      missings_per_col[["col_name"]] <- autoStatistics::insert_line_break(missings_per_col[["col_name"]], n = input$na_per_col_line_break)
      colnames(missings_per_col) <- c("number_na", "col_name")
      # plot
      plot <- ggplot(missings_per_col, aes(x = col_name, y = number_na)) +
        geom_bar(stat="identity", fill = input$na_per_col_color) +
        {if(input$na_per_col_flip_coord) {coord_flip()}} +
        labs(x = "column", y = "number of missing values") +
        theme_minimal()
      plot
    })

# server na_distribution ---------------------------------------------------------------------------------------------------------------------
    output$na_hist_col1 <- renderUI({
      selectInput(ns("na_hist_col1"), "Column 1", names(user_data()))
    })
    output$na_hist_col2 <- renderUI({
      selectInput(ns("na_hist_col2"), "Column 2", c("None", names(user_data())))
    })
    output$na_hist_plot <- renderPlot({
      # plot if only one col selected
      if(input$na_hist_col2 == "None"){
        col_name <- {{input$na_hist_col1}}
        na_hist_data <- user_data()
        na_hist_data[["isna"]] <-



        na_hist_data <- as.data.frame(na_hist_data[[col_name]])
        colnames(na_hist_data) <- c(col_name)
        print(head(na_hist_data))
        na_hist_data[["isna"]] <- is.na(na_hist_data[[col_name]])
        print(head(na_hist_data))


        plot <-
          ggplot(data = na_hist_data, aes(x = col_name, fill = isna)) +
          stat_count(binwidth = 0.05)
      }else{

      }
      plot
    })
    output$debug_text <- renderUI({
      renderText({print(names(user_data()))})
    })
    output$debug_selected_col <- renderUI({
      renderText({print(input$col)})
    })
  })
}
