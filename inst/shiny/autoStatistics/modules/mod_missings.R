


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
    fluidRow(div(style = "height:50px;"),
      column(4,
             uiOutput(ns("na_hist_col1")),
             sliderInput(ns("na_hist_bins"), "bin width", min = 0.001, max = 0.3, value = 0.02, step = 0.001),
             uiOutput(ns("na_hist_col2"))
             ),
      column(8,
             plotOutput(ns("na_hist_plot"), height = "500px"),
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
      missings_per_col <- sapply(user_data()[["data"]], function(x){sum(is.na(x))}, simplify = TRUE)
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
      selectInput(ns("na_hist_col1"), "Column 1", names(user_data()[["data"]]))
    })
    output$na_hist_col2 <- renderUI({
      selectInput(ns("na_hist_col2"), "Column 2", c("None", names(user_data()[["data"]])))
    })
    output$na_hist_plot <- renderPlot({
      col_name1 <- {{input$na_hist_col1}}
      col_name2 <- {{input$na_hist_col2}}
      na_hist_data <- user_data()[["data"]]
      target_col <- user_data()[["target_col"]]
      na_hist_data[["isna"]] <- is.na(na_hist_data[[col_name1]])

      # plot if only one col selected
      if(input$na_hist_col2 == "None"){
        cur_plot <- ggplot(na_hist_data, aes(x = get(target_col), fill = isna)) +
          geom_histogram(binwidth = input$na_hist_bins) +
          scale_fill_manual(values = c("#377EB8", "#E41A1C")) +
          labs(x = target_col) +
          theme_minimal()
      }else{
        cur_plot <- ggplot(na_hist_data, aes(x = get(target_col), y = get(col_name2), color = isna)) +
          geom_jitter() +
          scale_color_manual(values = c("#377EB8", "#E41A1C")) +
          labs(x = target_col, y = col_name2) +
          theme_minimal()


      }
      cur_plot
    })
    output$debug_text <- renderUI({
      renderText({print(names(user_data()[["data"]]))})
    })
    output$debug_selected_col <- renderUI({
      renderText({print(input$col)})
    })
  })
}
