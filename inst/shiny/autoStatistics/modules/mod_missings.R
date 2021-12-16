


missings_ui <- function(id){
  ns <- NS(id)
  tagList(
    # UI na_per_col --------------------------------------------------------------------------------------------------------------------------
    h3(HTML("<u><i>Missing values per column</i></u>")),
    fluidRow(
      column(2,
             p(HTML(paste0("<i>",app_descriptions[["plot_mis_per_col"]],"</i>"))),
             uiOutput(ns("na_per_col_line_break")),
             uiOutput(ns("na_per_col_flip_coord"))
             ),
      column(10,
             plotOutput(ns("plot_na_per_col"), height = "500px")
             )
    ),
    fluidRow(save_plot_ui(ns("save_na_per_col"))), # download button
    # UI na_combinations ------------------------------------------------------------------------------------------------------------------
    h3(HTML("<u><i>Missing combinations</u></i>")),
    fluidRow(
      column(2,
             actionButton(ns("start_na_comb"), "Start"),
             p(HTML(paste0("<i>",app_descriptions[["plot_mis_na_comb"]],"</i>"))),
             uiOutput(ns("na_comb_topn")),
             uiOutput(ns("na_comb_line_break")),
             uiOutput(ns("na_comb_use_names"))
      ),
      column(10,
             plotOutput(ns("na_comb_plot"))
      )
    ),
    fluidRow(save_plot_ui(ns("save_na_comb"))), # download button
    # UI na_distribution ---------------------------------------------------------------------------------------------------------------------
    h3(HTML("<u><i>Missing values distribution</i></u>")),
    fluidRow(
      column(2,
             p(HTML("<i>description.....</i>")),
             uiOutput(ns("na_hist_col1")),
             uiOutput(ns("na_hist_bins")),
             uiOutput(ns("na_hist_col2"))
             ),
      column(10,
             plotOutput(ns("na_hist_plot"), height = "500px")
             )
    ),
    fluidRow(save_plot_ui(ns("save_na_dist"))), # download button
  )
}



missings_server <- function(id, user_data, target_col){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # server na_per_col --------------------------------------------------------------------------------------------------------------------------

    # x_label line break
    output$na_per_col_line_break <- renderUI({
      req(user_data())  # req()
      numericInput(ns("na_per_col_line_break"), "x_label linebreak", 30, 1, 200, 1)
    })
    # flip coordinates
    output$na_per_col_flip_coord <- renderUI({
      req(user_data())  # req()
      checkboxInput(ns("na_per_col_flip_coord"), "flip Coordinates?", value = TRUE)
    })
    # plot
    output$plot_na_per_col <- renderPlot({
      # get missings per col
      req(user_data()) # req()
      missings_per_col <- as.data.frame(sapply(user_data(), function(x){sum(is.na(x))}, simplify = TRUE))
      missings_per_col[["col_name"]] <- rownames(missings_per_col)

      missings_per_col[["col_name"]] <- autoStatistics::insert_line_break(missings_per_col[["col_name"]], n = input$na_per_col_line_break)

      colnames(missings_per_col) <- c("number_na", "col_name")
      missings_per_col[["col_name"]] <- factor(missings_per_col[["col_name"]], levels = missings_per_col[["col_name"]])
      # get color from setting
      plot_color <- ifelse(is.null(app_settings$plot_color_miss_custom),
                           RColorBrewer::brewer.pal(n = 3, name = app_settings$plot_color_set)[2],
                           app_settings$plot_color_miss_custom[2])
      # plot
      cur_plot <- ggplot(missings_per_col, aes(x = col_name, y = number_na)) +
        geom_bar(stat="identity", fill = plot_color, na.rm = TRUE) +
        {if(input$na_per_col_flip_coord) {coord_flip()}} +
        labs(x = "column", y = "number of missing values") +
        theme_minimal()
        #+
        # theme(axis.text=element_text(size=12),
        #        axis.title=element_text(size=14,face="bold"))

      user_plot$na_per_col <- cur_plot
      return(cur_plot)
    })

    # server na_combinations --------------------------------------------------------------------------------------------------------------
    # top n
    output$na_comb_topn <- renderUI({
      req(user_data()) # req()
      numericInput(ns("na_comb_topn"), "show top n combinations", 10, 1, 200, 1)
    })
    # line break
    output$na_comb_line_break <- renderUI({
      req(user_data()) # req()
      numericInput(ns("na_comb_line_break"), "x_label linebreak", 30, 1, 200, 1)
    })
    # use names
    output$na_comb_use_names <- renderUI({
      req(user_data()) # req()
      checkboxInput(ns("na_comb_use_names"), "use names?", value = FALSE)
    })
    # plot
    observeEvent(input$start_na_comb, {
      req(user_data()) # req()
      output$na_comb_plot <- renderPlot({
        missing_obj <- autoStatistics::missing_combinations(isolate(user_data()), names_col = TRUE)
        na_comb_label <- if (input$na_comb_use_names) "name" else "index"
        # get color from setting
        plot_color <- ifelse(is.null(app_settings$plot_color_miss_custom),
                             RColorBrewer::brewer.pal(n = 3, name = app_settings$plot_color_set)[2],
                             app_settings$plot_color_miss_custom[2])

        cur_plot <- plot(missing_obj, labels = na_comb_label, label_length = input$na_comb_line_break, show_numbers = FALSE,
                         top_n = input$na_comb_topn, bar_color = plot_color, plot_title = "NA combinations",
                         x_lab = "combination", y_lab = "n")
        user_plot$na_comb <- cur_plot
        return(cur_plot)
      })
    })




    # server na_distribution ---------------------------------------------------------------------------------------------------------------------
    # col1
    output$na_hist_col1 <- renderUI({
      req(user_data()) # req()
      selectInput(ns("na_hist_col1"), "Column 1", names(user_data()))
    })

    # bin width
    output$na_hist_bins <- renderUI({
      req(user_data()) # req()
      sliderInput(ns("na_hist_bins"), "bin width", min = 0.001, max = 1, value = 0.02, step = 0.001)
    })

    # column 2
    output$na_hist_col2 <- renderUI({
      req(user_data()) # req()
      selectInput(ns("na_hist_col2"), "Column 2", c("None", names(user_data())))
    })

    # plot
    output$na_hist_plot <- renderPlot({
      req(user_data(), )
      col_name1 <- {{input$na_hist_col1}}
      col_name2 <- {{input$na_hist_col2}}
      na_hist_data <- user_data()

      na_hist_data[["isna"]] <- is.na(na_hist_data[[col_name1]])
      req(na_hist_data)  # req()
      # get plot color from settings
      if(is.null(app_settings$plot_color_miss_custom)){
        plot_color <- RColorBrewer::brewer.pal(n = 3, name = app_settings$plot_color_set)[1:2]
      }else{
        plot_color <- app_settings$plot_color_miss_custom
      }
      # plot if only one col selected
      if(input$na_hist_col2 == "None"){
        if(is.factor(na_hist_data[[{{ target_col() }}]])){
          cur_plot <- ggplot(na_hist_data, aes(x = get(target_col()), fill = isna)) +
            geom_bar(na.rm = TRUE) +
            scale_fill_manual(values = plot_color) +
            labs(x = target_col()) +
            theme_minimal()
        }else{
          cur_plot <- ggplot(na_hist_data, aes(x = get(target_col()), fill = isna)) +
            geom_histogram(binwidth = input$na_hist_bins) +
            scale_fill_manual(values = plot_color) +
            labs(x = target_col()) +
            theme_minimal()
        }

      }else{
        cur_plot <- ggplot(na_hist_data, aes(x = target_col(), y = get(col_name2), color = isna)) +
          geom_jitter() +
          scale_color_manual(values = plot_color) +
          labs(x = target_col(), y = col_name2) +
          theme_minimal()
      }
      user_plot$na_dist <- cur_plot
      cur_plot
      })

    # download buttons ------------------------------------------------------------------------------------------------------------------------
    save_plot_server("save_na_per_col", plot_save = reactive({user_plot$na_per_col}), plot_width = app_settings$plot_download_width, plot_height = app_settings$plot_download_height, plot_dpi = app_settings$plot_download_dpi)
    save_plot_server("save_na_comb", plot_save = reactive({user_plot$na_comb}), plot_width = app_settings$plot_download_width, plot_height = app_settings$plot_download_height, plot_dpi = app_settings$plot_download_dpi)
    save_plot_server("save_na_dist", plot_save = reactive({user_plot$na_dist}), plot_width = app_settings$plot_download_width, plot_height = app_settings$plot_download_height, plot_dpi = app_settings$plot_download_dpi)


    })
}
