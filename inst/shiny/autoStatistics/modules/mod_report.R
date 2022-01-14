


report_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2,
             h3(),
             uiOutput(ns("report_type")),
             conditionalPanel("input.report_type == 'descriptive'",
                              tagList(
                                uiOutput(ns("descr_report_features"))
                              ), ns = ns),
             conditionalPanel("input.report_type.includes('descriptive') || input.report_type.includes('ml')",
                              tagList(
                                uiOutput(ns("report_append_custom"))
                              ), ns = ns),
             actionButton(ns("report_generate"), "Generate"),
             downloadButton(ns("download_report"), "Download")
             ),
      column(10,
             h3("Preview Report"),
             htmlOutput(ns("preview_report")),
             )
    )
  )
}


report_server <- function(id, user_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # general -----------------------------------------------------------
    output$report_type <- renderUI({
      selectInput(ns("report_type"), label = "report type", multiple = FALSE, selected = "custom",
                  choices = c("Custom" = "custom", "Machine Learning" = "ml", "Descriptive" = "descriptive"))
    })
    observeEvent(input$report_type, {
      report_settings$type <- input$report_type
    })

    output$report_append_custom <- renderUI({
      checkboxInput(ns("report_append_custom"), "Append custom report", FALSE)
    })
    observeEvent(input$report_append_custom, {
      req(user_data())
      report_settings$append_custom <- input$report_append_custom
    })
    output$preview_report <- renderUI({
      req(cur_report$path)
      includeHTML(cur_report$path)

    })

# custom report -----------------------------------------------------------
    output$descr_report_features <- renderUI({
      req(user_data())
      selectInput(ns("descr_report_features"), "Select Featues", choices = c("Top5" = "top", names(user_data())), multiple = TRUE, selected = "top")
    })

    # generate report ---------------------------------------------------------
    output$download_report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {

        #path_template <- system.file("shiny", "autoStatistics", "www", "rep_templ_custom_html.Rmd", package="autoStatistics")
        #dev path
        path_template <- ("./www/rep_templ_custom_html.Rmd")
        print(path_template)

        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(path_template, tempReport, overwrite = TRUE)


        rmarkdown::render(tempReport, output_file = file,
                          params = list(custom_plot = report_plots$custom_report),
                          envir = new.env(parent = globalenv()))
    })

    observeEvent(input$report_generate, {

      cur_report$type <- NULL
      cur_report$path <- NULL
      if(input$report_type == "custom"){
        filename = "report.html"

        #path_template <- system.file("shiny", "autoStatistics", "www", "rep_templ_custom_html.Rmd", package="autoStatistics")
        #dev path
        path_template <- ("./www/rep_templ_custom_html.Rmd")


        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(path_template, tempReport, overwrite = TRUE)

        temp_report <-
          rmarkdown::render(tempReport,params = list(custom_plot = report_plots$custom_report),envir = new.env(parent = globalenv()))
        cur_report$type <- "html"
        cur_report$path <- temp_report
        print(cur_report$path)

      }

# descriptive report ------------------------------------------------------
      else if(input$report_type == "descriptive"){
        # check if feature imp already calculated if top n is in descr_report_features
        selected_features <- input$descr_report_features

        if(c("top") %in% selected_features){
          if(is.null(user_tables$feature_imp)){
            tryCatch({
              importance_table <- autoStatistics::feature_importance(task = user_task$task, filters = pre_feature_import_filter)
            }, error = function(cond){
              message(paste0(cond))
            })
            # get NAs per column
            df_na_per_col <- sapply(isolate(user_data()), function(x) sum(is.na(x)))
            # transform to df
            df_na_per_col <- data.frame("feature" = names(df_na_per_col), "NAs" = unname(df_na_per_col))
            # merge into one dataframe
            importance_table <- importance_table %>%
              dplyr::left_join(df_na_per_col, by = "feature") %>%
              dplyr::arrange(mean)

            user_tables$feature_imp <- importance_table
          }
          # add top5 to selected features
          if(nrow(user_tables$feature_imp) < 5){
            selected_features <- c(selected_features, user_tables$feature_imp[["feature"]])
          }else{
            selected_features <- c(selected_features, user_tables$feature_imp[["feature"]][c(1:5)])
          }
          selected_features <- selected_features[!selected_features %in% c("top")]
          selected_features <- unique(selected_features)
          print(selected_features)
        }
        req(user_tables$feature_imp)

        # generate plots for all the selected features
        report_content <- vector(mode = "list", length = 0L)
        plot_color_one <- RColorBrewer::brewer.pal(n = 3, name = app_settings$plot_color_set)[1]

        #cor matrix ####
        temp_numeric_cols <- sapply(user_data(), function(x){
          if(is.numeric(x) & (!is.factor(x))){
            return(TRUE)
          }else{
            return(FALSE)
          }
        })
        temp_cor_data <- user_data()[, temp_numeric_cols]
        temp_cor_matrix <- cor(temp_cor_data, use = "pairwise.complete.obs")
        report_content <- autoStatistics::appendList(report_content,temp_cor_matrix, "cor_matrix")


        for(feature in selected_features){
          print(paste0("creating descriptive report for feature: ", feature))
          feature_content <- vector("list", length = 0L)
          # hist####
          temp_hist <- plot_hist_server("plot_hist", data = user_data(), feature = feature, user_color = plot_color_one, user_binwidth = start_bin_width,
                                        check_bin_width=FALSE)


          feature_content <- autoStatistics::appendList(
            feature_content,
            temp_hist,
            "hist"
          )
          # scatter target####
          feature_content <- autoStatistics::appendList(
            feature_content,
            plot_scatter_server("plot_scatter", data = user_data(), target_feature = target_column(),
                                selected_feature = feature, user_color = plot_color_one, point_size = 3),
            "scatter_target"
          )
          # text NA and feature imp####
          feature_content <- autoStatistics::appendList(
            feature_content,
            autoStatistics::generate_descr_report_text_na(feature = feature, imp_tbl = user_tables$feature_imp, task_obj = user_task$task),
            "na_text"
          )
          # df with stat summary ####
          feature_content <- autoStatistics::appendList(
            feature_content,
            autoStatistics::generate_descr_report_tbl_stat(data = user_data(), feature = feature),
            "tbl_stat"
          )

          # cor matrix and text ####
          feature_content <- autoStatistics::appendList(
            feature_content,
            autoStatistics::generate_descr_report_cor(temp_cor_matrix, feature),
            "cor_text"
          )
          # append to report content ####
          report_content <- autoStatistics::appendList(report_content,feature_content, feature)
        }
        #print(report_content)
        View(report_content)
        rm(temp_numeric_cols,temp_cor_data,temp_cor_matrix)



        filename = "report.html"
        #path_template <- system.file("shiny", "autoStatistics", "www", "rep_templ_custom_html.Rmd", package="autoStatistics")
        #dev path
        path_template <- ("./www/rep_templ_descriptive_html.Rmd")


        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(path_template, tempReport, overwrite = TRUE)

        temp_report <-
          rmarkdown::render(tempReport,params = list(custom_plot = report_plots$custom_report, content = report_content),envir = new.env(parent = globalenv()))
        cur_report$type <- "html"
        cur_report$path <- temp_report
        print(cur_report$path)



      }
      else if(input$report_type == "ml"){
        print("ML REPORT HERE")
      }





      })

  })
}
