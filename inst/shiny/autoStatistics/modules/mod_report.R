report_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      shinybusy::add_busy_spinner(spin = "fading-circle"),
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


# REPORT ------------------------------------------------------------------
    output$descr_report_features <- renderUI({
      req(user_data())
      selectInput(ns("descr_report_features"), "Select Featues", choices = c("Top5" = "top", names(user_data())), multiple = TRUE, selected = "top")
    })

# generate report ---------------------------------------------------------
    output$download_report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = function(){
        paste0("report.html")
        },
      content = function(file) {
        file.copy(cur_report$path, file)
    })

    observeEvent(input$report_generate, {

      cur_report$type <- NULL
      cur_report$path <- NULL

# custom report -----------------------------------------------------------
      if(input$report_type == "custom"){
        req(custom_report_content)
        filename = "report.html"

        path_template <- system.file("shiny", "autoStatistics", "www", "rep_templ_custom_html.Rmd", package="autoStatistics")
        # dev path
        # path_template <- ("./www/rep_templ_custom_html.Rmd")


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
        }
        #req(user_tables$feature_imp)

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
        cor_matrix_obj <- autoStatistics::ReportContent$new(id="cor_matrix", type = "cor_matrix", content = temp_cor_matrix)
        report_content <- autoStatistics::appendList(report_content, cor_matrix_obj, "cor_matrix")


        for(feature in selected_features){
          print(paste0("creating descriptive report for feature: ", feature))
          feature_content <- vector("list", length = 0L)
          # hist####
          temp_hist <- plot_hist_server("plot_hist", data = user_data(), feature = feature, user_color = plot_color_one, user_binwidth = 0.5)
          temp_hist_obj <- autoStatistics::ReportContent$new(id=paste0(feature, "_hist"), type = "ggplot", content = temp_hist)

          feature_content <- autoStatistics::appendList(
            feature_content,
            temp_hist_obj,
            "hist"
          )
          cat("Histogram created \n")
          # scatter target####
          temp_scatter <- plot_scatter_server("plot_scatter", data = user_data(), target_feature = target_column(),
                                              selected_feature = feature, user_color = plot_color_one, point_size = 3)
          temp_scatter_obj <- autoStatistics::ReportContent$new(id = paste0(feature,"_scatter"), type = "ggplot", content = temp_scatter)


          feature_content <- autoStatistics::appendList(
            feature_content,
            temp_scatter_obj,
            "scatter_target"
          )
          cat("Scatter created \n")
          # text NA and feature imp####
          temp_text_na <- autoStatistics::generate_descr_report_text_na(feature = feature, imp_tbl = user_tables$feature_imp, task_obj = user_task$task)
          temp_text_na_obj <- autoStatistics::ReportContent$new(id = paste0(feature, "_textNA"), type = "text", content = temp_text_na)

          feature_content <- autoStatistics::appendList(
            feature_content,
            temp_text_na_obj,
            "na_text"
          )
          cat("temp_text_na_obj created \n")
          # df with stat summary ####
          temp_stats <- autoStatistics::generate_descr_report_tbl_stat(data = user_data(), feature = feature)
          temp_stats_obj <- autoStatistics::ReportContent$new(id = paste0(feature, "_stats"), type = "dataframe", content = temp_stats)

          feature_content <- autoStatistics::appendList(feature_content, temp_stats_obj, "tbl_stat")
          cat("temp_stats_obj created \n")
          # cor matrix and text ####
          temp_cor_text <- autoStatistics::generate_descr_report_cor(temp_cor_matrix, feature)
          print(temp_cor_text)



          temp_cor_text_obj <- autoStatistics::ReportContent$new(id = paste0(feature,"_cor_text"), type = "text", content = temp_cor_text)

          feature_content <- autoStatistics::appendList(feature_content, temp_cor_text_obj, "cor_text")
          cat("temp_cor_text_obj created \n")

          # append to report content ####
          report_content <- autoStatistics::appendList(report_content,feature_content, feature)
        }
        #print(report_content)
        #View(report_content)


        rm(temp_numeric_cols,temp_cor_data,temp_cor_matrix)


        #dev path
        # path_template <- ("./www/rep_templ_descriptive_html.Rmd")

        warning("cashew")
        path_template <- system.file("shiny", "autoStatistics", "www", "rep_templ_descriptive_html.Rmd", package="autoStatistics")



        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy(path_template, tempReport, overwrite = TRUE)

        temp_report <-
          rmarkdown::render(tempReport,"html_document", params = list(
            custom_plot = report_plots$custom_report, content = report_content, append_custom = input$report_append_custom),
            envir = new.env(parent = globalenv()))
        cur_report$type <- "html"
        cur_report$path <- temp_report
        print(cur_report$path)
      }
      else if(input$report_type == "ml"){
        req(results$bmr_result)
        filename = "report.html"

        path_template <- system.file("shiny", "autoStatistics", "www", "rep_templ_ml_html.Rmd", package="autoStatistics")

        #dev path
        #path_template <- ("./www/rep_templ_ml_html.Rmd")

        tempReport <- file.path(tempdir(), "report_descriptive.Rmd")
        file.copy(path_template, tempReport, overwrite = TRUE)

        temp_report <-
          rmarkdown::render(tempReport, "html_document", params = list(bmr_result = results$bmr_result, param_list = results$param_list),envir = new.env(parent = globalenv()))
        cur_report$type <- "html"
        cur_report$path <- temp_report
        print(cur_report$path)
      }
      })

  })
}
