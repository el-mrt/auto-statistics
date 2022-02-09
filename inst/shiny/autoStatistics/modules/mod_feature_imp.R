

feature_importance_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(8, style = "margin-left:15px;",
             h3("Feature Importance")
             ),
      column(1,
             shinybusy::add_busy_spinner(spin = "fading-circle")
             )
      ),
    fluidRow(
      column(3, style = "margin-left:15px;",
             actionButton(ns("start"), "Start")
             ),
      column(8,
             DTOutput(ns("table"))
             )
    ),
    fluidRow(save_table_ui(ns("save_feature_imp"))),
  )
}


feature_importance_server <- function(id, user_task, user_filters, store_output = TRUE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # start btn
    observeEvent(input$start, {
      req(user_task)
      shinybusy::show_spinner() # show the spinner

      tryCatch({
        importance_table <- autoStatistics::feature_importance(task = user_task, filters = user_filters, ranks = TRUE)
      }, error = function(cond){
        message(paste0(cond))
      })
      # get NAs per column
      df_na_per_col <- sapply(isolate(user_data()), function(x) sum(is.na(x)))
      # transform to df
      df_na_per_col <- data.frame("feature" = names(df_na_per_col), "NAs" = unname(df_na_per_col))
      # merge into one dataframe
      importance_table <- importance_table %>%
        dplyr::left_join(df_na_per_col, by = "feature")
      cur_plot <- NULL
      if(store_output)
        user_tables$feature_imp <- importance_table[order(importance_table[["mean"]]), ]
      else
        cur_plot <- importance_table[order(importance_table[["mean"]]), ]



      output$table <- renderDT({
        if(store_output){
          req(user_tables$feature_imp)
          return(user_tables$feature_imp)
        }else{
          req(cur_plot)
          return(cur_plot)
        }
      },
        options = list(pageLength = 100, lengthChange = FALSE, searching = FALSE), rownames= FALSE, selection = c("none"))
      shinybusy::hide_spinner()
    })
    save_table_server("save_feature_imp", report = "custom", tbl = user_tables$feature_imp, tbl_name = "feature_imp")
  })
}
