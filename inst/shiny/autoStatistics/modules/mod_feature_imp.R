

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
    )
  )


}


feature_importance_server <- function(id, task, filters){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # start btn
    observeEvent(input$start, {
      shinybusy::show_spinner() # show the spinner

      tryCatch({
        importance_table <- autoStatistics::feature_importance(task = task, filters = filters, ranks = TRUE)
      }, warning = function(cond){
        message(paste0(cond))
      }, error = function(cond){
        message(paste0(cond))
      })



      output$table <- renderDT({
        # get NAs per column
        df_na_per_col <- sapply(isolate(user_data()), function(x) sum(is.na(x)))
        # transform to df
        df_na_per_col <- data.frame("feature" = names(df_na_per_col), "NAs" = unname(df_na_per_col))
        # merge into one dataframe
        importance_table <- importance_table %>%
          dplyr::left_join(df_na_per_col, by = "feature")

        importance_table[order(importance_table[["mean"]]), ]
        },
        options = list(pageLength = 100, lengthChange = FALSE, searching = FALSE), rownames= FALSE, selection = c("none"))
      shinybusy::hide_spinner()
    })


  })
}
