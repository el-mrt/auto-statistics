


report_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2,
             h3(),
             uiOutput(ns("report_type")),
             conditionalPanel("input.report_type == 'descriptive'",
                              tagList(
                                uiOutput(ns("custom_report_features"))
                              ), ns = ns),
             conditionalPanel("input.report_type.includes('descriptive') || input.report_type.includes('ml')",
                              tagList(
                                uiOutput(ns("report_append_custom"))
                              ), ns = ns),
             actionButton(ns("report_generate"), "Generate")
             ),
      column(10,
             h3("Preview Report"),
             htmlOutput(ns("preview_report"))
             )
    )
  )
}


report_server <- function(id, user_data){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # general -----------------------------------------------------------
    output$report_type <- renderUI({
      selectInput(ns("report_type"), label = "report type", multiple = FALSE, selected = "ml",
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
    output$custom_report_features <- renderUI({
      req(user_data())
      selectInput(ns("custom_report_features"), "Select Featues", choices = c("Top5", names(user_data())), multiple = TRUE, selected = "Top5")
    })

    # generate report ---------------------------------------------------------
    observeEvent(input$report_generate, {

      cur_report$type <- NULL
      cur_report$path <- NULL
      if(input$report_type == "custom"){

        message(paste0(getwd()))
        message(paste0(file.path(tempdir(), "temp_report.Rmd")))
      #   # get template and set up temp file path
      #   path_template <- system.file("inst", "shiny", "autoStatistics","www", "rep_templ_custom_html.Rmd", package="autoStatistics")
      #   temp_report <- file.path(tempdir(), "temp_report.Rmd")
      #   # copy file
      #   file.copy(path_template, temp_report, overwrite = TRUE)
      #
      #   temp_report <- rmarkdown::render(input = path_template, output_dir = file.path(tempdir(), "temp_report_html.Rmd"), envir = new.env(parent = globalenv()), params = list(custom_plot = report_plots$custom_report))
      #   cur_report$path <- c(
      #     cur_report$path,
      #     "html" = temp_report)
      }


      })

  })
}
