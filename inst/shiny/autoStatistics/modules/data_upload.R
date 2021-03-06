cat("data... called\n")


# data_upload -------------------------------------------------------------------------------------------------------------------------


data_upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      actionButton(ns("btn_reset_data"), "Reset Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; margin-left:15px;"),
      actionButton(ns("btn_reset_session"), "Reset Session", style =  "color: #fff; background-color: #d41313; border-color: #d41313; margin-left:15px;")
      #verbatimTextOutput(ns("session_id"))
    ),
    fluidRow(
      uiOutput(ns("error_message_upload"))
    ),
    fluidRow(
      column(width = 2,
             fileInput(ns("file"), "Upload Data"),
             selectInput(ns("encoding"), "File encoding", choices = c("Default" = "", "Latin1" = "Latin1", "UTF-8" = "UTF-8", "UCS-2LE" = "UCS-2LE", "UTF-16LE" = "UTF-16LE", "German_Germany.1252" = "German_Germany.1252"), selected = ""),
             textInput(ns("sep"), "Seperator", value = ","),
             checkboxInput(ns("header"), "Has Header", value = TRUE),
             textInput(ns("NA_string"), "NA string", value = "NA"),
             textInput(ns("dec_symbol"), "Decimal symbol", value = "."),
             uiOutput(ns("target_col")),
             textInput(ns("task_name"), "Task name", "task", )
      ),
      column(width = 10,
             DTOutput(ns("table")))
      ),
    fluidRow(hr()),
    fluidRow(
      column(width = 2,
             h4("Factors"),
             uiOutput(ns("fct_threshold")),
             uiOutput(ns("fct_cols"))
             ),
      column(width = 10,
             fluidRow(
               h4(""),
                 column(8,
                        conditionalPanel(condition = "output.warn_fct_col_active == true",
                                         verbatimTextOutput(ns("warn_fct_col"), placeholder = FALSE),
                                         actionButton(ns("btn_warn_fct_cont"), "", icon = icon("glyphicon glyphicon-ok", lib = "glyphicon")),
                                         actionButton(ns("btn_warn_fct_discard"), "", icon = icon("glyphicon glyphicon-remove", lib = "glyphicon")),
                                         ns = ns)
                                         )),
             )
      )
  )
}


data_upload_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # upload data----
    observeEvent(input$file, {
      target_column(NULL)
      tryCatch(
        {
          user_file(input$file$datapath)
          autoStatistics::debug_console(sprintf("new file uploaded: %s", input$file$name))
        },
        error=function(cond){
          output$error_message_upload <- renderUI({autoStatistics::render_error("UPLOAD", cond)})
        }
      )

    })
    # read data into dataframe----
    observeEvent(c(input$file,input$sep,input$header,input$NA_string,input$dec_symbol, input$encoding, input$btn_reset_data, input$fct_threshold), {
      req(user_file())
      tryCatch(
        {
          temp <- switch(tools::file_ext(user_file()),
                         txt = read.table(file = user_file(), header = input$header, sep = input$sep,
                                          na.strings = input$NA_string, dec = input$dec_symbol, fileEncoding = input$encoding),
                         csv = read.csv(file = user_file(), header = input$header, sep = input$sep,
                                        na.strings = input$NA_string, dec = input$dec_symbol, fileEncoding = input$encoding))
          colnames(temp) <- unlist(lapply(names(temp), function(x) stringi::stri_trans_general(x, id = "Latin-ASCII")))


          user_data(temp)
          autoStatistics::debug_console(sprintf("Data loaded into a dataframe"))

        },
        error = function(cond){
          output$error_message_upload <- renderUI({autoStatistics::render_error("FILETOTABLE", cond)})
        },
        warn = function(cond){
          output$error_message_upload <- renderUI({autoStatistics::render_error("FILETOTABLE", cond)})
        }
      )
      req(user_data())
    # detect factor columns
      tryCatch(
        {
          col_types <- lapply(names(user_data()), function(col_name){
            autoStatistics::identify_CR(user_data(), col_name, input$fct_threshold)
          })
          col_types <- as.vector(unlist(col_types))
          factor_col_index <- which(col_types == "classif")
          factor_columns(names(user_data()[factor_col_index]))
          # update columns from numeric to factors
          temp_data <- user_data()
          for(col in factor_columns()){
            temp_data[[{{ col }}]] <- as.factor(temp_data[[{{ col }}]])
          }
          user_data(temp_data)

        }, error = function(cond){
          message(paste0("ERROR HERE 2:  ", cond))
        }
      )
    })
    # render DT----
    output$table <- renderDT({
      tryCatch(
        {
          validate(need(user_data(), message = "upload your data"))
          user_data()
        }, error=function(cond){
          message(paste0("ERROR while render DT:  ", cond))
        }, warning=function(cond){
          message(paste0("WARN while render DT:  ", cond))
        }
      )

      })

    # select target column and create task----
    output$target_col <- renderUI({
      if(is.null(target_column())){target_column(names(user_data())[1])}
      selectInput(ns("target_col"), "Select target feature", names(user_data()), selected = target_column())
    })
    observeEvent(input$target_col, {
      req(user_data())
      target_column(input$target_col) # update target column

      # detect type of task
      tryCatch({
        user_task$type <- autoStatistics::identify_CR(user_data(), target_column())
        #task_type(autoStatistics::identify_CR(user_data(), target_column()))
        autoStatistics::debug_console(sprintf("new task type detected: %s", user_task$type))
      }, error=function(cond){
        message(paste("error while detect the task type while observing the data inputs: ", cond))
      })
      # create task
      tryCatch(
        {
          req(user_task$type)
          req(user_data())
          temp_data <- user_data()
          temp_data <- temp_data[!is.na(temp_data[[{{ target_column() }}]]), ]

          user_task$task <- autoStatistics::create_task(temp_data, target_column(), user_task$type, task_name = input$task_name)
          autoStatistics::debug_console(sprintf("new task created with type: %s", user_task$type))
          print(user_task$task)
        },
        error=function(cond){
          message(paste("Error while creating  while observing the data inputs: ", cond))
        }
      )
    })

    # select factor cols----
    output$fct_cols <- renderUI({
      req(user_data())
      selectInput(ns("fct_cols"), "Select factor columns", choices = names(user_data()), multiple = TRUE, selected = factor_columns())
    })
    output$fct_threshold <- renderUI({
      numericInput(ns("fct_threshold"), "Threshold", 6, 2, 100, 1)
    })

    observeEvent(input$fct_cols, {
        if(length(setdiff(factor_columns(), input$fct_cols)) > 0 ||
           length(setdiff(input$fct_cols, factor_columns())) > 0){

          tryCatch(
            {
              temp_fct_updated <- autoStatistics::update_factor_cols(user_data(), old_cols = factor_columns(), new_cols = input$fct_cols)
              user_data(temp_fct_updated$data)
              # update factor columns
              factor_columns(autoStatistics::factor_col_names(user_data()))
              #factor_columns(temp_fct_updated$new_factors_names)
              # get task type
              tryCatch({
                user_task$type <- autoStatistics::identify_CR(user_data(), target_column())
                autoStatistics::debug_console(sprintf("new task type detected: %s", user_task$type))
              }, error=function(cond){
                message(paste("error while detect the task type when updating factor columns: ", cond))
              })
              # create task
              tryCatch(
                {
                  temp_data <- user_data()
                  temp_data <- temp_data[!is.na(temp_data[[{{ target_column() }}]]), ]

                  user_task$task <- autoStatistics::create_task(temp_data, target_column(), user_task$type)
                  autoStatistics::debug_console(sprintf("new task created with type: %s", user_task$type))
                  print(user_task$task)
                },
                error=function(cond){
                  message(paste("Error while creating  when updating factor columns: ", cond))
                }
              )
              },
            error = function(cond){
              message(cond)
            },
            warning = function(cond){
              fct_col_warn$is_active <- TRUE
              message(paste0("WARN WHILE TRANSFORMING TO FCT COL: ", cond, "...", fct_col_warn$is_active))
              updateSelectInput(session, inputId = "fct_cols", label = "select factor column", choices = names(user_data()), selected = factor_columns())
              fct_col_warn$col_name <- c(setdiff(factor_columns(), input$fct_cols), setdiff(input$fct_cols, factor_columns()))
              temp_data <- user_data()
              temp_data <- temp_data[[{{fct_col_warn$col_name}}]]
              # get numbers of new NAs
              n_new_na <- sum(is.na(as.numeric(levels(temp_data))[temp_data]))
              n_old_na <-sum(is.na(temp_data))
              n_na <- abs(n_new_na - n_old_na)
              fct_col_warn$text <- paste0("Converting the column '", fct_col_warn$col_name, "' would insert ", n_na, " new NAs to the column. Continue?")

            }
          )
        }

    })
    # warn text----
    # track if active
    output$warn_fct_col_active <- reactive({
      print(paste0("UPDATED: ", fct_col_warn$is_active))
      return(fct_col_warn$is_active)
    })
    outputOptions(output, "warn_fct_col_active", suspendWhenHidden = FALSE)

    output$warn_fct_col <- renderText({print(fct_col_warn$text)})
    observeEvent(input$btn_warn_fct_cont,{
      req(fct_col_warn$col_name)
      temp_data <- user_data()
      temp_data[[{{ fct_col_warn$col_name }}]] <- as.numeric(levels(temp_data[[{{ fct_col_warn$col_name }}]]))[temp_data[[{{ fct_col_warn$col_name }}]]]
      user_data(temp_data)
      factor_columns(autoStatistics::factor_col_names(user_data()))
      fct_col_warn$text <- ""
      fct_col_warn$is_active <- FALSE
      # get task type
      tryCatch({
        user_task$type <- autoStatistics::identify_CR(user_data(), target_column())
        autoStatistics::debug_console(sprintf("new task type detected: %s", user_task$type))
      }, error=function(cond){
        message(paste("error while detect the task type when updating factor columns after the warning: ", cond))
      })
      # create task
      tryCatch(
        {
          temp_data <- user_data()
          temp_data <- temp_data[!is.na(temp_data[[{{ target_column() }}]]), ]

          user_task$task <- autoStatistics::create_task(temp_data, target_column(), user_task$type)
          autoStatistics::debug_console(sprintf("new task created with type: %s", user_task$type))
          print(user_task$task)
        },
        error=function(cond){
          message(paste("Error while creating  when updating factor columns after the warning: ", cond))
        }
      )
    })
    observeEvent(input$btn_warn_fct_discard,{
      fct_col_warn$text <- ""
      fct_col_warn$is_active <- FALSE

    })
    # reset session ----
    observeEvent(input$btn_reset_session,{
      session$reload()
      global_path <- system.file("shiny", "autoStatistics", package = "autoStatistics")
      print(paste0(global_path, "/global.R"))
      source(paste0(global_path, "/global.R"))

    })
})
}


