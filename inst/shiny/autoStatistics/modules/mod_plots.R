# HIST --------------------------------------------------------------------
plot_hist_ui <- function(id){
  ns <- NS(id)

}

plot_hist_server <- function(id, data = NULL, feature = NULL, user_color = NULL, user_binwidth =0.5){
  moduleServer(id, function(input, output, session){
    req(data, feature, user_color)
    temp_data_hist <- data[!is.na(data[[{{ feature }}]]), ]


    cur_plot <- ggplot(temp_data_hist) +
      {
        if(is.factor(temp_data_hist[[{{ feature }}]]))
          geom_histogram(aes_string(x = feature, fill = TRUE), stat = "count")
        else
          geom_histogram(aes_string(x = feature, fill = TRUE), binwidth = user_binwidth)
      }+
      scale_fill_manual(values = user_color) +
      labs(x = feature) +
      ggtitle(paste0("distribution ", feature)) +
      theme_minimal() +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

    tryCatch(print(cur_plot),
             error = function(cond){
               message(paste0(cond))
             }, warning = function(cond){
               message(paste0("WARNING HISTOGRAM: ", cond))
               cur_plot <<- ggplot(temp_data_hist) +
                 {
                   if(is.factor(temp_data_hist[[{{ feature }}]]))
                     geom_histogram(aes_string(x = feature, fill = TRUE), stat = "count")
                   else
                     geom_histogram(aes_string(x = feature, fill = TRUE), bins=50)
                 }+
                 scale_fill_manual(values = user_color) +
                 labs(x = feature) +
                 ggtitle(paste0("distribution ", feature)) +
                 theme_minimal() +
                 theme(legend.position = "none", plot.title = element_text(hjust = 0.5))
             })

    return(cur_plot)
  })
}


# SCATTER -----------------------------------------------------------------

plot_scatter_ui <- function(id){
  ns <- NS(id)

}

plot_scatter_server <- function(id, data, target_feature, selected_feature, user_color, point_size, jitter=FALSE){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    req(data, target_feature, selected_feature, user_color, point_size)
    x_feature <- data[[{{ target_feature }}]]
    y_feature <- data[[{{ selected_feature }}]]
    if(!jitter){
      cur_plot <- ggplot(data) +
        geom_point(aes(x_feature, y_feature), color = user_color, size = point_size) +
        labs(x = target_feature, y = selected_feature) +
        ggtitle(paste0("Relationship ", target_feature, " and ", selected_feature))+
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    }else{
      cur_plot <- ggplot(data) +
        geom_jitter(aes(x_feature, y_feature), color = user_color, size = point_size) +
        labs(x = target_feature, y = selected_feature) +
        ggtitle(paste0("Relationship ", target_feature, " and ", selected_feature))+
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5))
    }



    return(cur_plot)
  })
}


# stat cummary ------------------------------------------------------------

stat_summary_ui <- function(id){
  ns <- NS(id)
}

stat_summary_server <- function(id, data, feature){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    req(data, feature)

    return(summary(data[[{{ feature }}]]))
  })
}


# CORPLOT -----------------------------------------------------------------
plot_cor_ui <- function(id){
  ns <- NS(id)
}

plot_cor_server <- function(id, data, method, type){
  moduleServer(id, function(input, output, session){
    req(data, method, type)

    numeric_cols <- sapply(user_data(), function(x){
      if(is.numeric(x) & (!is.factor(x))){
        return(TRUE)
      }else{
        return(FALSE)
      }
    })
    cor_data <- data[, numeric_cols]
    cor_matrix <- cor(cor_data, use = "pairwise.complete.obs")

    cur_plot <- corrplot::corrplot(cor_matrix, method = method, type = type)
    return(cur_plot)
  })
}
