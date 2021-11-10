#' Function to get all combinations of NAs within the rows of a dataset
#'
#' @param x data
#' @param names_col append column with the corresponding column names of the missing combinations
#'
#' @examples
#' missing_combinations(iris, names_col = FALSE)
#'
#' @return `MissComb` object
#'
#' @export

missing_combinations <- function(x, names_col = TRUE){
  # get NA combinations
  row_na_comb <- apply(x, 1, function(x){
    paste(unname(which(is.na(x))), collapse = ",")
  })
  # summarise with table()
  row_na_comb <- as.data.frame(table(row_na_comb))
  # drop rows without NA
  row_na_comb <- row_na_comb[row_na_comb[, 1] != "", ]
  colnames(row_na_comb) <- c("index", "freq")
  row_na_comb[["index"]] <- as.character(row_na_comb[["index"]])

  # generate names
  if(names_col){
    col_names <- colnames(x)
    temp <- strsplit(row_na_comb[["index"]], split = ",")
    row_na_comb[["name"]] <- sapply(temp, simplify = FALSE, function(x){
      paste(col_names[as.numeric(x)], collapse = ", ")
    })
  }
  # order by freq
  row_na_comb <- row_na_comb[order(-row_na_comb[["freq"]]), ]
  # return
  return_object <- MissComb$new(na_comb = row_na_comb)
  return(return_object)
}



#' R6 Class for missing combinations
#' The class has the field for the combinations and custom plot function
#' @importFrom R6 R6Class
#' @importFrom dplyr top_n
#' @importFrom stringr str_replace_all
#' @import ggplot2
#'
MissComb <- R6::R6Class("MissComb", list(
  #' @field na_combinations dataframe with missing combinations
  na_combinations = NA,
  #' @description
  #' Creates a new MissComb Object
  #' @param na_comb dataframe with missing combinations
  initialize = function(na_comb = NA){
    self$na_combinations <- na_comb
  },
  #' @description
  #' custom plot function for the Class
  #' @param labels which labels should be used. options: be `index` or `name`. `index` results in a much shorter length usually
  #' @param label_length after how many characters the line should be splitted
  #' @param show_numbers if `TRUE` shows the numbers of observations next to the bar.
  #' @param text_offset defines the offset of the text to the bar
  #' @param top_n show the `top_n` cases
  #' @param bar_color defines the color of the bars
  #' @param plot_title title of the plot
  #' @param x_lab x axis label
  #' @param y_lab y axis label
  plot = function(labels = "index", label_length = 30, show_numbers = TRUE, text_offset = 50, top_n = 10, bar_color = "#FC8D62",
                  plot_title = "NA combinations", x_lab = "combination", y_lab = "n"){
    # split labels by length to avoid too long row names
    self$na_combinations[["labels"]] <- stringr::str_replace_all(
      self$na_combinations[[{{ labels }}]], paste0("(.{",label_length,"})"), "\\1\n")
    # plot
    return(
      ggplot2::ggplot(data = dplyr::top_n(self$na_combinations, top_n, freq), aes(x = reorder(labels, freq), freq)) +
        geom_bar(stat = "identity", fill = bar_color) +
        {if(show_numbers) geom_text(aes(label=freq, y = freq + text_offset), position = position_identity())} +
        labs(title = plot_title, x = x_lab, y = y_lab) +
        coord_flip() +
        theme_minimal()
    )
  }
))
