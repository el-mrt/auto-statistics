#' Function that given a vector adjusts formats the datatype in cases that it makes sense.
#'
#' @param vect vector of data
#' @param n number of unique values under below which formating will apply
#'
#' @examples
#' var_format(iris$Species, n = 6)
#' var_format(mtcars$gear, n = 6)
#'
#' @return formated vector
#'
#' @export
#'
#'
#'

var_format <- function(vect, n){
  if (!is.numeric(vect)) {
    return(vect)
  }

  if (length(unique(vect)) >= n) {
    return(vect)
  }

  len <- length(unique(vect))

  c1 <- !any((unique(vect) == 1:len) == FALSE, na.rm = TRUE)
  c2 <- !any((unique(vect) == 0:(len-1)) == FALSE, na.rm = TRUE)

  if (c1 || c2) {
    return(as.ordered(vect))
  } else {
    return(as.factor(vect))
  }
}
