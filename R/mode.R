#' Mode Function
#'
#' This function returns the mode for a given data frame.
#' @param x variable within data frame or a list of values.
#' @param na.rm remove the NAs, default is FALSE.
#' @return This function returns the mode for a variable within a data frame or a list of values.
#' @examples
#'
#' data <- mtcars
#'
#' mode(data$mpg)
#' @export

mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }

  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}


