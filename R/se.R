#' Standard Error Calculation
#'
#' This function calculates the standard error for a variable in a given data frame.
#' @importFrom gdata nobs
#' @importFrom stats qt sd
#' @param var variable to read in.
#' @param na.rm logical (default set to \code{T}). When set to \code{na.rm = F}, will include NA's in calculation.
#' @return This function returns the standard error for a given variable
#' @examples
#' data <- mtcars
#'
#' se(data$mpg)
#' @export


se <- function (var, na.rm = TRUE){
  stderror <- sd(var, na.rm = na.rm)/sqrt(nobs(var))
  if(na.rm == FALSE){
    stderror <- sd(var, na.rm = FALSE)/sqrt(nobs(var))
  }
  out <- stderror

  return(out)
}

