#' Simplified Crosstabs
#'
#' This function returns a crosstab (tab) on a given data frame, and using simplified calls within the function for two variables, to return the observed and expected frequencies.
#' @import ggplot2 dplyr purrr
#' @importFrom stats chisq.test
#' @param df data frame to read in.
#' @param var1 a first grouping variable.
#' @param var2 a second grouping variable.
#' @return This function returns the observed and expected frequencies of a bivariate relationship between \code{var1} and \code{var2} in data frame \code{df}.

#' @examples
#' data <- mtcars
#'
#' tab(data,mpg,cyl)
#' @export


tab <- function(df, var1, var2){
  #options(warn=-1) # suppress warnings for chi square run
  #v1 <- paste0(substitute(df),"$",substitute(var1)) # how to use $ operator in dataframe$variable
  #v2 <- paste0(substitute(df),"$",substitute(var2)) # how to use $ operator in dataframe$variable
  v1 <- (eval(substitute(var1), df))
  v2 <- (eval(substitute(var2), df))
  crosstab <- chisq.test(v1, v2, correct=FALSE)
  options(warn=0) # unsuppress warnings for chi square
  crosstab$data.name <- paste0(deparse(substitute(var1)), " and ", deparse(substitute(var2)))
  names(dimnames(crosstab$observed)) <- c(deparse(substitute(var1)),deparse(substitute(var2)))
  names(dimnames(crosstab$expected)) <- c(deparse(substitute(var1)),deparse(substitute(var2)))
  #crosstab$observed
  #crosstab$expected
  tab <- list(crosstab$observed, crosstab$expected)
  names(tab) <- c("Observed Frequencies","Expected Frquencies")
  return(tab)
}



