#' Simplified Correlation
#'
#' This function simplifies the call for Pearson's Product-Moment Correlation Coefficient (p.corr) on a given data frame.
#' @importFrom stats cor.test
#' @param df data frame to read in.
#' @param var1 the dependent/outcome variable, \eqn{Y}.
#' @param var2 the main independent/predictor variable, \eqn{X}.
#' @return This function returns the summary results table for a Pearson's correlation, examining the relationship between \code{var1} from data frame \code{df}, and \code{var2}.
#' @examples
#' data <- mtcars
#'
#' p.corr(data,mpg,wt)
#' @export

p.corr <- function(df, var1, var2){
  #options(scipen=999)
  model <- cor.test(eval(substitute(var1), df), eval(substitute(var2), df))
  t <- model$statistic
  model$statistic <- model$estimate
  names(model$statistic) <- "r"
  model$estimate <- t
  model$data.name <- paste0(deparse(substitute(var1))," and ", deparse(substitute(var2)))
  return(model)
}
