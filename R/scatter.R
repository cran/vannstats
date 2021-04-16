#' Simplified Scatterplot
#'
#' This function plots a scatterplot (scatter) on a given data frame, and adds a fit-line to the data.
#' @importFrom graphics abline plot
#' @importFrom stats lm
#' @param df data frame to read in.
#' @param var1 the dependent/outcome variable, \eqn{Y}.
#' @param var2 the independent/predictor variable, \eqn{X}.
#' @examples
#' data <- mtcars
#'
#' scatter(data,mpg,wt)
#' @export

scatter <- function(df, var1, var2){
  #bygroups <- length(match.call())-3
  #if(bygroups==0) {
  #  main <- paste0("Boxplot of '", deparse(substitute(var1)), "'")
  #  laby <- deparse(substitute(var1))
  #  boxplot(eval(substitute(var1), df), main = main, ylab = laby) # a way of calling values within #df$var1
  #  #boxplot({{ var1 }}, data = df, main = main)
  #}
  #if(bygroups==1) {
  #  main <- paste0("Boxplot of '", deparse(substitute(var1)),"' by '", deparse(substitute(var2)),"'")
  #  labx <- deparse(substitute(var2))
  #  laby <- deparse(substitute(var1))
  #  boxplot(eval(substitute(var1), df) ~ eval(substitute(var2), df), main = main, xlab = labx, ylab = laby)
  #}
  #if(bygroups==2) {
  #  main <- paste0("Boxplot of '", deparse(substitute(var1)),"' by '", deparse(substitute(var2)),"' and '", deparse(substitute(by2)),"'")
  #  labx2 <- paste0(deparse(substitute(var2))," by ", deparse(substitute(by2)))
  #  laby2 <- deparse(substitute(var1))
  #  boxplot(eval(substitute(var1), df) ~ eval(substitute(var2), df) + eval(substitute(by2), df), main = main, xlab = labx2, ylab = laby2)
  #}
  main <- paste0("Scatterplot of '", deparse(substitute(var1)),"' and '", deparse(substitute(var2)),"'")
  labx <- deparse(substitute(var2))
  laby <- deparse(substitute(var1))
  plot(eval(substitute(var2), df), eval(substitute(var1), df), main = main, xlab = labx, ylab = laby)
  abline(lm(eval(substitute(var1), df)~eval(substitute(var2), df)), col="Blue")
  #return(p)
}
