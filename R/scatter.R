#' Simplified Scatterplot
#'
#' This function plots a scatterplot (scatter) on a given data frame, and adds a fit-line to the data.
#' @importFrom graphics abline plot text
#' @importFrom stats lm
#' @param df data frame to read in.
#' @param var1 the dependent/outcome variable, \eqn{Y}.
#' @param var2 the independent/predictor variable, \eqn{X}.
#' @param lab logical (default set to \code{FALSE}). When set to \code{lab = TRUE}, will add Pearson's correlation coefficient (\eqn{r}) value to the plot.
#' @examples
#' data <- mtcars
#'
#' scatter(data,mpg,wt)
#' @export

scatter <- function(df, var1, var2, lab = FALSE){
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
  ycoord <- ( max(eval(substitute(var1), df), na.rm = T) - min(eval(substitute(var1), df), na.rm = T) ) * .80
  xcoord <- ( max(eval(substitute(var2), df), na.rm = T) - min(eval(substitute(var2), df), na.rm = T) ) * .80
  #print(ycoord)
  #print(xcoord)
  model <- cor.test(eval(substitute(var1), df), eval(substitute(var2), df))
  r_val <- model$estimate[[1]]
  r_val_round <- round(r_val, 4)
  r_text <- "\u0072" #"\u0072\U00B2" #U1D493
  r_text2 <- paste0(r_text, " = ", r_val_round)
  plot(eval(substitute(var2), df), eval(substitute(var1), df), main = main, xlab = labx, ylab = laby)
  abline(lm(eval(substitute(var1), df)~eval(substitute(var2), df)), col="Blue")
  if(lab == TRUE){
    text(xcoord, ycoord, r_text2, cex = 1.35, col = "red")
  }
  #return(p)
}
