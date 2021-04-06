#' Simplified Boxplot
#'
#' This function plots a Box-and-Whisker (box) on a given data frame, and uses simplified calls within the function to parse the histogram by up to variables.
#' @importFrom graphics boxplot
#' @param df data frame to read in.
#' @param var1 the variable of interest that should be plotted.
#' @param by1 a grouping variable by which the histogram for \code{var1} should be parsed.
#' @param by2 a potential second grouping variable by which the histogram for \code{var1} (already parsed by \code{by1}) should be parsed.
#' @examples
#' data <- mtcars
#'
#' box(data,mpg,cyl)
#' @export

box <- function(df, var1, by1, by2){
  bygroups <- length(match.call())-3
  if(bygroups==-1) {
    main <- paste0("Boxplot of '", deparse(substitute(df)), "'")
    laby <- deparse(substitute(df))
    boxplot(df, main = main, ylab = laby)
    #boxplot({{ var1 }}, data = df, main = main)
  }
  if(bygroups==0) {
    main <- paste0("Boxplot of '", deparse(substitute(var1)), "'")
    laby <- deparse(substitute(var1))
    boxplot(eval(substitute(var1), df), main = main, ylab = laby) # a way of calling values within #df$var1
    #boxplot({{ var1 }}, data = df, main = main)
  }
  if(bygroups==1) {
    main <- paste0("Boxplot of '", deparse(substitute(var1)),"' by '", deparse(substitute(by1)),"'")
    labx <- deparse(substitute(by1))
    laby <- deparse(substitute(var1))
    boxplot(eval(substitute(var1), df) ~ eval(substitute(by1), df), main = main, xlab = labx, ylab = laby)
  }
  if(bygroups==2) {
    main <- paste0("Boxplot of '", deparse(substitute(var1)),"' by '", deparse(substitute(by1)),"' and '", deparse(substitute(by2)),"'")
    labx2 <- paste0(deparse(substitute(by1))," by ", deparse(substitute(by2)))
    laby2 <- deparse(substitute(var1))
    boxplot(eval(substitute(var1), df) ~ eval(substitute(by1), df) + eval(substitute(by2), df), main = main, xlab = labx2, ylab = laby2)
  }
  #return(p)
}
