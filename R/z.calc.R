#' Simplified Z Scores
#'
#' This function calculates the Z score for a given value, relative to the mean and standard deviation for a variable in a given data frame.
#' @importFrom stats sd
#' @param df data frame to read in.
#' @param var1 the variable of interest for which the mean and standard deviations will be calculated.
#' @param x the desired raw score to compare with the mean and standard deviation of \code{var1}.
#' @return This function returns the raw score, mean, and z-score for a given raw score.
#' @examples
#' data <- mtcars
#'
#' z.calc(data,mpg,12)
#' @export


z.calc <- function (df, var1, x){
  calls <- length(match.call())-3
  if(calls==0){
    newx <- gsub("\\s*\\([^\\)]+\\)","",as.character(match.call()[3]))
    newx <- as.numeric(newx)
    #print(as.character(match.call()[3]))
    xbar <- mean(df, na.rm = TRUE)
    sd <- sd(df, na.rm = TRUE)
    z <- (newx - xbar)/(sd)
    out <- c(`Raw Score` = newx, `Mean` = xbar, `Z Score` = z)
  }
  else {
    newx <- gsub("\\s*\\([^\\)]+\\)","",as.character(match.call()[4]))
    newx <- as.numeric(newx)
    #CI(eval(substitute(var1), df), ci=cl)
    xbar <- mean(eval(substitute(var1), df), na.rm = TRUE)
    sd <- sd(eval(substitute(var1), df), na.rm = TRUE)
    z <- (newx - xbar)/(sd)
    out <- c(`Raw Score` = newx, `Mean` = xbar, `Z Score` = z)
  }
  return(out)
}

