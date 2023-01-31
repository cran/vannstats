#' Simplified Z Scores
#'
#' This function calculates the Z score for a given value, relative to the mean and standard deviation for a variable in a given data frame.
#' @importFrom stats sd pnorm
#' @param df data frame to read in.
#' @param var1 the variable of interest for which the mean and standard deviations will be calculated.
#' @param x the desired raw score to compare with the mean and standard deviation of \code{var1}.
#' @param tails to report a p-value (level of significance) for the reported Z score, user must select a desired number of tails (either \code{tails = 1} for a one-tailed test, or \code{tails = 2} for a two-tailed test). Default set to \code{NULL}, and does not report a p-value.
#' @return This function returns the raw score, mean, and z-score for a given raw score.
#' @examples
#' data <- mtcars
#'
#' z.calc(data,mpg,12)
#' @export


z.calc <- function (df, var1, x, tails = NULL){
  calls <- length(match.call())-3
  if(calls==0){
    newx <- gsub("\\s*\\([^\\)]+\\)","",as.character(match.call()[3]))
    newx <- as.numeric(newx)
    #print(as.character(match.call()[3]))
    xbar <- mean(df, na.rm = TRUE)
    sd <- sd(df, na.rm = TRUE)
    z <- (newx - xbar)/(sd)
    if(is.null(tails)){
      out <- c(`Raw Score` = newx, `Mean` = xbar, `Z Score` = z)
    }
    else {
      if(tails==1){
        pval <- ((2*pnorm(z, mean = xbar, sd = sd, lower.tail = TRUE))/2)
        out <- c(`Raw Score` = newx, `Mean` = xbar, `Z Score` = z, `p-value (1-tailed)` = pval)
      }
      if(tails==2){
        pval <- (2*pnorm(z, mean = xbar, sd = sd, lower.tail = TRUE))
        out <- c(`Raw Score` = newx, `Mean` = xbar, `Z Score` = z, `p-value (2-tailed)` = pval)
      }
      if(!((tails == 1) || (tails == 2))){
        out <- c(`Raw Score` = newx, `Mean` = xbar, `Z Score` = z)
      }
    }
  }
  else {
    newx <- gsub("\\s*\\([^\\)]+\\)","",as.character(match.call()[4]))
    newx <- as.numeric(newx)
    #CI(eval(substitute(var1), df), ci=cl)
    xbar <- mean(eval(substitute(var1), df), na.rm = TRUE)
    sd <- sd(eval(substitute(var1), df), na.rm = TRUE)
    z <- (newx - xbar)/(sd)
    if(is.null(tails)){
      out <- c(`Raw Score` = newx, `Mean` = xbar, `Z Score` = z)
    }
    else {
      if(tails==1){
        pval <- ((2*pnorm(z, mean = xbar, sd = sd, lower.tail = TRUE))/2)
        out <- c(`Raw Score` = newx, `Mean` = xbar, `Z Score` = z, `p-value (1-tailed)` = pval)
      }
      if(tails==2){
        pval <- (2*pnorm(z, mean = xbar, sd = sd, lower.tail = TRUE))
        out <- c(`Raw Score` = newx, `Mean` = xbar, `Z Score` = z, `p-value (2-tailed)` = pval)
      }
      if(!((tails == 1) || (tails == 2))){
        out <- c(`Raw Score` = newx, `Mean` = xbar, `Z Score` = z)
      }
    }
  }
  return(out)
}
