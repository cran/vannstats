#' Simplified Confidence Interval Calculation
#'
#' This function calculates the confidence interval (for a given confidence level) for a variable in a given data frame.
#' @importFrom gdata nobs
#' @importFrom stats qt sd
#' @param df data frame to read in.
#' @param var1 the variable of interest for which the CI will be calculated.
#' @param cl the desired confidence level (in percentages, ranging from \eqn{1} to \eqn{100}).
#' @return This function returns the mean, lower bound, upper bound, and standard error.
#' @examples
#' data <- mtcars
#'
#' ci.calc(data,mpg,cl=95)
#' @export


ci.calc <- function (df, var1, cl){
  cl <- (cl/100)
  alpha <- 1 - cl
  calls <- length(match.call())-3
  if(calls==0){
    #CI(eval(substitute(var1), df), ci=cl)
    xbar <- mean(df, na.rm = TRUE)
    se <- sd(df, na.rm = TRUE)/sqrt(nobs(df))
    ci_low <- xbar + round(qt(alpha/2, 100000000000), 3) * se
    ci_high <- xbar - round(qt(alpha/2, 100000000000), 3) * se
    out <- c(Mean = xbar, `CI lower` = ci_low, `CI upper` = ci_high,
             `Std. Error` = se)
    #print(abs(round(qt(alpha/2, 100000000000), 3)))
  }
  else {
    #CI(eval(substitute(var1), df), ci=cl)
    xbar <- mean(eval(substitute(var1), df), na.rm = TRUE)
    se <- sd(eval(substitute(var1), df), na.rm = TRUE)/sqrt(nobs(eval(substitute(var1), df)))
    ci_low <- xbar + round(qt(alpha/2, 100000000000), 3) * se
    ci_high <- xbar - round(qt(alpha/2, 100000000000), 3) * se
    out <- c(Mean = xbar, `CI lower` = ci_low, `CI upper` = ci_high,
             `Std. Error` = se)
    #print(abs(round(qt(alpha/2, 100000000000), 3)))
  }
  return(out)
}

