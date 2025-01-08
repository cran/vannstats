#' Simplified Z Tests
#'
#' This function runs a one-sample Z-test, comparing the proportion in your sample to the proportion in the population.
#' @importFrom stats sd pnorm
#' @param df data frame to read in.
#' @param var1 variable with the total number of events, by sub-unit (e.g. cities within a county).
#' @param var2 variable with number of events for a specific group.
#' @param prop proportion to compare to (between 0 and 1).
#' @return This function returns the Z score and p-value for the z-test.
#' @examples
#' data <- UCR2015[UCR2015$state=="California",]
#' data$total_part2 <- data$burglary + data$larceny + data$mv_theft + data$arson
#'
#' z.test(data,total_part2,burglary,.25)
#' @export

z.test <- function (df, var1, var2, prop){
  calls <- length(match.call())-4
  if(calls==0){
    n <- sum(df, na.rm = TRUE)
    var1tot <- sum(var1, na.rm = TRUE)
    pi <- var1tot / n
    pi_0 <- var2
    z_stat <- (pi - pi_0) / sqrt((pi_0 * (1 - pi_0)) / n)
    if(pi < pi_0){
      sig <- 2*pnorm(q=z_stat, lower.tail=TRUE)
    }
    else{
      sig <- 2*pnorm(q=z_stat, lower.tail=FALSE)
    }
    z2 <- round(z_stat,3)
    out <- c(`Proportion` = pi, `Comparison Proportion` = pi_0, `Z Score` = z2, `p-value` = sig)
  }
  else{
    #new1 <- gsub("\\s*\\([^\\)]+\\)","",as.character(match.call()[4]))
    #print(new1)
    #new1 <- as.numeric(new1)
    n <- sum(eval(substitute(var1), df), na.rm = TRUE)
    var2tot <- sum(eval(substitute(var2), df), na.rm = TRUE)
    pi <- var2tot / n
    pi_0 <- prop
    z_stat <- (pi - pi_0) / sqrt((pi_0 * (1 - pi_0)) / n)
    if(pi < pi_0){
      sig <- 2*pnorm(q=z_stat, lower.tail=TRUE)
    }
    else{
      sig <- 2*pnorm(q=z_stat, lower.tail=FALSE)
    }
    z2 <- round(z_stat,3)
    out <- c(`Proportion` = pi, `Comparison Proportion` = pi_0, `Z Score` = z2, `p-value` = sig)
  }
  return(out)
  #https://rdrr.io/cran/DAAG/man/onet.permutation.html
}

