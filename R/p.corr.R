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
  names(model$statistic) <- "\U1D493" #"\u0072" #"\u0072\U00B2" #U1D493
  model$estimate <- t
  names(model$estimate) <- "\U1D495" #bold italic t #"\U1D461"#reg italic t
  #(model$p.value,"names") <- "p"
  #names(model$p.value) <- "\U1D45D" #\U1D45D == p italic
  model$data.name <- paste0(deparse(substitute(var1))," and ", deparse(substitute(var2)))
  df_1 <- model$parameter[[1]]
  crit_t <- (-qt(0.025,df_1))
  #print(crit_t)
  crit_r2 <- ((crit_t^2) / ((crit_t^2) + df_1))
  crit_r <- sqrt(crit_r2)
  #print(crit_r)
  return(model)
}
