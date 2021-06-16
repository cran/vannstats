#' Simplified Residuals Plot
#'
#' This function creates a residual plot (residplot) on a data frame of the variables in an equation.
#' @importFrom formula.tools get.vars
#' @importFrom stats predict residuals
#' @param df data frame to read in.
#' @param formula the variables in the regression model, \eqn{Y = X_1 + X_2 + ... + X_m}, written as \code{Y ~ X1 + X2}...
#' @examples
#' data <- mtcars
#'
#' residplot(data, mpg ~ wt + am)
#' @export


residplot <- function(df, formula){
  y <- get.vars(formula, data = df)[1]
  main <- paste0("Residual Plot Predicting ", deparse(substitute(y)))
  reg <- lm(formula, data=df)
  predicted <- predict(reg)
  residuals <- residuals(reg)
  df2 <- data.frame(predicted,residuals)
  plot(df2$predicted, df2$residuals, xlab="Predicted Y Value", ylab="Residuals", main=main)
  abline(0, 0)
}
