#' Simplified Correlation Matrix
#'
#' This function creates a correlation (cormat) on a data frame of the variables in an equation.
#' @importFrom formula.tools get.vars
#' @importFrom stats cor
#' @param df data frame to read in.
#' @param formula the variables in the regression model, \eqn{Y = X_1 + X_2 + ... + X_m}, written as \code{Y ~ X1 + X2}...
#' @return This function returns a correlation matrix for the variables provided in the formula.

#' @examples
#' data <- mtcars
#'
#' cormat(data, mpg ~ wt + am)
#' @export


cormat <- function(df, formula){
  vars <- get.vars(formula, data = df)
  frame <- df %>%
    dplyr::select(all_of(vars))
  frame <- as.data.frame(frame)
  #find variables/columns with characters/factors and convert to numeric, then report recode values as.numeric(frame$nom/ord)-1
  cormatrix <- round((cor(frame, use = "complete.obs")),2)
  #cormatrix <- data.frame(cormatrix)
  cormatrix[upper.tri(cormatrix)] <- ""
  cormatrix[cormatrix == 1.00] <- 1
  cormatrix <- as.data.frame(cormatrix)
  #cormatrix
  return(cormatrix)
}
