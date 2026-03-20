#' Summarize Results of ps.t
#'
#' Displays results of ps.t
#' @importFrom stats printCoefmat
#' @param object Object returned by \code{\link{ps.t}}.
#' @param ... Additional parameters to pass on.
#' @return Matrix of values for results from paired samples t-test.
#' @export
#' @examples
#' data1 <- mtcars
#' data1$mpg2 <- c(21.8,14.7,5.9,24.0,12.2,14.9,33.0,27.7,
#' 29.9,33.9,16.0,14.7,13.3,23.1,38.0,39.7,24.1,33.9,
#' 25.7,24.2,28.3,37.3,20.3,36.0,18.1,32.8,28.7,30.3,6.6,
#' 26.4,35.8,32.8)
#'
#' ttest <- ps.t(data1,mpg2,mpg)
#'
#' summary(ttest)
#'
summary.pst <- function(object, ...){
  cat("Call:\n")
  print(object$call)
  cat("\n")
  #cat(paste0(object$call,": \n"))
  cat(paste0(object$name,": \n"))
  cat("\n")
  printCoefmat(object$results, signif.stars = T)
  cat("\n")
  cat("Group Means and Difference:\n")
  print(object$means)
  cat("\n")
  #cat("95 percent CI of Mean Difference:\n")
  #print(paste0(object$conf.int[1]," to ", object$conf.int[2]))
  #cat("\n")
  #cat("\n")
}
