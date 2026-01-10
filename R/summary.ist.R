#' Summarize Results of is.t
#'
#' Displays results of is.t
#' @importFrom stats printCoefmat
#' @param object Object returned by \code{\link{is.t}}.
#' @param ... Additional parameters to pass on.
#' @return Matrix of values for results from independent samples t-test.
#' @export
#' @examples
#' data1 <- mtcars
#' ttest <- is.t(data1, mpg, am)
#'
#' summary(ttest)
#'
summary.ist <- function(object, ...){
  cat("Call:\n")
  print(object$call)
  cat("\n")
  #cat(paste0(object$call,": \n"))
  cat(paste0(object$name,": \n"))
  cat("\n")
  printCoefmat(object$results, signif.stars = T)
  cat("\n")
  cat("Group Means:\n")
  print(object$means)
  cat("\n")
  #cat("95 percent CI of Mean Difference:\n")
  #print(paste0(object$conf.int[1]," to ", object$conf.int[2]))
  #cat("\n")
  #cat("\n")
}
