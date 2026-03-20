#' Summarize Results of os.t
#'
#' Displays results of os.t
#' @importFrom stats printCoefmat
#' @param object Object returned by \code{\link{os.t}}.
#' @param ... Additional parameters to pass on.
#' @return Matrix of values for results from one sample t-test.
#' @export
#' @examples
#' data1 <- mtcars
#' ttest <- os.t(data1,mpg,3)
#'
#' summary(ttest)
#'
summary.ost <- function(object, ...){
  cat("Call:\n")
  print(object$call)
  cat("\n")
  #cat(paste0(object$call,": \n"))
  cat(paste0(object$name,": \n"))
  cat("\n")
  printCoefmat(object$results, signif.stars = T)
  cat("\n")
  cat("Sample and Population Means:\n")
  print(object$means)
  cat("\n")
  #cat("95 percent CI of Mean Difference:\n")
  #print(paste0(object$conf.int[1]," to ", object$conf.int[2]))
  #cat("\n")
  #cat("\n")
}
