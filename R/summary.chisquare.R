#' Summarize Results of chi.sq
#'
#' Displays results of chi.sq
#' @importFrom stats printCoefmat
#' @param object Object returned by \code{\link{chi.sq}}.
#' @param ... Additional parameters to pass on.
#' @return Matrix of values for results from chi square test.
#' @export
#' @examples
#' data1 <- mtcars
#' x2 <- chi.sq(data1, vs, am)
#'
#' summary(x2)
#'
summary.chisquare <- function(object, ...){
  cat("Call:\n")
  print(object$call)
  cat("\n")
  #cat(paste0(object$call,": \n"))
  cat(paste0(object$name,": \n"))
  cat("\n")
  printCoefmat(object$results, signif.stars = T)
  cat("\n")
  if(length(object$post_hoc)>=1){
    cat("\n")
    cat("Post-Hoc Test w/ Bonferroni Adjustment:")
    cat("\n")
    cat(paste0("Comparing: ",object$comparison," \n"))
    cat("\n")
    printCoefmat(object$post_hoc, signif.stars = T, has.Pvalue = T)
    cat("\n")
  }
  if(length(object$cramers)>=1){
    cat("\n")
    cat("Cramer's Measure of Association:")
    cat("\n")
    printCoefmat(object$cramers)
    cat("\n")
  }
  #cat("\n")
}
