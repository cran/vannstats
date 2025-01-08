#' Summarize Results of ow.anova
#'
#' Displays results of ow.anova
#' @importFrom stats printCoefmat
#' @param object Object returned by \code{\link{ow.anova}}.
#' @param ... Additional parameters to pass on.
#' @return Matrix of values for results from One-Way ANOVA test.
#' @examples
#' data1 <- mtcars
#' ow <- ow.anova(data1, mpg, cyl)
#'
#' summary(ow)
summary.ow.anova <- function(object, ...){
  cat("Call:\n")
  print(object$call)
  cat("\n")
  cat(paste0("One-Way Analysis of Variance (ANOVA)",": \n"))
  printCoefmat(object$results, signif.stars = T, has.Pvalue = T, na.print = "")
  cat("\n")
  if(length(object$post_hoc)>=1){
    cat("\n")
    cat(paste0("Tukey's HSD (Honestly Significant Difference)",": \n"))
    #cat(paste0("Comparing: ",object$comparison," \n"))
    cat("\n")
    printCoefmat(object$post_hoc, signif.stars = T, has.Pvalue = T)
    cat("\n")
  }
  #cat("\n")
}
