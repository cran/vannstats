#' Simplified One-Way Analysis of Variance
#'
#' This function simplifies the call for one-way ANOVA (ow.anova) on a given data frame. Also allows calls for Tukey's Honestly Significant Difference Post-Hoc Comparisons Test (hsd), as well as a means plot (plot).
#' @importFrom gplots plotmeans
#' @importFrom stats TukeyHSD aov
#' @param df data frame to read in.
#' @param var1 the dependent/outcome variable, \eqn{Y}.
#' @param by1 the main independent/predictor variable, \eqn{X}. A grouping variable by which \code{var1} should be parsed.
#' @param hsd logical (default set to \code{F}). When set to \code{hsd = T}, will return results of Tukey's Honestly Significant Difference Post-Hoc Comparisons Test.
#' @param plot logical (default set to \code{F}). When set to \code{plot = T}, will return a means plot with 95 percent confidence intervals, broken out by each group (\code{by1}).
#' @return This function returns the summary results table for a one-way ANOVA, examining mean differences in \code{var1} from data frame \code{df}, across \code{by1} groups.
#' @export
#' @examples
#' data <- mtcars
#'
#' ow <- ow.anova(data,mpg,cyl,plot=TRUE)
#' summary(ow)

ow.anova <- function(df, var1, by1, plot = FALSE, hsd = FALSE){
  #options(scipen=999)
  labx <- deparse(substitute(by1))
  laby <- deparse(substitute(var1))
  btwn <- paste0("Between Groups (",deparse(substitute(by1)),")")
  witn <- paste0("Within Groups (",deparse(substitute(by1)),")")
  model <- summary(aov(eval(substitute(var1), df) ~ factor(eval(substitute(by1), df))))
  #rownames(model[[1]]) <- c(deparse(substitute(by1)), "Residuals")
  italic_p <- "\U1D45D"
  italic_F <- "\U1D46D"
  #colnames(model[[1]]) <- c("df", "SS", "MS", "F", "p")
  model2 <- as.matrix(model[[1]])
  model2[,1] <- round(model2[,1], 0)
  #print(dim(model2))
  colnames(model2) <- c("df", "SS", "MS", "F", "p-value")
  rownames(model2) <- c(btwn, witn)
  #colnames(model[[1]]) <- c("df", "SS", "MS", italic_F, italic_p)
  #rownames(model[[1]]) <- c(btwn, witn)
  post_hoc<-matrix(,nrow=0,ncol=0)
  compare.name <- paste0(deparse(substitute(by1))," by ",deparse(substitute(var1)))

  if(plot==TRUE){
    #model <- summary(aov(eval(substitute(var1), df) ~ eval(substitute(by1), df)))
    ##rownames(model[[1]]) <- c(deparse(substitute(by1)), "Residuals")
    #rownames(model[[1]]) <- c(btwn, witn)
    {suppressWarnings(plotmeans(eval(substitute(var1), df) ~ eval(substitute(by1), df), main = "Plot of Group Means with 95% CI", xlab = labx, ylab = laby))}
  }

  if(hsd==TRUE){
    v2 <- as.factor(eval(substitute(by1), df))
    tukey <- stats::TukeyHSD(aov(eval(substitute(var1), df) ~ v2))
    comparison <- labx
    #names(tukey) <- "Tukey's HSD (Honestly Significant Difference)"
    #b <- tukey$v2
    #b <- format(tukey$v2, scientific=F)
    #sig <- rep(NA, length(b[,4]))
    #b <- cbind(b,sig)
    #b[,4] <- as.numeric(b[,4])
    #b[,5][b[,4]<=.05] <- "*"
    #b[,5][b[,4]<=.01] <- "**"
    #b[,5][b[,4]<=.001] <- "***"
    #colnames(b) <- c("diff", "lwr", "upr", italp)
    #return(b)
    post_hoc <- as.matrix(tukey[[1]])
    colnames(post_hoc) <- c("Mean Difference", "lwr", "upr", "p-value")
    #print(dim(tukey[[1]]))
    #list <- c(model, tukey)
    #names(list) <- c("Analysis of Variance (ANOVA)","Tukey's HSD (Honestly Significant Difference)")
    #return(list)
  }

  return.obj <- list(call = match.call(), results = model2, post_hoc = post_hoc#, comparison = compare.name
                     )
  class(return.obj) <- "oneway"
  return.obj
  #return(model2)
}
