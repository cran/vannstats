#' Simplified Descriptive Statistics
#'
#' This function returns univariate/descriptive statistics (univ.desc) on a variable within a given data frame, and uses simplified calls within the function to parse the descriptives by another variable.
#' @import dplyr
#' @importFrom DescTools Skew Kurt MeanSE
#' @importFrom stats median var
#' @param df data frame to read in.
#' @param var1 the dependent/outcome variable, \eqn{Y}. The variable of interest .
#' @param by1 the main independent/predictor variable, \eqn{X_1}. A grouping variable by which the descriptive statistics for \code{var1} should be parsed.
#' @return This function returns the descriptive statistics for \code{var1} in data frame \code{df}. Can be split to return descriptives for \code{var1} in data frame \code{df}, broken out by \code{var2}.
#' @examples
#' data <- mtcars
#'
#' univ.desc(data,mpg)
#' @export


univ.desc <- function(df, var1, by1){
  #options(warn=-1)
  #suppressWarnings()
  bygroups <- length(match.call())-3
  if(bygroups==0) {
    p <- df %>%
      dplyr::summarise(n = sum(!is.na({{var1}})),
                mean = mean({{var1}}, na.rm=T),
                sd = sd({{var1}}, na.rm=T),
                variance = var({{var1}}, na.rm=T),
                median = median({{var1}}, na.rm=T),
                min = min({{var1}}, na.rm=T),
                max = max({{var1}}, na.rm=T),
                skewness = Skew({{var1}}, na.rm=T),#DescTools
                kurtosis = Kurt({{var1}}, na.rm=T),#DescTools
                se = MeanSE({{var1}}, na.rm=T)#DescTools
      )
  }

  if(bygroups==1) {
    p <- as.data.frame(df %>%
      group_by({{by1}}) %>% dplyr::filter(!is.na({{by1}})) %>%
      dplyr::summarise(n = sum(!is.na({{var1}})),
                       mean = mean({{var1}}, na.rm=T),
                       sd = sd({{var1}}, na.rm=T),
                       variance = var({{var1}}, na.rm=T),
                       median = median({{var1}}, na.rm=T),
                       min = min({{var1}}, na.rm=T),
                       max = max({{var1}}, na.rm=T),
                       skewness = Skew({{var1}}, na.rm=T),#DescTools
                       kurtosis = Kurt({{var1}}, na.rm=T),#DescTools
                       se = MeanSE({{var1}}, na.rm=T)#DescTools
      )
    )
  }
  #df$group
  #print(df)
  return(p)
}



