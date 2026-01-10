#' Simplified Independent Samples T-Test
#'
#' This function simplifies the call for the independent samples t-test (is.t) on a given data frame.
#' @importFrom stats t.test qt
#' @param df data frame to read in.
#' @param var1 the dependent/outcome variable, \eqn{Y}.
#' @param var2 the main independent/predictor variable, \eqn{X}.
#' @param var.equal logical (default set to \code{T}). When set to \code{var.equal = F}, will employ Welsh's correction to the t-test (for data that violate the equal variances assumption).
#' @param two.tailed logical (default set to \code{T}). When set to \code{two.tailed = F}, will return results of a one-sided t-test.
#' @return This function returns the summary results table for an independent samples t-test, examining the mean differences of \code{var1} (in data frame \code{df}) between groups in \code{var2}.
#' @export
#' @examples
#' data <- mtcars
#'
#' ttest <- is.t(data,mpg,am)
#' summary(ttest)
#'

is.t <- function(df, var1, var2, var.equal = TRUE, two.tailed = TRUE){
  #options(scipen=999)
  #suppressWarnings({})
  v1 <- deparse(substitute(var1)) #DV
  v2 <- deparse(substitute(var2)) #IV
  {suppressWarnings(model <- t.test(eval(substitute(var1), df) ~ eval(substitute(var2), df), var.equal = var.equal))}
  modname <- model$method
  df_1 <- model$parameter[[1]]
  t_val_original <- as.numeric(model$statistic)
  t_val <- as.numeric(model$statistic)
  t_val <- abs(t_val)
  t_val <- round(t_val, 3)
  pm_text <- "\U00B1"
  t_val_new <- paste0(pm_text,t_val)
  #n_cases <- min(sum(!is.na(eval(substitute(var1), df))),sum(!is.na(eval(substitute(var2), df))))
  crit <- qt(p=.975, df=df_1)
  if(two.tailed == FALSE){
    crit <- qt(p=.95, df=df_1)
  }
  t_text <- "\U1D461"
  t_c_text <- paste0("Critical ",t_text)
  model$statistic[2] <- round(crit, 3)
  #model$statistic[1] <- t_val_new
  names(model$statistic) <- c(t_text,t_c_text)
  compare.name <- paste0(deparse(substitute(var2)),"-",deparse(substitute(var1)))

  if(var.equal == FALSE){
    {suppressWarnings(model <- t.test(eval(substitute(var1), df) ~ eval(substitute(var2), df), var.equal = var.equal))}
    modname <- model$method
    df_1 <- model$parameter[[1]]
    t_val_original <- as.numeric(model$statistic)
    t_val <- as.numeric(model$statistic)
    t_val <- abs(t_val)
    t_val <- round(t_val, 3)
    pm_text <- "\U00B1"
    t_val_new <- paste0(pm_text,t_val)
    #n_cases <- min(sum(!is.na(eval(substitute(var1), df))),sum(!is.na(eval(substitute(var2), df))))
    crit <- qt(p=.975, df=df_1)
    if(two.tailed == FALSE){
      crit <- qt(p=.95, df=df_1)
    }
    t_text <- "\U1D461"
    t_c_text <- paste0("Critical ",t_text)
    model$statistic[2] <- round(crit, 3)
    #model$statistic[1] <- t_val_new
    names(model$statistic) <- c(t_text,t_c_text)
  }

  #print(crit)

  modname <- "Independent Samples (Two Sample) t-test"
  model$method <- modname
  model$data.name <- paste0(deparse(substitute(var1))," by ", deparse(substitute(var2)))

  mean_text <- paste0("\U0078\u0305",":")
  names(model$estimate) <- gsub("mean in group", mean_text, names(model$estimate))

  #print(model$residuals)
  #print(model$stdres)

  #post_hoc<-matrix(,nrow=0,ncol=0)
  #cramers<-matrix(,nrow=0,ncol=0)
  #z_crit<-matrix(,nrow=0,ncol=0)
  #stdres_copy<-matrix(,nrow=0,ncol=0)
  #adj_p_vals_copy<-matrix(,nrow=0,ncol=0)
  #newList <- model


  out.mat <- matrix(, nrow = 1, ncol = 4)
  out.mat[,1] <- model$statistic[1]
  out.mat[,2] <- model$statistic[2]
  out.mat[,3] <- df_1
  out.mat[,4] <- model$p.value
  #rownames(out.mat) <- model$method
  rownames(out.mat) <- ""
  colnames(out.mat) <- c(t_text, t_c_text, "df","p-value")

  return.obj <- list(call = match.call(), results = out.mat, name = model$method, means = model$estimate, null.value = model$null.value, conf.int = model$conf.int#,
                     #comparison = compare.name,post_hoc = post_hoc, cramers = cramers, z_crit = z_crit, z_mat = stdres_copy, p_z_mat = adj_p_vals_copy
                     )
  class(return.obj) <- "ist"
  return.obj

  #return(model)
}
