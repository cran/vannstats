#' Simplified Paired Samples (Repeated Measures) T-Test
#'
#' This function simplifies the call for the paired samples t-test (ps.t) on a given data frame.
#' @importFrom stats t.test qt complete.cases
#' @param df data frame to read in.
#' @param t2 the \eqn{t_2} or post-test variable.
#' @param t1 the \eqn{t_1} or pre-test variable.
#' @param var.equal logical (default set to \code{T}). When set to \code{var.equal = F}, will employ Welsh's correction to the t-test (for data that violate the equal variances assumption).
#' @return This function returns the summary results table for an paired samples t-test, examining the mean differences between \code{t2} and \code{t1} (in data frame \code{df}).
#' @export
#' @examples
#' data <- mtcars
#' data$mpg2 <- c(21.8,14.7,5.9,24.0,12.2,14.9,33.0,27.7,
#' 29.9,33.9,16.0,14.7,13.3,23.1,38.0,39.7,24.1,33.9,
#' 25.7,24.2,28.3,37.3,20.3,36.0,18.1,32.8,28.7,30.3,6.6,
#' 26.4,35.8,32.8)
#'
#' ttest <- ps.t(data,mpg2,mpg)
#' summary(ttest)
#'

ps.t <- function(df, t2, t1, var.equal = TRUE){
  #options(scipen=999)
  #suppressWarnings({})
  v1 <- deparse(substitute(t2)) #DV
  v2 <- deparse(substitute(t1)) #IV
  {suppressWarnings(model <- t.test(eval(substitute(t2), df), eval(substitute(t1), df), paired = TRUE, var.equal = var.equal))}
  modname <- model$method
  df_1 <- model$parameter[[1]]
  t_val_original <- as.numeric(model$statistic)
  t_val <- as.numeric(model$statistic)
  t_val <- abs(t_val)
  t_val <- round(t_val, 3)
  pm_text <- "\U00B1"
  t_val_new <- paste0(pm_text,t_val)
  #n_cases <- min(sum(!is.na(eval(substitute(t2), df))),sum(!is.na(eval(substitute(t1), df))))
  crit <- qt(p=.975, df=df_1)

  #if(two.tailed == FALSE){
  #  crit <- qt(p=.95, df=df_1)
  #}

  t_text <- "\U1D461"
  t_c_text <- paste0("Critical ",t_text)
  model$statistic[2] <- round(crit, 3)
  #model$statistic[1] <- t_val_new
  names(model$statistic) <- c(t_text,t_c_text)
  compare.name <- paste0(deparse(substitute(t1)),"-",deparse(substitute(t2)))

  if(var.equal == FALSE){
    {suppressWarnings(model <- t.test(eval(substitute(t2), df) ~ eval(substitute(t1), df), var.equal = var.equal))}
    modname <- model$method
    df_1 <- model$parameter[[1]]
    t_val_original <- as.numeric(model$statistic)
    t_val <- as.numeric(model$statistic)
    t_val <- abs(t_val)
    t_val <- round(t_val, 3)
    pm_text <- "\U00B1"
    t_val_new <- paste0(pm_text,t_val)
    #n_cases <- min(sum(!is.na(eval(substitute(t2), df))),sum(!is.na(eval(substitute(t1), df))))
    crit <- qt(p=.975, df=df_1)
    #if(two.tailed == FALSE){
    #  crit <- qt(p=.95, df=df_1)
    #}
    t_text <- "\U1D461"
    t_c_text <- paste0("Critical ",t_text)
    model$statistic[2] <- round(crit, 3)
    #model$statistic[1] <- t_val_new
    names(model$statistic) <- c(t_text,t_c_text)
  }

  #print(crit)

  modname <- "Paired Samples (Repeated Measures) t-test"
  model$method <- modname
  model$data.name <- paste0(deparse(substitute(t2))," and ", deparse(substitute(t1)))

  mean_text <- paste0("\U0078\u0305",":")
  #names(model$estimate) <- gsub("mean in group", mean_text, names(model$estimate))
  df2 <- df[complete.cases(df[, c(deparse(substitute(t2)),deparse(substitute(t1)))]), ]
  mean2_text <- paste0(mean_text," ",deparse(substitute(t2)))
  mean2 <- mean(eval(substitute(t2), df2),na.rm=TRUE)
  mean1_text <- paste0(mean_text," ",deparse(substitute(t1)))
  mean1 <- mean(eval(substitute(t1), df2),na.rm=TRUE)
  meandiff_text <- "mean difference"
  meandiff <- model$estimate[1]
  model$estimate[1] <- mean2
  model$estimate[2] <- mean1
  model$estimate[3] <- meandiff
  model$estimate <- as.numeric(model$estimate)
  names(model$estimate) <- c(mean2_text,mean1_text,meandiff_text)




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
  class(return.obj) <- "pst"
  return.obj

  #return(model)
}
