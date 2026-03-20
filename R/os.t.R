#' Simplified One Sample T-Test
#'
#' This function simplifies the call for the one sample t-test (os.t) on a given data frame.
#' @importFrom stats t.test qt
#' @param df data frame to read in.
#' @param var1 the dependent/outcome variable, \eqn{Y}.
#' @param mu the population mean, \eqn{\mu}.
#' @return This function returns the summary results table for an one sample t-test, examining the mean differences between \code{var1} (in data frame \code{df}) and the population mean \code{mu}.
#' @export
#' @examples
#' data <- mtcars
#'
#' ttest <- os.t(data,mpg,3)
#' summary(ttest)
#'

os.t <- function(df, var1, mu){
  #options(scipen=999)
  #suppressWarnings({})
  v1 <- deparse(substitute(var1)) #DV
  v2 <- deparse(substitute(mu)) #population mean
  #v2_mu <- as.numeric(v2)
  {suppressWarnings(model <- t.test(eval(substitute(var1), df), mu = mu, alternative = "two.sided"))}
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
  #if(two.tailed == FALSE){
  #  crit <- qt(p=.95, df=df_1)
  #}
  t_text <- "\U1D461"
  t_c_text <- paste0("Critical ",t_text)
  model$statistic[2] <- round(crit, 3)
  #model$statistic[1] <- t_val_new
  names(model$statistic) <- c(t_text,t_c_text)
  compare.name <- paste0(deparse(substitute(var1)),"-",deparse(substitute(mu)))

  #if(var.equal == FALSE){
  #  {suppressWarnings(model <- t.test(eval(substitute(var1), df) ~ eval(substitute(var2), df), var.equal = var.equal))}
  #  modname <- model$method
  #  df_1 <- model$parameter[[1]]
  #  t_val_original <- as.numeric(model$statistic)
  #  t_val <- as.numeric(model$statistic)
  #  t_val <- abs(t_val)
  #  t_val <- round(t_val, 3)
  #  pm_text <- "\U00B1"
  #  t_val_new <- paste0(pm_text,t_val)
  #  #n_cases <- min(sum(!is.na(eval(substitute(var1), df))),sum(!is.na(eval(substitute(var2), df))))
  #  crit <- qt(p=.975, df=df_1)
  #  if(two.tailed == FALSE){
  #    crit <- qt(p=.95, df=df_1)
  #  }
  #  t_text <- "\U1D461"
  #  t_c_text <- paste0("Critical ",t_text)
  #  model$statistic[2] <- round(crit, 3)
  #  #model$statistic[1] <- t_val_new
  #  names(model$statistic) <- c(t_text,t_c_text)
  #}

  ##print(crit)

  modname <- "One Sample t-test"
  model$method <- modname
  model$data.name <- paste0(deparse(substitute(var1))," by \U03BC")

  mean_text <- paste0("\U0078\u0305",":")
  #print(names(model$means))
  #names(model$estimate) <- gsub("mean of x", mean_text, names(model$estimate))
  mu_text <- paste0("\U03BC",":") #U+03BC
  model$estimate[2] <- deparse(substitute(mu))
  model$estimate <- as.numeric(model$estimate)
  names(model$estimate) <- c(mean_text,mu_text)
  #print(model$estimate)
  #add MU of mu value as second named number in model$estimate

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
  class(return.obj) <- "ost"
  return.obj

  #return(model)
}
