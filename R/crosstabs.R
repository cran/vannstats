#' Simplified Crosstabs
#'
#' This function returns a crosstab (tab) on a given data frame, and using simplified calls within the function for two variables, to return the observed and expected frequencies.
#' @import ggplot2 dplyr purrr
#' @importFrom stats chisq.test
#' @param df data frame to read in.
#' @param var1 a first grouping variable.
#' @param var2 a second grouping variable.
#' @return This function returns the observed and expected frequencies of a bivariate relationship between \code{var1} and \code{var2} in data frame \code{df}.

#' @examples
#' data <- mtcars
#'
#' tab(data,mpg,cyl)
#' @export


tab <- function(df, var1, var2){
  #options(warn=-1) # suppress warnings for chi square run
  #v1 <- paste0(substitute(df),"$",substitute(var1)) # how to use $ operator in dataframe$variable
  #v2 <- paste0(substitute(df),"$",substitute(var2)) # how to use $ operator in dataframe$variable
  v1 <- (eval(substitute(var1), df))
  v2 <- (eval(substitute(var2), df))
  suppressWarnings(crosstab <- chisq.test(v1, v2, correct=FALSE))
  #options(warn=0) # unsuppress warnings for chi square
  crosstab$data.name <- paste0(deparse(substitute(var1)), " and ", deparse(substitute(var2)))
  names(dimnames(crosstab$observed)) <- c(deparse(substitute(var1)),deparse(substitute(var2)))
  names(dimnames(crosstab$expected)) <- c(deparse(substitute(var1)),deparse(substitute(var2)))
  #crosstab$observed
  #crosstab$expected

  rowmarg <- NULL
  for(i in 1:dim(crosstab$observed)[1]){
    i_rmarg <- sum(crosstab$observed[i,])
    rowmarg <- c(rowmarg,i_rmarg)
  }
  #print(rowmarg)

  colmarg <- NULL
  for(i in 1:dim(crosstab$observed)[2]){
    i_cmarg <- sum(crosstab$observed[,i])
    colmarg <- c(colmarg,i_cmarg)
  }

  tot_tot <- sum(colmarg)
  colmarg <- c(colmarg,tot_tot)

  obs <- as.matrix(crosstab$observed)
  ex <- as.matrix(crosstab$expected)
  endcol <- dim(crosstab$observed)[2] + 1
  endrow <- dim(crosstab$observed)[1] + 1
  #print(endrow)
  #print(endcol)
  #print(colmarg)
  #print(endcol)
  obs <- cbind(obs,rowmarg)
  obs <- rbind(obs,colmarg)
  ex <- cbind(ex,rowmarg)
  ex <- rbind(ex,colmarg)
  #print(dimnames(obs))

  dimnames(obs)[[1]][endrow] <- "Total" #paste0("Col. Total", " (", deparse(substitute(var2)), "):")
  dimnames(obs)[[2]][endcol] <- "Total" #paste0("Row Total", " (", deparse(substitute(var1)), "):")
  dimnames(obs)[[1]][1] <- paste0(deparse(substitute(var1)), ": ", dimnames(obs)[[1]][1])
  dimnames(obs)[[2]][1] <- paste0(deparse(substitute(var2)), ": ", dimnames(obs)[[2]][1])

  dimnames(ex)[[1]][endrow] <- "Total" #paste0("Col Total", " (", deparse(substitute(var2)), "):")
  dimnames(ex)[[2]][endcol] <- "Total" #paste0("Row Total", " (", deparse(substitute(var1)), "):")
  dimnames(ex)[[1]][1] <- paste0(deparse(substitute(var1)), ": ", dimnames(ex)[[1]][1])
  dimnames(ex)[[2]][1] <- paste0(deparse(substitute(var2)), ": ", dimnames(ex)[[2]][1])


  #print(dimnames(obs))
  #print(endcol)
  #print(obs)



  #tab <- list(crosstab$observed, crosstab$expected)
  tab <- list(obs, ex)
  names(tab) <- c("Observed Frequencies","Expected Frequencies")
  return(tab)
}



