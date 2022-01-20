#' Reverse Coding for Scales
#'
#' This function applies reverse-coding to a variable of interest.
#' @import dplyr
#' @importFrom rlang .data
#' @param df data frame to read in.
#' @param var the variable to be recoded.
#' @param missing a list of values in the variable that are ``missing'' values.
#' @return This function updates the data frame with a new variable with the recoded values.
#' @examples
#' data <- GSS2014
#'
#' revcode(data, amcult)
#' @export

revcode <- function(df, var, missing=c("")){
  df2 <- as.data.frame(df)
  max <- max(eval(substitute({{ var }}), df2), na.rm = T)
  min <- min(eval(substitute({{ var }}), df2), na.rm = T)
  df2 <- df2 %>%
    dplyr::mutate(newvar = ((max-{{ var }})+min) )
  if(length(missing)>1){
    for(i in {{ missing }}){
      i <- as.numeric(i)
      df2 <- df2 %>%
        dplyr::mutate(newvar=replace(.data$newvar, {{ var }}==i, NA))
    }
  }
  names(df2)[names(df2) == "newvar"] <- paste0("rev.",substitute(var))
  #output is an updated data frame
  dfname <- deparse(substitute(df))
  pos <- 1
  envir = as.environment(pos)
  assign(dfname, data.frame(df2), envir = envir)
}

