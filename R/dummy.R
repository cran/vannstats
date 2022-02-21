#' Creating Dummy-Code Columns for Values of a Variable
#'
#' This function applies dummy-coding to a variable of interest, enabling the creation of \emph{n} or \emph{n-1} columns/variables based on \emph{n} number of attributes for the variable.
#' @import dplyr
#' @importFrom rlang .data
#' @param df data frame to read in.
#' @param var the variable to be dummy-coded. Is automatically converted to a character string.
#' @param remove logical (default set to \code{F}). When set to \code{remove = T}, will return a data frame using the true number of dummy coded columns (e.g. \emph{n-1}).
#' @return This function updates the data frame with new variables (columns) representing unique values of a selected variable, and a binary score (0/1) for the absence or presence of a column's represented value for each observation.
#' @examples
#' data <- howell_aids_long
#'
#' dummy(data, student)
#' @export

dummy <- function(df, var, remove=FALSE){
  df2 <- as.data.frame(df)
  col <- deparse(substitute(var))
  vals <- unique(eval(substitute(var), df2))
  vals <- as.character(vals)
  mat <- data.frame(matrix(ncol = length(vals), nrow = nrow(df2)))
  colnames(mat) <- vals
  mat[is.na(mat)] <- 0
  df2 <- df2 %>%
    dplyr::bind_cols(mat)
  for(row in 1:nrow(df2)){
    code <- df2[row, col]
    code <- as.character(code)
    df2[row, code] <- 1
  }
  if(remove==TRUE){
    df2 <- df2[1:(length(df2)-1)]
  }
  dfname <- deparse(substitute(df))
  pos <- 1
  envir = as.environment(pos)
  assign(dfname, df2, envir = envir)
}

