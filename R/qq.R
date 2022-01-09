#' Simplified Normal (Q-Q) Plot
#'
#' This function plots a Q-Q/Quantile-Quantile plot (qq) on a given data frame, and uses simplified calls within the function to parse the Q-Q plot by up to 2 variables.
#' @import dplyr
#' @param df data frame to read in.
#' @param var1 the dependent/outcome variable, \eqn{Y}. The variable of interest that should be plotted.
#' @param by1 the main independent/predictor variable, \eqn{X_1}. A grouping variable by which the Q-Q plot for \code{var1} should be parsed.
#' @param by2 a potential second independent/predictor variable, \eqn{X_2}. A second grouping variable by which the Q-Q plot for \code{var1} (already parsed by \code{by1}) should be parsed.
#' @return This function returns the quantile-quantile plot for \code{var1} in data frame \code{df}. Can be split to return a quantile-quantile plot for \code{var1} in data frame \code{df}, broken out by \code{var2}.
#' @examples
#' data <- mtcars
#'
#' qq(data,mpg,cyl)
#' @export

qq <- function(df, var1, by1, by2){
  v1 <- NULL #necessary for removing the "undefined global function" warning
  bygroups <- length(match.call())-3
  if(bygroups==-1) {
    title <- paste0("Normal Q-Q Plot of '", deparse(substitute(df)), "'")
    laby <- paste0("Observed Value of '",deparse(substitute(df)),"'")
    df <- as.data.frame(df)
    names(df) <- "v1"
    df <- df %>%
      dplyr::filter(!is.na(v1))
    p <- ggplot2::ggplot(data = df, aes(sample=v1)) +
      stat_qq(shape = 1) + stat_qq_line() +
      facet_null() +
      ggtitle(title) + labs(x = "Expected (Theoretical) Normal", y = laby) +
      theme_classic() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.line = element_line(colour = "black"), axis.text.x = element_text(vjust=0.5, colour="#000000"),
            axis.text.y = element_text(face="bold", colour="#000000"), plot.title = element_text(hjust = 0.5, lineheight=1.5, face="bold"))
  }

  if(bygroups==0) {
    df <- df %>%
      dplyr::filter(!is.na({{ var1 }}))
    title <- paste0("Normal Q-Q Plot of '", deparse(substitute(var1)), "'")
    laby <- paste0("Observed Value of '",deparse(substitute(var1)),"'")
    p <- ggplot2::ggplot(data = df, aes(sample={{ var1 }})) +
      stat_qq(shape = 1) + stat_qq_line() +
      facet_null() +
      ggtitle(title) + labs(x = "Expected (Theoretical) Normal", y = laby) +
      theme_classic() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.line = element_line(colour = "black"), axis.text.x = element_text(vjust=0.5, colour="#000000"),
            axis.text.y = element_text(face="bold", colour="#000000"), plot.title = element_text(hjust = 0.5, lineheight=1.5, face="bold"))
  }

  if(bygroups==1) {
    df <- df %>%
      dplyr::filter(!is.na({{ var1 }})) %>% dplyr::filter(!is.na({{ by1 }}))
    df <- df %>%
      mutate(group = {{ by1 }})
    title <- paste0("Normal Q-Q Plot of '", deparse(substitute(var1)),"' by '", deparse(substitute(by1)), "'")
    laby <- paste0("Observed Value of '",deparse(substitute(var1)),"'")
    p <- ggplot2::ggplot(data = df, aes(sample={{ var1 }})) +
      stat_qq(shape = 1) + stat_qq_line() +
      facet_wrap(~group) +
      ggtitle(title) + labs(x = "Expected (Theoretical) Normal", y = laby) +
      theme_classic() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.line = element_line(colour = "black"), axis.text.x = element_text(vjust=0.5, colour="#000000"),
            axis.text.y = element_text(face="bold", colour="#000000"), plot.title = element_text(hjust = 0.5, lineheight=1.5, face="bold"))
  }

  if(bygroups==2) {
    df <- df %>%
      dplyr::filter(!is.na({{ var1 }})) %>% dplyr::filter(!is.na({{ by1 }})) %>% dplyr::filter(!is.na({{ by2 }}))
    df <- df %>%
      mutate(group = paste0({{ by1 }},", ",{{ by2 }}))
    title <- paste0("Normal Q-Q Plot of '",deparse(substitute(var1)),"' by '",deparse(substitute(by1)),"' and '",deparse(substitute(by2)),"'")
    laby <- paste0("Observed Value of '",deparse(substitute(var1)),"'")
    p <- ggplot2::ggplot(data = df, aes(sample={{ var1 }})) +
      stat_qq(shape = 1) + stat_qq_line() +
      facet_wrap(~group) +
      ggtitle(title) + labs(x = "Expected (Theoretical) Normal", y = laby) +
      theme_classic() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.line = element_line(colour = "black"), axis.text.x = element_text(vjust=0.5, colour="#000000"),
            axis.text.y = element_text(face="bold", colour="#000000"), plot.title = element_text(hjust = 0.5, lineheight=1.5, face="bold"))
  }
  return(p)
}


