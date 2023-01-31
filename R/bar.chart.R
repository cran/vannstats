#' Simplified Bar Chart
#'
#' This function plots a bar chart (bar.chart) on a given data frame.
#' @import ggplot2 dplyr purrr ggrepel
#' @importFrom stats na.omit
#' @param df data frame to read in.
#' @param var1 the dependent/outcome variable, \eqn{Y}. The variable of interest that should be plotted.
#' @param lab logical (default set to \code{FALSE}). When set to \code{lab = TRUE}, will add frequency label for each bar in chart.
#' @return This function returns the bar chart for \code{var1} in data frame \code{df}.
#' @examples
#' data <- mtcars
#'
#' bar.chart(data,cyl)
#' @export


bar.chart <- function(df, var1, lab=FALSE){
  #options(warn=-1)
  #suppressWarnings()
  v1 <- NULL #necessary for removing the "undefined global function" warning
  bygroups <- length(match.call())#-3
  n_call3 <- as.character(match.call()[3])
  n_call4 <- as.character(match.call()[4])
  n1 <- deparse(substitute(var1))
  n1 <- as.character(n1)
  if(bygroups==3 & n_call3 == "F" | bygroups==3 & n_call3 == "FALSE" | bygroups==3 & n_call3 == "TRUE" | bygroups==3 & n_call3 == "T"){
    bygroups <- 2
  }
  if(bygroups==3 & n_call3 != "NULL"){
    bygroups <- 3
  }
  if(bygroups==4 & n_call4 != "NULL"){
    bygroups <- 3
  }
  if(bygroups==2) {
    title <- paste0("Bar Chart of '", deparse(substitute(df)), "'")
    labx <- deparse(substitute(df))
    df <- as.factor(df)
    df <- as.data.frame(df)
    df <- df %>%
      group_by(df) %>%
      summarise(Frequency = n()) %>%
      na.omit()
    df <- as.data.frame(df)
    p <- ggplot(data = df, aes(x = df, y = .data$Frequency)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette="Blues") +
      #geom_text(stat='identity', aes(label=paste0("n = ",Frequency)), vjust=-1) +
      ggtitle(title) + xlab(labx) +
      theme_classic() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.line = element_line(colour = "black"), axis.text.x = element_text(vjust=0.5, colour="#000000"),
            axis.text.y = element_text(face="bold", colour="#000000"), plot.title = element_text(hjust = 0.5, lineheight=1.5, face="bold"))
  }
  if(bygroups==3) {
    title <- paste0("Bar Chart of '", deparse(substitute(var1)), "'")
    labx <- deparse(substitute(var1))
    df <- as.data.frame(df)
    df <- df %>%
      group_by({{ var1 }}) %>%
      summarise(Frequency = n()) %>%
      na.omit()
    df <- as.data.frame(df)
    p <- ggplot2::ggplot(data = df, aes(x=as.factor({{ var1 }}), y = .data$Frequency)) +
      geom_bar(stat = "identity") +
      scale_fill_brewer(palette="Blues") +
      #geom_text(stat='identity', aes(label=paste0("n = ",Frequency)), vjust=-1) +
      ggtitle(title) + xlab(labx) +
      theme_classic() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.line = element_line(colour = "black"), axis.text.x = element_text(vjust=0.5, colour="#000000"),
            axis.text.y = element_text(face="bold", colour="#000000"), plot.title = element_text(hjust = 0.5, lineheight=1.5, face="bold"))
  }

  if(lab==TRUE) {
    p <- p + #geom_text(stat='identity', aes(label=paste0("n = ",Frequency)), vjust=-1)
      geom_label_repel(stat='identity', aes(label = paste0("n = ",.data$Frequency)),
                       size = 4, nudge_x = 1, show.legend = FALSE)
  }

  return(p)
}
