#' Simplified Normal (Q-Q) Plot
#'
#' This function plots a Q-Q/Quantile-Quantile plot (qq) on a given data frame, and uses simplified calls within the function to parse the Q-Q plot by up to 2 variables.
#' @import dplyr ggpubr
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
    #norm_test <- shapiro.test(eval(substitute(v1), df))
    #norm_test_p <- round(norm_test$p.value,3)
    #norm_text <- paste0("p = ", norm_test_p)
    p <- ggplot2::ggplot(data = df, aes(sample=v1)) +
      #ggplot2::stat_qq(shape = 1) + ggplot2::stat_qq_line() +
      geom_point(stat = "qq", shape = 1) + geom_qq_line() +
      geom_exec(.stat_qq_confint, alpha = 0.2, conf.int.level = .95) +

      #annotate("text", x = 1, y=40, label = norm_text) +

      #qqplotr::stat_qq_band() +
      #qqplotr::stat_qq_line() +
      #qqplotr::stat_qq_point(shape = 1) +
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
    #norm_test <- shapiro.test(eval(substitute(var1), df))
    #norm_test_p <- round(norm_test$p.value,3)
    #norm_text <- paste0("p = ", norm_test_p)
    p <- ggplot2::ggplot(data = df, aes(sample={{ var1 }})) +
      #ggplot2::stat_qq(shape = 1) + ggplot2::stat_qq_line() +
      geom_point(stat = "qq", shape = 1) + geom_qq_line() +
      geom_exec(.stat_qq_confint, alpha = 0.2, conf.int.level = .95) +

      #annotate("text", x = 1, y=40, label = norm_text) +

      #qqplotr::stat_qq_band() +
      #qqplotr::stat_qq_line() +
      #qqplotr::stat_qq_point(shape = 1) +
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

    #pvals <- NULL

    #for(i in unique(eval(substitute(by1), df))){
    #  norm_test <- shapiro.test(eval(substitute(var1), df)[eval(substitute(by1), df)==i])
    #  #print(norm_test)
    #  norm_test_p <- round(norm_test$p.value,3)
    #  norm_text <- paste0("p = ", norm_test_p)
    #  print(norm_text)
    #  pvals <- c(pvals,norm_text)
    #}

    p <- ggplot2::ggplot(data = df, aes(sample={{ var1 }})) +
      #ggplot2::stat_qq(shape = 1) + ggplot2::stat_qq_line() +
      geom_point(stat = "qq", shape = 1) + geom_qq_line() +
      geom_exec(.stat_qq_confint, alpha = 0.2, conf.int.level = .95) +
      #qqplotr::stat_qq_band() +
      #qqplotr::stat_qq_line() +
      #qqplotr::stat_qq_point(shape = 1) +
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
      #ggplot2::stat_qq(shape = 1) + ggplot2::stat_qq_line() +
      geom_point(stat = "qq", shape = 1) + geom_qq_line() +
      geom_exec(.stat_qq_confint, alpha = 0.2, conf.int.level = .95) +
      #qqplotr::stat_qq_band() +
      #qqplotr::stat_qq_line() +
      #qqplotr::stat_qq_point(shape = 1) +
      facet_wrap(~group) +
      ggtitle(title) + labs(x = "Expected (Theoretical) Normal", y = laby) +
      theme_classic() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
            axis.line = element_line(colour = "black"), axis.text.x = element_text(vjust=0.5, colour="#000000"),
            axis.text.y = element_text(face="bold", colour="#000000"), plot.title = element_text(hjust = 0.5, lineheight=1.5, face="bold"))
  }
  return(p)
}


.qq_line <- function(data, qf, na.rm) {
  q.sample <- stats::quantile(data, c(0.25, 0.75), na.rm = na.rm)
  q.theory <- qf(c(0.25, 0.75))
  slope <- diff(q.sample) / diff(q.theory)
  intercept <- q.sample[1] - slope * q.theory[1]
  list(slope = slope, intercept = intercept)

}

StatQQLine <- ggproto("StatQQLine", Stat,
                      # http://docs.ggplot2.org/current/vignettes/extending-ggplot2.html
                      # https://github.com/hadley/ggplot2/blob/master/R/stat-qq.r
                      required_aes = c('sample'),
                      compute_group = function(data, scales,
                                               distribution = stats::qnorm,
                                               dparams = list(),
                                               conf.int.level = 0.95,
                                               na.rm = FALSE) {
                        qf <- function(p) do.call(distribution, c(list(p = p), dparams))
                        n <- length(data$sample)
                        P <- stats::ppoints(n)
                        theoretical <- qf(P)
                        qq <- .qq_line(data$sample, qf = qf, na.rm = na.rm)
                        line <- qq$intercept + theoretical * qq$slope
                        # Confidence interval
                        zz <- stats::qnorm(1 - (1 - conf.int.level)/2)
                        SE <- (qq$slope/stats::dnorm(theoretical)) * sqrt(P * (1 - P)/n)
                        fit.value <- qq$intercept + qq$slope * theoretical
                        ymax <- fit.value + zz * SE
                        ymin <- fit.value - zz * SE
                        data.frame(sample = line, x = theoretical, y = line, ymin = ymin, ymax = ymax)
                      }
)


.stat_qqline <- function(mapping = NULL, data = NULL, geom = "line",
                         position = "identity", ...,
                         distribution = stats::qnorm,
                         dparams = list(),
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         conf.int.level = 0.95) {
  layer(stat = StatQQLine, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(distribution = distribution,
                      dparams = dparams,
                      na.rm = na.rm, conf.int.level = conf.int.level, ...))
}


.stat_qq_confint <- function(mapping = NULL, data = NULL, geom = "ribbon",
                             position = "identity", ...,
                             distribution = stats::qnorm,
                             dparams = list(),
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = TRUE,
                             conf.int.level = 0.95) {
  layer(stat = StatQQLine, data = data, mapping = mapping, geom = geom,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(distribution = distribution,
                      dparams = dparams,
                      na.rm = na.rm, conf.int.level = conf.int.level, ...))
}

