#' Simplified Chi Square
#'
#' This function simplifies the call for Pearson's Chi Square test (chi.sq) on a given data frame.
#' @importFrom stats chisq.test pchisq qchisq qnorm p.adjust
#' @param df data frame to read in.
#' @param var1 the dependent/outcome variable, \eqn{Y}.
#' @param var2 the main independent/predictor variable, \eqn{X}.
#' @param correct logical (default set to \code{F}). When set to \code{correct = T}, will employ Yates' continuity correction (for data that violate the normality assumption).
#' @param post logical (default set to \code{F}). When set to \code{post = T}, will return results of post-hoc (Z) tests of the standardized residual for each cell (the standardized difference between observed and expected frequencies), using Bonferroni's alpha adjustment, and returns an adjusted p-value for each cell/comparison.
#' @param cramer logical (default set to \code{F}). When set to \code{post = T}, will return results of Cramer's V, a measure of the strength of the association between the two variables.
#' @return This function returns the summary results table for a Pearson's Chi Square test, examining the relationship between \code{var1} from data frame \code{df}, and \code{var2}.
#' @export
#' @examples
#' data <- mtcars
#'
#' x2 <- chi.sq(data,vs,am)
#' summary(x2)
#'

chi.sq <- function(df, var1, var2, correct = FALSE, post = FALSE, cramer = FALSE){
  #options(scipen=999)
  #suppressWarnings({})
  v1 <- deparse(substitute(var1))
  v2 <- deparse(substitute(var2))
  {suppressWarnings(model <- chisq.test(eval(substitute(var1), df), eval(substitute(var2), df), correct = FALSE))}
  modname <- model$method
  df_1 <- model$parameter[[1]]
  x2_val <- as.numeric(model$statistic)
  row <- nrow(model$observed)
  col <- ncol(model$observed)
  n_cases <- min(sum(!is.na(eval(substitute(var1), df))),sum(!is.na(eval(substitute(var2), df))))
  crit <- qchisq(p=.95, df=df_1)
  ch_text <- "\u03C7\U00B2"
  ch_c_text <- paste0("Critical ",ch_text)
  model$statistic[2] <- round(crit, 3)
  names(model$statistic) <- c(ch_text,ch_c_text)
  compare.name <- paste0(deparse(substitute(var2)),"-",deparse(substitute(var1)))

  if(correct == TRUE){
    {suppressWarnings(model <- chisq.test(eval(substitute(var1), df), eval(substitute(var2), df), correct = TRUE))}
    modname <- model$method
    df_1 <- model$parameter[[1]]
    crit <- qchisq(p=.95, df=df_1)
    ch_text <- "\u03C7\U00B2"
    ch_c_text <- paste0("Critical ",ch_text)
    model$statistic[2] <- round(crit, 3)
    names(model$statistic) <- c(ch_text,ch_c_text)
  }

  model$data.name <- paste0(deparse(substitute(var1))," and ", deparse(substitute(var2)))

  #print(model$residuals)
  #print(model$stdres)

  post_hoc<-matrix(,nrow=0,ncol=0)
  cramers<-matrix(,nrow=0,ncol=0)
  #newList <- model

  if(cramer == TRUE){
    denom <- min((col-1),(row)-1)
    cramers_v <- sqrt((x2_val/n_cases)/(denom))
    model$cramers <- cramers_v
    cram <- matrix(,nrow=1,ncol=1)
    cram[,1] <- cramers_v
    #rownames(out.mat) <- model$method
    rownames(cram) <- ""
    colnames(cram) <- c("Cramer's V")
    cramers <- cram
  }

  if(post == TRUE){
    method = "bonferroni"
    round = 2
    stdres <- as.matrix(model$stdres)
    #get dim num of matrix
    dim(stdres)
    #get dim num of matrix rows (dv)
    v1num <- dim(stdres)[1] #dv
    #get dim num of matrix cols (iv)
    v2num <- dim(stdres)[2] #iv
    #get attributes of rows, in order (this is DV)
    v1names <- dimnames(stdres)[[1]] #dv
    #get attributes of columns, in order (this is IV)
    v2names <- dimnames(stdres)[[2]] #iv
    #get chi square values from standardized residuals, by squaring the values in the matrix
    chisq_values <- stdres^2
    #get pvalues for chi square
    p_vals <- pchisq(chisq_values, 1, lower.tail = FALSE)
    #crit <- qchisq(p=.95, df=1)
    #crit_round <- round(crit, 2)
    #z_crit <- sqrt(crit)
    #print(z_crit)
    adj_p_vals <- p_vals
    #print(p_vals)
    for (i in 1:nrow(adj_p_vals)) {
      adj_p_vals[i, ] <- p.adjust(
        adj_p_vals[i, ],
        method = method,
        n = ncol(adj_p_vals) * nrow(adj_p_vals)
      )
    }

    z_crit <- qnorm(p=1-((.05/2)/(  ncol(adj_p_vals) * nrow(adj_p_vals)  )))
    z_crit_round <- round(z_crit, 2)
    crit <- z_crit^2
    crit_round <- round(crit, 2)
    #print(z_crit_round)
    #print(crit_round)


    v1_names <- rep(v1names, v2num) #dv
    v2_names <- NULL #iv
    for(v2 in v2names){
      v2_names_pre <- rep(v2, v1num) #iv
      v2_names <- c(v2_names,v2_names_pre)
    }
    bonf <-
      as.data.frame(matrix(
        data = NA,
        #nrow = nrow(adj_p_vals) * 2,
        #ncol = ncol(adj_p_vals) + 2
        nrow = nrow(adj_p_vals) * ncol(adj_p_vals),
        #ncol = 4
        ncol = 2
      ))

    stdresvals <- NULL
    adjpvals <- NULL
    chi2vals <- NULL
    for(i in 1:v2num){
      for(j in 1:v1num){
        stdresvals <- c(stdresvals,stdres[j,i])
        adjpvals <- c(adjpvals,adj_p_vals[j,i])
        chi2vals <- c(chi2vals,chisq_values[j,i])
      }
    }

    chi2vals_round <- round(chi2vals, 2)
    stdresvals_round <- round(stdresvals, 2)

    #bonf[,1] <- as.character(v2_names) #iv
    #bonf[,2] <- as.character(v1_names) #dv
    #bonf[,3] <- stdresvals
    #bonf[,4] <- as.numeric(adjpvals)
    bonf[,1] <- stdresvals
    bonf[,2] <- adjpvals

    #colnames(bonf) <- c(deparse(substitute(var2)), deparse(substitute(var1)), "Adj. Standardized Residual (Z)", "Chi Square", "Adj. p-value")
    #colnames(bonf) <- c(deparse(substitute(var2)), deparse(substitute(var1)), "Adj. Standardized Residual (Z)", "p-value")
    colnames(bonf) <- c("Standardized Residual (Z)", "p-value")

    #bonf2 <-
    #  as.data.frame(matrix(
    #    data = NA,
    #    #nrow = nrow(adj_p_vals) * 2,
    #    #ncol = ncol(adj_p_vals) + 2
    #    nrow = nrow(adj_p_vals) * ncol(adj_p_vals),
    #    ncol = 2
    #  ))

    grouping <- paste0(v2_names, " * ", v1_names)

    #bonf2[,1] <- grouping #group
    #bonf2[,2] <- chi2vals #chi2
    #bonf2[,3] <- adjpvals

    #bonf3 <- bonf2
    #bonf3[,4] <- chi2vals_round
    #bonf3[,5] <- stdresvals_round



    #comparison <- paste0(deparse(substitute(var2)),"-",deparse(substitute(var1)))

    post_hoc <- bonf
    #post_hoc[,1] <- v2_names #iv
    #post_hoc[,2] <- v1_names #dv
    #post_hoc[,3] <- stdresvals
    #bonf[,4] <- chi2vals
    #bonf[,5] <- adjpvals
    #post_hoc[,4] <- adjpvals
    #post_hoc[,1] <- as.numeric(stdresvals)
    #post_hoc[,2] <- as.numeric(adjpvals)
    rownames(post_hoc) <- paste0(v2_names,"-",v1_names)

    #post_hoc <- post_hoc[,3:4]





    #if(plot == TRUE){
      #plotmax <- nrow(bonf3)
      #title <- paste0("Bar Chart of ", ch_text, " Values")
      #labx <- "Intersection of Categories"
      #laby <- paste0(ch_text," Value")
      #bonf2 <- as.data.frame(bonf3)
      #p <- ggplot2::ggplot(data = bonf3, aes(x=as.factor(grouping), y = chi2vals)) +
      ##ggplot2::ggplot(data = bonf3, aes(x=as.factor(grouping), y = chi2vals)) +
      #  geom_bar(stat = "identity") +
      #  scale_fill_brewer(palette="Blues") +
      #  #geom_text(stat='identity', aes(label=paste0("n = ",Frequency)), vjust=-1) +
      #  ggtitle(title) + xlab(labx) + ylab(laby) +
      #  geom_hline(yintercept = crit, color="red") +
      #  geom_text(aes(plotmax, crit, label = paste0("Critical ", ch_text, " = ", crit_round), vjust = -1), color = "red") +
      #  geom_label_repel(stat='identity', aes(label = paste0(ch_text, " = ", chi2vals_round, "\nZ = ", stdresvals_round)),
      #                   size = 4, nudge_x = 1, show.legend = FALSE) +
      #  theme_classic() +
      #  theme(axis.text.x = element_text(angle = 65)) +
      #  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),
      #        axis.line = element_line(colour = "black"), axis.text.x = element_text(vjust=0.5, colour="#000000"),
      #        axis.text.y = element_text(face="bold", colour="#000000"), plot.title = element_text(hjust = 0.5, lineheight=1.5, face="bold"))

      #newList <- list(model = model, "Post-Hoc Test w/ Bonferroni Adjustment" = bonf, plot = p)
      #return(newList)
      #print(p)
      #return(p)
  #}
  }

  out.mat <- matrix(, nrow = 1, ncol = 4)
  out.mat[,1] <- model$statistic[1]
  out.mat[,2] <- model$statistic[2]
  out.mat[,3] <- df_1
  out.mat[,4] <- model$p.value
  #rownames(out.mat) <- model$method
  rownames(out.mat) <- ""
  colnames(out.mat) <- c(ch_text, ch_c_text, "df","p-value")

  return.obj <- list(call = match.call(), results = out.mat, name = model$method, comparison = compare.name,
                     post_hoc = post_hoc, cramers = cramers)
  class(return.obj) <- "chisquare"
  return.obj

  #return(model)
}
