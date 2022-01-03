#' Calculating Ideological Distance
#'
#' This function calculates ideological distance scores based on the calculation created by Grossback et al. (2004) and clarified by Cruz-Aceves and Mallinson (2019). This calculation is based on state ideology data (by year) provided by Richard C. Fording (\url{https://rcfording.com/state-ideology-data/}) and used in Berry et al. (1998). This function can be applied to any unit of analysis and time level for any type of policy adoption.
#' @import dplyr
#' @importFrom stats time
#' @param df data frame to read in. This should be an adapted version of the \code{Ideology} data set provided in the package. The adapted version should include an outcome variable measuring the policy adoption of choice.
#' @param id the grouping variable, usually states
#' @param ideology the state's ideology score variable (either \emph{state} or \emph{citizen} ideology) in a given year. These data come from Richard C. Fording (\url{https://rcfording.com/state-ideology-data/}) as used in Berry et al. (1998), and are measured, for each state, from 1960 to 2018.
#' @param time the time variable, at which the ideology score is measured. These data come from Richard C. Fording (\url{https://rcfording.com/state-ideology-data/}) as used in Berry et al. (1998), and are measured, for each state, from 1960 to 2018.
#' @param adoption binary, user-defined measure of the status of policy adoption in a state in a given year. \code{0} equates to \emph{policy not adopted in the year, for the state}, \code{1} equates to\emph{policy is adopted in the year, for the state} -- a value of \code{1} should only exist for a state in the year it was adopted (e.g. not every year thereafter). The example below relies on ERA ratification data from Soule and King (2006), but the user should include the measure of adoption of their choice.
#' @examples
#' data <- Ideology_ERA
#'
#' ideo.dist(data, state, s_ideo, year, era_status)
#' @export


ideo.dist <- function(df, id, ideology, time, adoption){
  df2 <- as.data.frame(df)
  df2$ideo_distance_s <- NA
  #options(warn=-1)
  for (i in 1:length(eval(substitute({{ id }}), df2))) {
    p_time <- data.frame(eval(substitute({{ time }}), df2)[eval(substitute({{ adoption }}), df2)==1 & eval(substitute({{ time }}), df2)<eval(substitute({{ time }}), df2)[i]])
    p_ideo <- data.frame(eval(substitute({{ ideology }}), df2)[eval(substitute({{ adoption }}), df2)==1 & eval(substitute({{ time }}), df2)<eval(substitute({{ time }}), df2)[i]])
    p_id <- data.frame(eval(substitute({{ id }}), df2)[eval(substitute({{ adoption }}), df2)==1 & eval(substitute({{ time }}), df2)<eval(substitute({{ time }}), df2)[i]])
    names(p_id) <- "p_id"
    names(p_time) <- "p_time"
    names(p_ideo) <- "p_ideo"
    scores <- dplyr::bind_cols(p_id,p_time,p_ideo)
    names(scores) <- c("id","t","ideo")
    max_time <- {suppressWarnings(max(eval(substitute(t), scores)))}
    min_time <- {suppressWarnings(min(eval(substitute(t), scores)))}
    recent_scores <- scores %>% dplyr::filter(t==max_time)
    older <- scores %>% dplyr::filter(t==min_time)
    older_scores <- dplyr::setdiff(scores,recent_scores)
    df2$last_adopter_ideo[i] <- (sum(eval(substitute(ideo), recent_scores))/(length(eval(substitute(ideo), recent_scores))))
    df2$allother_adopter_ideo[i] <- (sum(eval(substitute(ideo), older_scores))/(length(eval(substitute(ideo), older_scores))))
  }
  df2$last_adopter_ideo[is.nan(df2$last_adopter_ideo)] <- 0
  df2$allother_adopter_ideo[is.nan(df2$allother_adopter_ideo)] <- 0
  df2$ideo_distance_s <- abs(((eval(substitute(last_adopter_ideo), df2) + eval(substitute(allother_adopter_ideo), df2))/2) - eval(substitute(ideology), df2))
  dfname <- deparse(substitute(df))
  pos <- 1
  envir = as.environment(pos)
  #assign("trellis.par.theme", trellis.par.get(), envir = envir)
  assign(dfname, data.frame(df2), envir = envir)
  #assign(dfname, data.frame(df2), envir = .GlobalEnv)
  #assign(dfname, data.frame(df2), envir = .GlobalEnv)
  #dfname <<- data.frame(df2)
}

