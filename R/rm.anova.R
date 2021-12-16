#' Simplified One-Way Repeated Measures Analysis of Variance
#'
#' This function simplifies the call for repeated measures ANOVA (rm.anova) on a given data frame. Also allows calls for sphericity correction (correct), as well as a sphericity test table (sph).
#' @import dplyr rstatix stringr
#' @importFrom stats time
#' @importFrom rlang .data
#' @param df data frame to read in.
#' @param id the main grouping variable by which \code{times} will be analyzed
#' @param times dependent variable values at the time points measured. Read in as a list of time point variables (e.g. \code{c("t1", "t2", "t3", ..., "tn")}), where the values represent the scores at the various time points. Read in as list if data are in wide form. If data are in long form, the \code{times} variable is one column (rather than multiple columns) in the data frame, and must be paired with the \code{scores} variable for actual values (listed below).
#' @param scores if data are in long form (where each group has multiple observations), a \code{scores} variable must be read in, which represents the values at the specific time points represented in the \code{times} variable.
#' @param correct logical (default set to \code{T}). Corrects the results in the repeated measures ANOVA table -- adjusts the degrees of freedom (\eqn{df}) by multiplying the sphericity assumed degrees of freedom (\eqn{df}) by the Greenhouse-Geisser Epsilon value. When set to \code{correct = F}, will print results of repeated measures ANOVA with sphericity assumed.
#' @param sph logical (default set to \code{F}). When set to \code{sph = T}, will print a sphericity tests table with Mauchly's W, as well as two Epsilon values (Greenhouse-Geisser and Huynh-Feldt).
#' @param phc logical (default set to \code{F}). When set to \code{sph = T}, will print a post-hoc comparisons table with Bonferroni's adjusted alpha levels (and p-values).
#' @examples
#' data <- howell_aids_wide
#' rm.anova(data, student, c("t1","t2","t3"))
#'
#' data2 <- howell_aids_long
#' rm.anova(data2, student, time, scores=knowledge)
#' @export



rm.anova <- function(df, id, times, scores=NULL, correct=TRUE, sph=FALSE, phc=FALSE){
  #utils::globalVariables(c("score","dupe"))
  df <- as.data.frame(df)
  df <- df %>%
    dplyr::mutate(dupe=ifelse(duplicated({{ id }}),1, ifelse(!duplicated({{ id }}),0,NA)))
  if(sum(df$dupe)==0){ #in wide form, to convert to long form
    df <- df %>%
      dplyr::select(-.data$dupe) %>%
      #  gather(key = "time", value = "score", {{ v1 }}, {{ v2 }}, {{ v3 }}, {{ v4 }}) %>%
      gather(key = "time", value = "score", times) %>% #score is time component, value is outcome
      convert_as_factor({{ id }}, time) %>% #break out id and time component
      dplyr::select({{ id }}, time, .data$score)
    model <- anova_test(data = df, dv=.data$score, wid={{ id }}, within=time, effect.size = "pes") #score is time, value is the outcome value at that time
    print(model)
    }
  if(sum(df$dupe)>0){ #already in long form
    df <- df %>%
      dplyr::select(-.data$dupe) %>%
      convert_as_factor({{ id }}, {{ times }}) %>%
      dplyr::select({{ id }}, {{ times }}, {{ scores }}) %>%
      dplyr::rename(time = {{ times }}, score = {{ scores }})
    #model <- anova_test(data = df, dv={{ scores }}, wid={{ id }}, within={{ times }}, effect.size = "pes")
    model <- anova_test(data = df, dv=.data$score, wid={{ id }}, within=time, effect.size = "pes") #score is time, value is the outcome value at that time
  }
  model$ANOVA[[6]] <- ""
  model$ANOVA[[6]][model$ANOVA[[5]]<=.05] <- "*"
  model$ANOVA[[6]][model$ANOVA[[5]]<=.01] <- "**"
  model$ANOVA[[6]][model$ANOVA[[5]]<=.001] <- "***"
  model$ANOVA <- rbind(model$ANOVA,model$ANOVA)
  model$ANOVA[[1]][2] <- "error"
  model$ANOVA[[2]][2] <- model$ANOVA[[3]][2]
  model$ANOVA <- model$ANOVA[-3]
  #names(model$ANOVA) <- c("", "df", "F", "p", "sig.", "eta^2")
  model$ANOVA[[3]][2] <- ""
  model$ANOVA[[4]][2] <- ""
  model$ANOVA[[5]][2] <- ""
  model$ANOVA[[6]][2] <- ""
  model$ANOVA[[7]] <- paste(model$ANOVA[[4]],model$ANOVA[[5]])
  model$ANOVA <- model$ANOVA[, c(1, 2, 3, 7, 6, 4, 5)]
  model$ANOVA <- model$ANOVA[,-c(6,7)]
  names(model$ANOVA) <- c("", "df", "F", "p-value", "eta^2")
  model$sph <- model[2]
  model$eps <- model[3]
  names(model$sph) <- "Sphericity Test"
  names(model$eps) <- "Corrections"
  model$eps[[1]][5][[1]] <- ""
  model$eps[[1]][5][[1]][model$eps[[1]][4][[1]]<=.05] <- "*"
  model$eps[[1]][5][[1]][model$eps[[1]][4][[1]]<=.01] <- "**"
  model$eps[[1]][5][[1]][model$eps[[1]][4][[1]]<=.001] <- "***"
  model$sph[[1]][5] <- paste(model$sph[[1]][3],model$sph[[1]][4])
  model$sph[[1]] <- model$sph[[1]][, c(1, 2, 5, 3, 4)]
  model$sph[[1]] <- model$sph[[1]][,-c(4,5)]
  model$sph[[1]][4] <- model$eps[[1]][2]
  model$sph[[1]][5] <- model$eps[[1]][6]
  names(model$sph[[1]]) <- c("", "Mauchly's W", "p-value","Greenhouse-Geisser", "Huynh-Feldt")
  c_df <- str_split(model$eps[[1]][3][[1]], ", ")
  note <- "---\nSignif. codes: '***' 0.001 '**' 0.01 '*' 0.05\nNote: Model with Sphericity Assumed"
  if(correct==TRUE){
    if(as.numeric(model$sph[[1]][4][1])<.700){
      model$ANOVA[[2]][1] <- as.numeric(c_df[[1]][1][1]) #df1
      model$ANOVA[[2]][2] <- as.numeric(c_df[[1]][2][1])#df2
      model$eps[[1]][10] <- paste(model$eps[[1]][4],model$eps[[1]][5])
      model$ANOVA[[4]][1] <- model$eps[[1]][10][[1]]
      model$eps <- model$eps[[1]][,-10]
      note <- "---\nSignif. codes: '***' 0.001 '**' 0.01 '*' 0.05\nNote: Model with Greenhouse-Geisser Adjusted df"
    }
  }
  if(sph==TRUE){
    cat("\nSphericity Tests\n\n")
    print(model$sph[[1]], , row.names = FALSE)
    cat("\n")
  }

  cat("\nRepeated Measures ANOVA\n\n") #(type III tests)
  #return(get_anova_table(model))
  print(model$ANOVA, row.names = FALSE)
  cat(note)
  cat("\n")

  if(phc==TRUE){
    cat("\nPost-Hoc Comparisons\n\n")
    pwc <- df %>%
      pairwise_t_test(
        score ~ time, paired = TRUE,
        p.adjust.method = "bonferroni"
      )
    pwc <- as.data.frame(pwc)
    pwc[[10]] <- ""
    pwc[[10]][pwc[[9]]<=.05] <- "*"
    pwc[[10]][pwc[[9]]<=.01] <- "**"
    pwc[[10]][pwc[[9]]<=.001] <- "***"
    pwc[[11]] <- paste0(pwc[[9]]," ",pwc[[10]])
    pwc[[11]]
    pwc <- pwc[, c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
    pwc <- pwc[,-c(1,9,10,11)]
    names(pwc) <- c("Group 1","Group 2","N (Group 1)","N (Group 2)", "t", "df", "p-value (Bonferroni Adjusted)" )
    print(pwc, row.names = FALSE)
    #print(model$sph[[1]], , row.names = FALSE)
    cat("\n")
  }
}

