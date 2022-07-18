#' Simplified STATA Predictive Margins
#'
#' This function returns a data frame with interactive margins and standard errors similar to those returned in the STATA margins call. The function can also return a margins plot.
#'
#' @import ggplot2 purrr dplyr
#' @importFrom rlang .data
#' @importFrom stats vcov
#' @importFrom plm plm fixef
#' @param mod a plm model object.
#' @param plot logical (default set to \code{FALSE}). When set to \code{plot = TRUE}, will return a an margins plot of the interaction terms.
#' @param error the number standard deviation units for which the margins will be calculated (default set to 2).
#' @return This function creates a data frame of predictive margins for the dependent variable, given values of the variables in the interaction.
#' @examples
#' library(plm)
#' data <- UCR2015
#' summary(mod <- plm(dui_pct ~ pct_poverty*pct_unemp +
#' income_inequality, data=data, index=c("state","county"),
#' model="within"))
#'
#' stata.plm.margins(mod)
#' @export

stata.plm.margins <- function(mod, plot=FALSE, error=NULL){
  if(is.null(error)){
    error <- 2
  }
  formula <- as.list(mod$call)[2][[1]]
  #df <- mod$call$data
  #df <- as.data.frame(df)
  df <- eval(mod$call$data)
  df <- as.data.frame(df)
  formula_s <- toString(formula)
  formula_s <- gsub("~", "", formula_s)
  y <- str_extract(formula_s, "(?<=\\, )(\\w+)") #look after first comma
  formula_s <- gsub(",", "", formula_s)
  x1 <- str_extract(formula_s, "(\\w+)(?= \\*)") #look before int asterisk
  x2 <- str_extract(formula_s, "(?<=\\* )(\\w+)") #look after int asterisk
  factorvar <- as.character(as.list(mod$call)[5][[1]][2])
  factorvar2 <- paste0(" + factor(",factorvar,")")
  formula2 <- as.character(formula)
  formula2 <- paste0(formula2[2]," ",formula2[1]," ",formula2[3],factorvar2)
  mod2 <- lm(formula2, eval(mod$call$data))
  lm_names <- names(mod2$coefficients)
  plm_names <- names(mod$coefficients)
  diff_names <- setdiff(lm_names,plm_names)
  diff_names_no_intercept <- diff_names[-c(1)]
  #vcovs
  vcov2 <- vcov(mod2)
  vcov2_intercept_rows <- vcov2[row.names(vcov2) %in% diff_names,]
  int_data_rows <- data.frame()
  for(i in 1:ncol(vcov2_intercept_rows)){
    othermeans <- mean(vcov2_intercept_rows[-c(1),i])
    sum1 <- sum(c(vcov2_intercept_rows[1,i],othermeans))
    int_data_rows[1,i] <- sum1
  }
  colnames(int_data_rows) <- colnames(vcov2_intercept_rows)
  row.names(int_data_rows) <- row.names(vcov2_intercept_rows)[1]
  int_data_rows <- int_data_rows[,!(colnames(int_data_rows) %in% diff_names_no_intercept),drop = FALSE]

  vcov2_intercept_cols <- vcov2[,colnames(vcov2) %in% diff_names]
  int_data_cols <- data.frame()
  for(i in 1:nrow(vcov2_intercept_cols)){
    othermeans2 <- mean(vcov2_intercept_cols[i,2:ncol(vcov2_intercept_cols)])
    sum2 <- sum(c(vcov2_intercept_cols[i,1],othermeans2))
    int_data_cols[i,1] <- sum2
  }
  row.names(int_data_cols) <- row.names(vcov2_intercept_cols)
  colnames(int_data_cols) <- colnames(vcov2_intercept_cols)[1]
  int_data_cols <- int_data_cols[!(row.names(int_data_cols) %in% diff_names_no_intercept),,drop = FALSE]
  #
  vcov_new <- vcov2
  vcov_new <- vcov_new[!(row.names(vcov_new) %in% diff_names_no_intercept),]
  vcov_new <- vcov_new[,!(colnames(vcov_new) %in% diff_names_no_intercept)]
  vcov_new <- vcov_new
  vcov_new[1,] <- as.matrix(int_data_rows)
  vcov_new[,1] <- as.matrix(int_data_cols)
  var1 <- df[, x1]
  var2 <- df[, x2]
  #var1 <- eval(mod$call$data)[, x1]
  #var2 <- eval(mod$call$data)[, x2]
  constants <- fixef(mod)
  constants <- as.data.frame(constants)
  constants$factorvar <- rownames(constants)
  colnames(constants) <- c("constants",factorvar)
  u_id <- fixef(mod, type = "dmean") # fixed effects for each id.
  u_id <- as.data.frame(u_id)
  u_id$u_id <- (-1*u_id$u_id)
  u_id$factorvar <- rownames(u_id)
  colnames(u_id) <- c("u_id",factorvar)
  #print(u_id)
  #
  error_terms <- df %>% dplyr::select(factorvar)
  if(is.character(constants[,2])){
    #print("is character")
    error_terms[,1] <- as.character(error_terms[,1])
  }
  #error_terms$factorvar <- as.factor(error_terms$factorvar)
  error_terms <- error_terms %>% dplyr::left_join(u_id, by=factorvar)
  error_terms <- error_terms %>% dplyr::left_join(constants, by=factorvar)
  mean_u_id <- mean(error_terms$u_id, na.rm = T)
  mean_constants <- mean(error_terms$constants, na.rm = T)
  pred_data <- data.frame(
    var1_at <- c( rep(-error*(sd(var1)), 3), rep(0, 3), rep((error*(sd(var1))), 3)),
    var2_at <- rep(c((-error*(sd(var2))), 0, (error*(sd(var2)))), 3)
  )
  names(pred_data) <- c("x1", "x2")
  pred_data$id <- 1
  y_hat_mod <- as.numeric(mod$model[[1]] - mod$residuals)
  l1 <- as.list(mod$coefficients)
  vars_in_model <- names(mod$coefficients) #list of variables in model
  vars_in_model <- vars_in_model[-c(NROW(vars_in_model))] #list of variables in model but remove interaction term

  df2 <- df %>% dplyr::select(
    all_of(vars_in_model)) %>%
    summarise(across(everything(), list(mean)))
  names(df2) <- vars_in_model
  df2$id <- 1
  pred_data <- pred_data %>% left_join(df2, by = "id")
  pred_data <- pred_data[-c(3,4,5)]
  pred_data$x1x2 <- (pred_data$x1)*(pred_data$x2)
  calc_y <- data.frame()
  for(i in 1:nrow(pred_data)){
    for(j in 1:ncol(pred_data)){
      val = (l1[j][[1]]*pred_data[i,j]) #multiply mean of each var by the slope/coefficient
      calc_y[i,j] <- val
    }
  }
  calc_y <- calc_y %>%
    dplyr::rowwise() %>%
    dplyr::mutate(calc_y_no_errors = sum(across(starts_with("V")), na.rm = T))
  calc_y <- calc_y %>%
    dplyr::rowwise() %>%
    dplyr::mutate(y_hat = sum(.data$calc_y_no_errors, mean_constants, mean_u_id))
    #dplyr::mutate(y_hat = sum(calc_y_no_errors, mean_constants, mean_u_id))
  y_hat <- calc_y %>% dplyr::select(y_hat)
  margins_vals <- pred_data %>% dplyr::select(x1,x2)
  margins_plot <- dplyr::bind_cols(margins_vals,y_hat)
  numrows <- nrow(margins_plot)
  v <- vcov_new
  jac_intercept <- data.frame(
    rep(1,numrows)
  )
  names(jac_intercept) <- "(Intercept)"
  j <- as.data.frame(jac_intercept)
  j <- j %>% dplyr::bind_cols(pred_data)
  j <- as.matrix(j)
  delta_mat <- j %*% v %*% t(j)
  se <- as.data.frame(sqrt(diag(delta_mat)))
  names(se) <- "se"
  names(margins_plot) <- c(x1,x2,"y_hat")
  margins_plot <- as.data.frame(margins_plot)
  margins_plot <- margins_plot %>% dplyr::bind_cols(se)
  margins_plot[,1][margins_plot[,1]==min(margins_plot[,1])] <- -1*error
  margins_plot[,1][margins_plot[,1]==0] <- 0
  margins_plot[,1][margins_plot[,1]==max(margins_plot[,1])] <- 1*error
  margins_plot[,1] <- as.numeric(margins_plot[,1])
  margins_plot[,2][margins_plot[,2]==0] <- "Mean"
  margins_plot[,2][margins_plot[,2]==min(margins_plot[,2])] <- "Low"
  #margins_plot[,2][margins_plot[,2]==max(margins_plot[,2])] <- "High" #old way
  margins_plot[,2][margins_plot[,2]==min(margins_plot[,2])] <- "High" #because, rewritten as character, so the only number left is the min. the max is the character string.
  #dfname <- deparse(substitute(margins_plot))
  #return(margins_plot)

  margins_df <- "margins_df"
  pos <- 1
  envir = as.environment(pos)
  assign(margins_df, margins_plot, envir = envir)

  if(plot==TRUE){
    ylabel <- paste0("Predicted Outcome\n",y)
    margins_df<-as.data.frame(margins_plot)
    v1 <- margins_df[,x1]
    v2 <- margins_df[,x2]
    plotlimits <- c((margins_df$y_hat+(-1.96*margins_df$se)),(margins_df$y_hat+(1.96*margins_df$se)))
    low <- min(plotlimits)
    low <- floor(low/5)*5
    high <- max(plotlimits)
    high <- ceiling(high/5)*5
    ggplot2::ggplot(margins_df, aes(x= v1, y= y_hat, #color=factor(c_gini),
                                    shape= v2, linetype= v2)) +
      geom_line() +
      geom_point(size = 2) +
      labs(#title = "Predictive Margins (with 95% CI) for\nPercent Supporting Legalization",
        x = x1,
        #y = "Predicted Outcome",
        y = ylabel,
        #color = "Income\nInequality",
        shape = x2,
        linetype = x2) +
      scale_linetype_discrete(breaks=c('Low', 'Mean', 'High')) +
      scale_shape_discrete(breaks=c('Low', 'Mean', 'High')) +
      geom_errorbar(aes(ymin = y_hat+(-1.96*se), ymax = y_hat+(1.96*se)), data = margins_df, width = 0.2) +
      theme(plot.title = element_text(size=12)) +
      ylim(low, high) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            #axis.line = element_line(colour = "black")
      ) +
      theme(panel.grid.major.y = element_line(color = "grey",
                                              size = 0.25,
                                              linetype = 1)) +
      theme(axis.line.x = element_line(color="black", size = .75),
            axis.line.y = element_line(color="black", size = .75),
            plot.title = element_text(hjust = 0.5))
  }
}
