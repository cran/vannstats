#' Chi Square Plot
#'
#' This function plots a cell-by-cell chart (chiplot) on a given matrix of standardized residuals (Z) from a chi square test.
#' @import ggplot2 reshape2
#' @importFrom stats na.omit
#' @param zmat a matrix of standardized residuals, extracted from the chi square test, to visualize.
#' @param method the type of visualization used in plot (default set to \code{"square"}).
#' @param type the section of the plot that will be displayed (default set to \code{"full"}).
#' @param ggtheme \code{ggplot2} function to set the theme of the plot (default set to \code{ggplot2::theme_classic()}).
#' @param title title of the plot, extracted from the chi square test.
#' @param show.legend logical (default set to \code{TRUE}), to display legend on plot.
#' @param legend.title title of the legend (default set to \code{"Z"}).
#' @param colors a vector of 3 colors for negative, zero, and positive residuals.
#' @param outline.color the outline color of the square (default set to \code{"white"}).
#' @param lab logical (default set to \code{FALSE}, but set to \code{TRUE} if extracted from chi square test), to display standardized residual (Z) value (and significance stars) on for each cell comparison on the plot.
#' @param lab_col color of text displayed in each cell comparison for standardized residual (Z) value when \code{lab = TRUE} (default set to \code{"black"}).
#' @param lab_size size of text displayed in each cell comparison for standardized residual (Z) value when \code{lab = TRUE} (default set to \code{4}).
#' @param p.mat a matrix of p-values for each standardized residual (Z) value comparison, extracted from chi square test.
#' @param z.crit the critical Z value for the comparison of standardized residuals (Z), extracted from the chi square test.
#' @param sig.level the alpha value used to assess significance (default set to \code{0.05}).
#' @param insig glyphs to add on non-significant standardized residual (Z) values (default set to \code{"pch"}).
#' @param pch glyphs added to non-significant standardized residual (Z) values (default set to \code{4}).
#' @param pch.col color of pch glyphs (default set to \code{"black"}).
#' @param pch.cex size of pch glyphs (default set to \code{5}).
#' @param tl.cex size of text label for each category name for both variables (default set to \code{12}).
#' @param tl.col color of text label for each category name for both variables (default set to \code{"black"}).
#' @param tl.srt string rotation of text label for each category name for the variable on the x-axis (default set to \code{45}).
#' @param digits the number of decimal digits to be displayed in the plot (default set to \code{2}, but set to \code{3} if extracted from chi square test).
#' @param as.is determines how to handle dimnames, either left as strings (if set to \code{TRUE}), or converted (default set to \code{FALSE}).
#' @return This function returns the chi square plot of standardized residuals for categories of \code{var1} by \code{var2} in data frame \code{df}, all of which are extracted from the chi square test.
#' @examples
#' data <- mtcars
#'
#' x2 <- chi.sq(data,vs,am,post=TRUE)
#'
#' chiplot(zmat = x2$z_mat,p.mat = x2$p_z_mat,z.crit = round(x2$z_crit, 4),
#' outline.color = "white",lab = TRUE,digits = 3)
#' @export


chiplot <- function(zmat,
                    method = "square",
                    type = "full",
                    ggtheme = ggplot2::theme_classic(),
                    title = "",
                    show.legend = TRUE,
                    legend.title = "Z",
                    colors = c("dodgerblue3", "white", "firebrick2"),
                    outline.color = "white",
                    lab = FALSE,
                    lab_col = "black",
                    lab_size = 4,
                    p.mat = NULL,
                    z.crit = NULL,
                    sig.level = 0.05,
                    insig = pch,
                    pch = 4,
                    pch.col = "black",
                    pch.cex = 5,
                    tl.cex = 12,
                    tl.col = "black",
                    tl.srt = 45,
                    digits = 2,
                    as.is = FALSE) {
  type <- match.arg(type)
  method <- match.arg(method)

  if (!is.matrix(zmat) & !is.data.frame(zmat)) {
    stop("Need a matrix or data frame!")
  }
  zmat <- as.matrix(zmat)

  zmat <- base::round(x = zmat, digits = digits)

  # Melt zmat and pmat
  zmat <- reshape2::melt(zmat, na.rm = TRUE, as.is = as.is)
  colnames(zmat) <- c("Var1", "Var2", "value")
  zmat$Var1 <- as.character(zmat$Var1)
  zmat$Var2 <- as.character(zmat$Var2)
  zmat$pvalue <- rep(NA, nrow(zmat))
  zmat$signif <- rep(NA, nrow(zmat))

  if (!is.null(p.mat)) {
    p.mat <- reshape2::melt(p.mat, na.rm = TRUE)
    zmat$coef <- zmat$value
    zmat$pvalue <- p.mat$value
    zmat$signif <- as.numeric(p.mat$value <= sig.level)
    p.mat <- subset(p.mat, p.mat$value > sig.level)
  }


  zmat$abs_zmat <- abs(zmat$value) * 100

  mat_min <- min(zmat$value)
  mat_max <- max(zmat$value)

  #print(zmat$abs_zmat)

  #ch_text <- "\u03C7\U00B2"
  #ch_c_text <- paste0("Critical ",ch_text)
  #z_crit <- qnorm(p=1-((.05/2)/(  ncol(adj_p_vals) * nrow(adj_p_vals)  )))
  #z_crit_round <- round(z_crit, 2)
  #z_crit <- round(z_crit,3)
  z_crit <- z.crit
  total_critical_text <- paste0("\n","Critical Z = ",z_crit,"\n")

  #legend.title <- paste0(legend.title,"\n",total_critical_text)

  #print(dim(zmat))
  #b[,4] <- as.numeric(b[,4])
  sig <- rep("", length(zmat[,4]))
  zmat <- cbind(zmat,sig)
  zmat[,8][zmat[,4]<=.05] <- "*"
  zmat[,8][zmat[,4]<=.01] <- "**"
  zmat[,8][zmat[,4]<=.001] <- "***"
  full_lab <- rep(NA, length(zmat[,4]))
  zmat <- cbind(zmat,full_lab)
  zmat[,9] <- paste0(zmat[,3]," ",zmat[,8])

  # heatmap
  p <-
    ggplot2::ggplot(
      data = zmat,
      mapping = ggplot2::aes_string(x = "Var1", y = "Var2", fill = "value")
    )

  # modification based on method
  if (method == "square") {
    p <- p +
      ggplot2::geom_tile(color = outline.color)
  } else if (method == "circle") {
    p <- p +
      ggplot2::geom_point(
        color = outline.color,
        shape = 21,
        ggplot2::aes_string(size = "abs_zmat")
      ) +
      ggplot2::scale_size(range = c(4, 10)) +
      ggplot2::guides(size = "none")
  }

  # adding colors
  p <- p + ggplot2::scale_fill_gradient2(
    low = colors[1],
    high = colors[3],
    mid = colors[2],
    midpoint = 0,
    limit = c(mat_min, mat_max),
    space = "Lab",
    name = legend.title
  )

  # depending on the class of the object, add the specified theme
  if (class(ggtheme)[[1]] == "function") {
    p <- p + ggtheme()
  } else if (class(ggtheme)[[1]] == "theme") {
    p <- p + ggtheme
  }


  p <- p +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = tl.srt,
        vjust = 1,
        size = tl.cex,
        hjust = 1
      ),
      axis.text.y = ggplot2::element_text(size = tl.cex)
    ) +
    ggplot2::coord_fixed()

  label <- round(x = zmat[, "value"], digits = digits)
  if (!is.null(p.mat) & insig == "blank") {
    ns <- zmat$pvalue > sig.level
    if (sum(ns) > 0) label[ns] <- " "
  }

  label <- paste0(label," ",zmat[, "sig"])

  # matrix cell labels
  if (lab) {
    p <- p +
      ggplot2::geom_text(
        mapping = ggplot2::aes_string(x = "Var1", y = "Var2"),
        label = label,
        color = lab_col,
        size = lab_size
      )
  }

  p <- p +
    #labs(title = "Standardized Residuals (Z)", subtitle = total_critical_text) +
    #labs(title = paste0("Standardized Residuals (Z)\n","for ",v2_names," by ",v1_names)) +
    #labs(title = "Standardized Residuals (Z)\nfo - fe") +
    labs(title = "Standardized Residuals (Z)") +
    ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
    ggplot2::theme(legend.title = element_text(hjust = 0.5)) +
    labs(caption = total_critical_text) +
    ggplot2::theme(plot.caption = element_text(hjust = 0.5))

  # matrix cell glyphs
  #if (!is.null(p.mat) & insig == "pch") {
  # p <- p + ggplot2::geom_point(
  #    data = p.mat,
  #    mapping = ggplot2::aes_string(x = "Var1", y = "Var2"),
  #    shape = pch,
  #    size = pch.cex,
  #    color = pch.col
  #  )
  #}

  # add titles
  if (title != "") {
    p <- p +
      ggplot2::ggtitle(title)
  }

  # removing legend
  if (!show.legend) {
    p <- p +
      ggplot2::theme(legend.position = "none")
  }

  # removing panel
  p <- p +
    #.no_panel()
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )
  p
}
