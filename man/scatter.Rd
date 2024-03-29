% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatter.R
\name{scatter}
\alias{scatter}
\title{Simplified Scatterplot}
\usage{
scatter(df, var1, var2, lab = FALSE)
}
\arguments{
\item{df}{data frame to read in.}

\item{var1}{the dependent/outcome variable, \eqn{Y}.}

\item{var2}{the independent/predictor variable, \eqn{X}.}

\item{lab}{logical (default set to \code{FALSE}). When set to \code{lab = TRUE}, will add Pearson's correlation coefficient (\eqn{r}) value to the plot.}
}
\description{
This function plots a scatterplot (scatter) on a given data frame, and adds a fit-line to the data.
}
\examples{
data <- mtcars

scatter(data,mpg,wt)
}
