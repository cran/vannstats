% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.oneway.R
\name{summary.oneway}
\alias{summary.oneway}
\title{Summarize Results of ow.anova}
\usage{
\method{summary}{oneway}(object, ...)
}
\arguments{
\item{object}{Object returned by \code{\link{ow.anova}}.}

\item{...}{Additional parameters to pass on.}
}
\value{
Matrix of values for results from One-Way ANOVA test.
}
\description{
Displays results of ow.anova
}
\examples{
data1 <- mtcars
ow <- ow.anova(data1, mpg, cyl)

summary(ow)
}
