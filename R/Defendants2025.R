#' Defendants, 2025 (Individual-Level)
#'
#' This is a simulated data set, created in 2025. These data represent cases for individual defendants held at the Richard J. Donovan Correctional Facility in San Diego, CA. These data were simulated by Dr. Burrel Vann Jr, and represent a random sample of individuals held in the Center in 2025. Each observation in the data set represents a unique individual defendant, and the unique characteristics tied to their court case.
#'
#' @format A data frame with 1738 observations and 11 variables.
#' \tabular{ll}{ \cr
#' id \tab Unique defendant identifier \cr
#' age \tab The defendant's age	\cr
#' race \tab Race of the defendant \cr
#' race_binary \tab race, broken into a binary/dummy variable, measuring whether or not the defendant is white \cr
#' charge \tab The crime the defendant was charged with \cr
#' gang \tab Whether or not the defendant is affiliated with a gang	\cr
#' priors \tab The number of prior misdemeanors the defendant has	\cr
#' gun \tab Whether or not a gun was involved in this case \cr
#' risk_score \tab A judge's risk-of-reoffending score for the defendant \cr
#' bail \tab The bail amount for the defendant \cr
#' perkins \tab Whether or not a Perkins Operation was conducted on defendant while in custody \cr
#' }

#'
"Defendants2025"
