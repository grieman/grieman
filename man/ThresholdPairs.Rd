% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ThresholdPairs.R
\name{ThresholdPairs}
\alias{ThresholdPairs}
\title{ggpairs plot, with group column}
\usage{
ThresholdPairs(data, filtercolumn, cutoff)
}
\arguments{
\item{data}{data frame}

\item{filtercolumn}{column name of data to color by}

\item{cutoff}{value or values at which to split the filtercolumn. Integer or vector}
}
\value{
A ggpairs plot
}
\description{
ggpairs plot, with group column
}
\examples{
set.seed(1)
datain <- data.frame(matrix(rnorm(100), nrow=25))
colnames(datain) <- c("w","x","y","z")

ThresholdPairs(datain, "x", cutoff=c(-.33,.33))

}
