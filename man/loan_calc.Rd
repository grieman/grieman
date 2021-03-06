% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loan_calc.R
\name{loan_calc}
\alias{loan_calc}
\title{loan_calc}
\usage{
loan_calc(total, r, payment)
}
\arguments{
\item{total}{debt outstanding at start}

\item{r}{interest rate per period}

\item{payment}{payment per period. scalar or vector}
}
\value{
a list of vectors - total outstanding, payments per period, and interest paid per period
}
\description{
loan_calc
}
\examples{
loan = loan_calc(1000, .1, 110)
}
