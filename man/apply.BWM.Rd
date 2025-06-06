% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BWM.R
\name{apply.BWM}
\alias{apply.BWM}
\title{Function for applying the Best-Worst Method}
\usage{
apply.BWM(
  criteria.lst,
  worst.criteria,
  best.criteria,
  best.criteria.preference,
  worst.criteria.preference
)
}
\arguments{
\item{criteria.lst}{list of criteria}

\item{worst.criteria}{the worst criteria}

\item{best.criteria}{the best criteria}

\item{best.criteria.preference}{the comparison of the best criteria to others}

\item{worst.criteria.preference}{the comparison of the worst criteria to others}
}
\value{
the result of BWM
}
\description{
Function for applying the Best-Worst Method
}
\examples{
c <- c("C1", "C2", "C3")
w <- "C1"
b <- "C3"
bcp <- c(8, 2, 1)
wcp <- c(1, 5, 8)
apply.BWM(c, w, b, bcp, wcp)
}
