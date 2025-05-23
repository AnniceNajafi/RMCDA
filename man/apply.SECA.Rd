% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SECA.R
\name{apply.SECA}
\alias{apply.SECA}
\title{Apply Simultaneous Evaluation of Criteria and Alternatives (SECA) method}
\usage{
apply.SECA(mat, beneficial.vector, beta = 3)
}
\arguments{
\item{mat}{A numeric matrix containing the values for different properties
of different alternatives.}

\item{beneficial.vector}{A numeric vector containing the column indices of beneficial
properties. Non-beneficial properties are assumed to be the remaining columns.}

\item{beta}{A numeric value controlling the balance between criteria variability and
similarity. Default is 3.}
}
\value{
A numeric vector containing the calculated weights for each criterion.
}
\description{
Apply Simultaneous Evaluation of Criteria and Alternatives (SECA) method
}
\examples{
mat <- matrix(c(80, 60, 90,
                75, 85, 95,
                70, 65, 85,
                60, 75, 80),
              nrow = 4, byrow = TRUE)
colnames(mat) <- c("Criterion 1", "Criterion 2", "Criterion 3")
beneficial.vector <- c(1, 2, 3)
apply.SECA(mat, beneficial.vector)
}
