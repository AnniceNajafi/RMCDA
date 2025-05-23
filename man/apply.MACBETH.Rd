% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MACBETH.R
\name{apply.MACBETH}
\alias{apply.MACBETH}
\title{Apply MACBETH (Measuring Attractiveness by a Categorical Based Evaluation TecHnique)}
\usage{
apply.MACBETH(mat, beneficial.vector, weights)
}
\arguments{
\item{mat}{A numeric matrix where rows represent alternatives and columns represent criteria.}

\item{beneficial.vector}{An integer vector containing column indices for the beneficial
(larger-is-better) criteria. Columns not in beneficial.vector are treated as
non-beneficial (smaller-is-better).}

\item{weights}{A numeric vector of the same length as the number of columns in mat,
containing the relative importance weights for each criterion.}
}
\value{
A numeric vector V of length nrow(mat), the final attractiveness scores.
}
\description{
Apply MACBETH (Measuring Attractiveness by a Categorical Based Evaluation TecHnique)
}
\examples{
# Example matrix: 3 alternatives x 2 criteria
mat <- matrix(c(10, 5,
                12, 4,
                11, 6), nrow=3, byrow=TRUE)

# Suppose first column is beneficial, second is non-beneficial
benef.vec <- c(1)
wts <- c(0.6, 0.4)

# Get MACBETH scores
res <- apply.MACBETH(mat, benef.vec, wts)
print(res)

}
