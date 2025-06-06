% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DEMATEL.R
\name{apply.DEMATEL}
\alias{apply.DEMATEL}
\title{Apply DEMATEL method}
\usage{
apply.DEMATEL(comparisons.mat)
}
\arguments{
\item{comparisons.mat}{the matrix containing information related to pairwise comparisons of
criteria}
}
\value{
a list containing two vectors one holding D-R and the other D+R
}
\description{
Apply DEMATEL method
}
\examples{
comparisons.mat <- matrix(c(0, 3, 3, 4,
1, 0, 2, 1,
1, 2, 0, 2,
1, 2, 1, 0), nrow=4)
rownames(comparisons.mat)<-c("Price/cost", "Storage Space", "Camera", "Processor")
colnames(comparisons.mat)<-c("Price/cost", "Storage Space", "Camera", "Processor")
apply.DEMATEL(comparisons.mat)
}
