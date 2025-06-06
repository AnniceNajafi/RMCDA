% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CODAS.R
\name{apply.CODAS}
\alias{apply.CODAS}
\title{Apply Combinative Distance-based Assessment (CODAS)}
\usage{
apply.CODAS(mat, weights, beneficial.vector, psi)
}
\arguments{
\item{mat}{is a matrix and contains the values for different properties
of different alternatives}

\item{weights}{are the weights of each property in the decision making process}

\item{beneficial.vector}{is a vector that contains the column number of beneficial
properties.}

\item{psi}{threshold parameter}
}
\value{
a vector containing the calculated quantitative utility
}
\description{
Apply Combinative Distance-based Assessment (CODAS)
}
\examples{

mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
420, 91, 1365, 1120, 875, 1190, 200,
74.2, 70, 189, 210, 112, 217, 112,
2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)
colnames(mat)<-c("Toughness Index",	"Yield Strength",	"Young's Modulus",
                 "Density",	"Thermal Expansion",	"Thermal Conductivity",	"Specific Heat")
rownames(mat)<-c("AI 2024-T6", "AI 5052-O","SS 301 FH",
"SS 310-3AH",
"Ti-6AI-4V",
"Inconel 718",
"70Cu-30Zn")
weights <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)
beneficial.vector<-c(1,2,3)
psi <- 0.02
apply.CODAS(mat, weights, beneficial.vector, psi)
}
