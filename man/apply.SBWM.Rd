% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SBWM.R
\name{apply.SBWM}
\alias{apply.SBWM}
\title{Function for applying the Stratified Best-Worst Method (SBWM)}
\usage{
apply.SBWM(
  comparison.mat,
  others.to.worst,
  others.to.best,
  state.worst.lst,
  state.best.lst,
  likelihood.vector
)
}
\arguments{
\item{comparison.mat}{the comparison matrix containing the alternatives as column names
and the criteria as row names.}

\item{others.to.worst}{the comparison of the criteria to the worst criteria for each state,
column names should be states and the row names are criteria}

\item{others.to.best}{the comparison of the criteria to the best criteria for each state,
column names should be states and the row names are criteria}

\item{state.worst.lst}{the vector containing the name of the worst criteria in each state}

\item{state.best.lst}{the vector containing the name of the best criteria in each state}

\item{likelihood.vector}{the vector containing the likelihood of being in each state.}
}
\value{
the result of SBWM
}
\description{
Function for applying the Stratified Best-Worst Method (SBWM)
}
\examples{
data <- read.csv(system.file("extdata",
 "stratified_BWM_case_study_I_example.csv",
  package = "RMCDA"), header = FALSE)
mat.lst <- read.csv.SBWM.matrices(data)
comparison.mat <- mat.lst[[1]]
others.to.worst <- mat.lst[[2]]
others.to.best <- mat.lst[[3]]
state.worst.lst <- mat.lst[[4]]
state.best.lst <- mat.lst[[5]]
likelihood.vector <- mat.lst[[6]]
apply.SBWM(comparison.mat, others.to.worst,
 others.to.best, state.worst.lst,
  state.best.lst, likelihood.vector)
}
