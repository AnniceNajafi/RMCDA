#' Function for applying the Stratified Best-Worst Method (SBWM)
#'
#' @param comparison.mat the comparison matrix containing the alternatives as column names
#' and the criteria as row names.
#' @param others.to.worst the comparison of the criteria to the worst criteria for each state,
#' column names should be states and the row names are criteria
#' @param others.to.best the comparison of the criteria to the best criteria for each state,
#' column names should be states and the row names are criteria
#' @param state.worst.lst the vector containing the name of the worst criteria in each state
#' @param state.best.lst the vector containing the name of the best criteria in each state
#' @param likelihood.vector the vector containing the likelihood of being in each state.
#'
#' @return the result of SBWM
#'
#' @examples
#' data <- read.csv(system.file("extdata",
#'  "stratified_BWM_case_study_I_example.csv",
#'   package = "RMCDA"), header = FALSE)
#' mat.lst <- read.csv.SBWM.matrices(data)
#' comparison.mat <- mat.lst[[1]]
#' others.to.worst <- mat.lst[[2]]
#' others.to.best <- mat.lst[[3]]
#' state.worst.lst <- mat.lst[[4]]
#' state.best.lst <- mat.lst[[5]]
#' likelihood.vector <- mat.lst[[6]]
#' apply.SBWM(comparison.mat, others.to.worst,
#'  others.to.best, state.worst.lst,
#'   state.best.lst, likelihood.vector)
#' @importFrom utils combn
#' @export apply.SBWM
apply.SBWM <- function(comparison.mat, others.to.worst, others.to.best, state.worst.lst, state.best.lst, likelihood.vector){

  apply.SMCDM.internal.SBWM <- function(comparison.mat, state.criteria.probs, likelihood.vector){

    likelihood.vector.stratum.2 <- likelihood.vector[-1]/likelihood.vector[1]

    likelihood.vector.stratum.3 <- apply(combn(likelihood.vector.stratum.2, 2), 2, prod)

    likelihood.vector.stratum.4 <- prod(likelihood.vector.stratum.2)

    coefficients <- c(
      likelihood.vector.stratum.4,  #Coefficient of p^3
      sum(likelihood.vector.stratum.3),  #Coefficient of p^2
      sum(likelihood.vector.stratum.2)+1,  #Coefficient of p^1
      -1                            #Constant term at the end
    )

    #Find the roots of the equation
    roots <- polyroot(rev(coefficients))

    non.Im.root <- Re(roots[Im(roots) == 0])

    if(length(non.Im.root)==0){

      tolerance <- 1e-10
      non.Im.root <- Re(roots[abs(Im(roots)) < tolerance])
    }

    p.vector <- c(non.Im.root, non.Im.root*likelihood.vector.stratum.2, non.Im.root^2*likelihood.vector.stratum.3, non.Im.root^3*likelihood.vector.stratum.4)

    criteria.percentages <-  as.matrix(state.criteria.probs) %*% (p.vector)

    option.val <- as.matrix(comparison.mat) %*% (as.matrix(state.criteria.probs) %*% (p.vector))

    return(option.val)
  }

  if(sum(dim(others.to.worst)==dim(others.to.best))!=2){
    stop("Unable to proceed. Matrix dimensions mismatch.")
  }

  state.weights.lst <- list()

  for(state.no in 1:ncol(others.to.worst)){

    state.weights <- apply.BWM(rownames(others.to.best), state.worst.lst[state.no], state.best.lst[state.no], as.numeric(others.to.best[,state.no]), as.numeric(others.to.worst[,state.no]))

    state.weights.lst[[state.no]] <- state.weights
  }

  state.weights.mat <- do.call(cbind, state.weights.lst)

  state.weights.mat <-as.data.frame(state.weights.mat)[-nrow(state.weights.mat),]

  rownames(state.weights.mat)<- rownames(comparison.mat)

  colnames(state.weights.mat)<- paste0("state.", seq(0, ncol(state.weights.mat)-1, 1))

  rownames(t(comparison.mat))->alt.names

  comparison.mat <- apply(t(comparison.mat), 2, function(x) as.numeric(x))

  rownames(comparison.mat)<-alt.names

  res <- apply.SMCDM.internal.SBWM(comparison.mat, as.matrix(state.weights.mat), as.numeric(likelihood.vector))

  return(res)
}
