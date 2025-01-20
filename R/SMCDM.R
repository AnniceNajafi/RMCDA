#' Apply Stratified Multi-Criteria Decision Making (SMCDM) method
#'
#' @param comparison.mat the matrix containing alternatives as row names and criteria
#' as column names and corresponding scores as cell values.
#' @param state.criteria.probs the matrix containing the states as column names and
#' criteria as row names and the corresponding scores as matrix values.
#' @param likelihood.vector the vector containing the likelihood of being in each state.
#' @param independent.event this parameter is set to TRUE by default which indicates only the
#' probability of the occurence of each event is required (strati I and II). If set to FALSE
#' then the user should provide the probabilities of occurrence of all states.
#'
#' @return the SMCDM results
#'
#' @examples
#' data <- read.csv(system.file("extdata", "SMCDM_input.csv", package = "RMCDA"), header=FALSE)
#' mat.lst <- read.csv.SMCDM.matrices(data)
#' comparison.mat <- mat.lst[[1]]
#' state.criteria.probs <- mat.lst[[2]]
#' likelihood.vector <- mat.lst[[3]]
#' apply.SMCDM(comparison.mat, state.criteria.probs, likelihood.vector)
#' @export apply.SMCDM
apply.SMCDM <- function(comparison.mat, state.criteria.probs, likelihood.vector, independent.events = TRUE){

  if(independent.events == TRUE){

    extended.states <- t(apply(state.criteria.probs[,2:ncol(state.criteria.probs)], 1, function(x) {
      apply(combn(ncol(state.criteria.probs[,2:ncol(state.criteria.probs)]), 2), 2, function(y) mean(x[y])) }))

    all.event.happened.state <- rowMeans(state.criteria.probs[,2:ncol(state.criteria.probs)])

    extended.states <- cbind(extended.states, all.event.happened.state)

    colnames(extended.states) <- paste0("state.", seq(ncol(state.criteria.probs), ncol(state.criteria.probs)+ncol(extended.states)-1, 1))

    state.df <- cbind(state.criteria.probs, extended.states)

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


  }else{

    state.df <- state.criteria.probs

    likelihood.vector.stratum.2 <- likelihood.vector[2:4]

    likelihood.vector.stratum.3 <- likelihood.vector[5:7]

    likelihood.vector.stratum.4 <- likelihood.vector[8]

    p.vector <- likelihood.vector


  }


  criteria.percentages <- as.matrix(state.df) %*% (p.vector)

  option.val <- as.matrix(comparison.mat) %*% (as.matrix(state.df) %*% (p.vector))

  return(option.val)
}
