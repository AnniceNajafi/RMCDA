#' Function for applying the Best-Worst Method
#'
#' @param criteria.lst list of criteria
#' @param worst.criteria the worst criteria
#' @param best.criteria the best criteria
#' @param best.criteria.preference the comparison of the best criteria to others
#' @param worst.criteria.preference the comparison of the worst criteria to others
#'
#' @return the result of BWM
#' @export
#'
#' @examples
#' criteria.lst <- c("C1", "C2", "C3")
#' worst.criteria <- "C1"
#' best.criteria <- "C3"
#' best.criteria.preference <- c(8, 2, 1)
#' worst.criteria.preference <- c(1, 5, 8)
#' apply.BWM(criteria.lst, worst.criteria, best.criteria, best.criteria.preference, worst.criteria.preference)
apply.BWM <- function(criteria.lst, worst.criteria, best.criteria, best.criteria.preference, worst.criteria.preference){

  best.idx <- which(best.criteria == criteria.lst)
  worst.idx <- which(worst.criteria == criteria.lst)

  n_vars <- length(criteria.lst)+1
  objective <- c(rep(0, length(criteria.lst)), 1)

  #Initialize lists for constraints
  constraints <- list()
  directions <- character()
  rhs <- numeric()


  for (j in 1:length(best.criteria.preference)) {
    coef <- rep(0, n_vars)
    coef[best.idx] <- 1

    if(j==best.idx){
      coef[j]=0
    }else{
      coef[j] <- -1 * best.criteria.preference[j]
    }
    coef[n_vars] <- -1


    constraints[[length(constraints) + 1]] <- coef
    directions <- c(directions, "<=")
    rhs <- c(rhs, 0)

  }


  for (j in 1:length(worst.criteria.preference)) {
    coef <- rep(0, n_vars)
    coef[j] <- 1

    if(j==worst.idx){
      coef[j]=0
    }else{
      coef[worst.idx] <- -1 * worst.criteria.preference[j]
    }
    coef[n_vars] <- -1

    constraints[[length(constraints) + 1]] <- coef
    directions <- c(directions, "<=")
    rhs <- c(rhs, 0)

  }


  constraints[[length(constraints) + 1]] <- c(rep(1, length(criteria.lst)), 0)
  directions <- c(directions, "=")
  rhs <- c(rhs, 1)

  constraint_matrix <- do.call(rbind, constraints)

  identity.crit <- diag(length(criteria.lst))

  identity.crit <- cbind(identity.crit, rep(0, nrow(identity.crit)))
  constraint_matrix <- rbind(constraint_matrix, identity.crit)

  directions <- c(directions, rep(">=", nrow(identity.crit)))
  rhs <- c(rhs, rep("0", nrow(identity.crit)))

  #Solve the linear programming problem
  result <- lpSolve::lp("min", objective, constraint_matrix, directions, rhs)

  return(result$solution)
}
