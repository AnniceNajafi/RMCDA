#' Apply SRMP (Simple Ranking Method using Reference Profiles) on data
#'
#' @param evaluations.mat the matrix comparing alternatives based on criteria
#' @param reference.profiles matrix containing reference profile information
#' @param weights of different criteria
#'
#' @return alternatives ranked using SRMP
#' @export
#'
#' @examples
#' evaluations.mat <- matrix(c(41, 46, 43, -2, -4, -5.5, 4, 2, 3), nrow=3)
#' colnames(evaluations.mat) <- c("S", "L", "J")
#' rownames(evaluations.mat) <- c("x", "y", "z")
#' reference.profiles <- matrix(c(42, 45, -5, -3, 2, 4), nrow=2)
#' colnames(reference.profiles) <- c("S", "L", "J")
#' rownames(reference.profiles) <- c("p1", "p2")
#' weights <- c(1/3, 1/3, 1/3)
#' apply.SRMP(evaluations.mat, reference.profiles, weights)
apply.SRMP <- function(evaluations.mat, reference.profiles, weights) {

  comparisons <- c()

  #Initial comparison
  for (i in 1:nrow(evaluations.mat)) {
    comparisons <- c(comparisons, sum((evaluations.mat[i, ] >= reference.profiles[1, ]) * weights))
  }

  #Repeat until no duplicates are found
  ref.idx <- 2

  while (sum(duplicated(comparisons)) > 0) {
    duplicated_indices <- which(duplicated(comparisons) | duplicated(comparisons, fromLast = TRUE))
    comparisons.2 <- comparisons

    for (i in duplicated_indices) {
      #Recalculate using the next reference profile
      comparisons.2[i] <- sum((evaluations.mat[i, ] >= reference.profiles[ref.idx, ]) * weights)
    }

    comparisons[duplicated_indices] <- comparisons.2[duplicated_indices]
    ref.idx <- ref.idx +1
  }

  names(comparisons)<-rownames(evaluations.mat)

  return(comparisons)
}
