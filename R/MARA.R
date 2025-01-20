#' Apply the MARA (Magnitude of the Area for the Ranking of Alternatives) Method
#'
#' MARA ranks alternatives based on multiple criteria, each weighted. Columns
#' in beneficial.vector are treated as "max" (beneficial), and columns
#' not in beneficial.vector are treated as "min" (cost).
#'
#' The following function is the R implementation of the python function mara from the pyDecision package
#' Source: https://github.com/Valdecy/pyDecision/blob/master/pyDecision/algorithm/mara.py
#'
#'
#' @param mat A numeric matrix with each row an alternative and each column a criterion.
#' @param weights A numeric vector of weights for each criterion (same length as number of columns).
#' @param beneficial.vector An integer vector of column indices for the beneficial (max) criteria.
#' @return A numeric vector of MARA scores for each alternative.
#'
#' @examples
#' # Example
#' mat <- matrix(c(10, 2,
#'                 20, 4,
#'                 15, 5),
#'               nrow = 3, byrow = TRUE)
#' weights <- c(0.7, 0.3)
#' beneficial.vector <- c(1)  # First column is beneficial (max); second is cost (min)
#' apply.MARA(mat, weights, beneficial.vector)
#'
#' @export apply.MARA
apply.MARA <- function(mat, weights, beneficial.vector) {


  if (ncol(mat) != length(weights)) {
    stop("Number of weights must match the number of columns in 'mat'.")
  }

  criterion.type <- rep("min", ncol(mat))
  criterion.type[beneficial.vector] <- "max"


  X <- mat


  for (j in seq_len(ncol(X))) {
    if (criterion.type[j] == "max") {
      X[, j] <- X[, j] / max(X[, j])
    } else {
      X[, j] <- min(X[, j]) / X[, j]
    }
  }


  X <- sweep(X, 2, weights, `*`)


  opt <- apply(X, 2, max)


  S_k <- 0
  S_L <- 0
  T_k <- rep(0, nrow(X))
  T_L <- rep(0, nrow(X))


  for (j in seq_len(ncol(X))) {
    if (criterion.type[j] == "max") {
      S_k <- S_k + opt[j]
      T_k <- T_k + X[, j]
    } else {
      S_L <- S_L + opt[j]
      T_L <- T_L + X[, j]
    }
  }


  f_opt <- (S_L - S_k) / 2 + S_k
  f_i   <- (T_L - T_k) / 2 + T_k

  M <- f_opt - f_i

  return(M)
}

