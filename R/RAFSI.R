#' Ranking of Alternatives through Functional mapping of criterion sub-intervals
#' into a Single Interval (RAFSI)
#'
#' @param mat A numeric matrix or data frame with rows = alternatives, columns = criteria
#' @param weights A numeric vector of weights (one per criterion)
#' @param beneficial.vector A numeric vector that stores the column indices of all beneficial
#'        (i.e., "max") criteria. Columns not in `beneficial.vector` are treated as "min".
#' @param ideal A numeric vector of ideal values for each criterion (optional)
#' @param anti_ideal A numeric vector of anti-ideal values for each criterion (optional)
#' @param n_i Lower bound in the functional mapping (default = 1)
#' @param n_k Upper bound in the functional mapping (default = 6)
#'
#' @return A numeric vector of final RAFSI scores, one per row of `mat`.
#' @examples
#' mat <- matrix(c(3, 2, 5,
#' 4, 3, 2,
#' 1, 6, 4),
#' nrow = 3, byrow = TRUE)
#' weights <- c(0.3, 0.5, 0.2)
#' beneficial.vector <- c(1, 2)
#' apply.RAFSI(mat, weights, beneficial.vector,   n_i = 1, n_k = 6)
#' @export
apply.RAFSI <- function(mat,
                        weights,
                        beneficial.vector,
                        ideal = NULL,
                        anti_ideal = NULL,
                        n_i = 1,
                        n_k = 6) {


  X <- as.matrix(mat)

  n_crit <- ncol(X)
  coef   <- matrix(0, nrow = 2, ncol = n_crit)
  best   <- numeric(n_crit)
  worst  <- numeric(n_crit)


  for (j in seq_len(n_crit)) {

    if (j %in% beneficial.vector) {

      if (is.null(ideal)) {
        best[j]  <- max(X[, j]) * 2
      } else {
        best[j]  <- ideal[j]
      }
      if (is.null(anti_ideal)) {
        worst[j] <- min(X[, j]) * 0.5
      } else {
        worst[j] <- anti_ideal[j]
      }

    } else {

      if (is.null(anti_ideal)) {
        best[j]  <- min(X[, j]) * 0.5
      } else {
        best[j]  <- anti_ideal[j]
      }
      if (is.null(ideal)) {
        worst[j] <- max(X[, j]) * 2
      } else {
        worst[j] <- ideal[j]
      }
    }


    coef[1, j] <- (n_k - n_i) / (best[j] - worst[j])
    coef[2, j] <- (best[j] * n_i - worst[j] * n_k) / (best[j] - worst[j])
  }

  S <- sweep(X, 2, coef[1, ], FUN = "*") +
    matrix(rep(coef[2, ], nrow(X)), nrow = nrow(X), byrow = TRUE)


  A <- mean(c(n_i, n_k))
  H <- 2 / ((n_i^-1) + (n_k^-1))

  #Adjust columns of S based on whether j is beneficial (max) or not (min)
  for (j in seq_len(n_crit)) {
    if (j %in% beneficial.vector) {

      S[, j] <- S[, j] / (2 * A)
    } else {

      S[, j] <- H / (2 * S[, j])
    }
  }

  #Weighted sum to get final scores
  V <- rowSums(sweep(S, 2, weights, FUN = "*"))

  return(V)
}

