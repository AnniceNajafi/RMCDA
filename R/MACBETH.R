#' Apply MACBETH (Measuring Attractiveness by a Categorical Based Evaluation TecHnique)
#'
#'
#' @param mat A numeric matrix where rows represent alternatives and columns represent criteria.
#' @param beneficial.vector An integer vector containing column indices for the beneficial
#'   (larger-is-better) criteria. Columns not in \code{beneficial.vector} are treated as
#'   non-beneficial (smaller-is-better).
#' @param weights A numeric vector of the same length as the number of columns in \code{mat},
#'   containing the relative importance weights for each criterion.
#'
#' @return A numeric vector \code{V} of length \eqn{nrow(mat)}, the final attractiveness scores.
#'
#' @examples
#' # Example matrix: 3 alternatives x 2 criteria
#' mat <- matrix(c(10, 5,
#'                 12, 4,
#'                 11, 6), nrow=3, byrow=TRUE)
#'
#' # Suppose first column is beneficial, second is non-beneficial
#' benef.vec <- c(1)
#' wts <- c(0.6, 0.4)
#'
#' # Get MACBETH scores
#' res <- apply.MACBETH(mat, benef.vec, wts, verbose=TRUE)
#' print(res)
#'
#' @export
apply.MACBETH <- function(mat,
                          beneficial.vector,
                          weights) {


  X <- as.matrix(mat)

  n <- nrow(X)
  m <- ncol(X)


  all_cols <- seq_len(m)
  non_beneficial <- setdiff(all_cols, beneficial.vector)


  best  <- numeric(m)
  worst <- numeric(m)


  for (j in seq_len(m)) {
    col_j <- X[, j]
    if (j %in% beneficial.vector) {
      best[j]  <- max(col_j)
      worst[j] <- min(col_j)
    } else {
      best[j]  <- min(col_j)
      worst[j] <- max(col_j)
    }
  }

  eps <- 1e-16


  for (j in seq_len(m)) {
    X[, j] <- (X[, j] - worst[j]) / (best[j] - worst[j] + eps)
  }


  X <- X * weights


  V <- rowSums(X)

  return(V)
}
