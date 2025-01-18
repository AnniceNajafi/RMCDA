#' Apply Proximity Indexed Value (PIV) method
#'
#' @param mat A numeric matrix containing the values for different properties
#' of different alternatives.
#' @param weights A numeric vector containing the weights of each property.
#' @param beneficial.vector A numeric vector containing the column indices of beneficial
#' criteria. Non-beneficial criteria are assumed to be the remaining columns.
#'
#' @return A numeric vector containing the calculated PIV scores for each alternative.
#' @export
#'
#' @examples
#' mat <- matrix(c(80, 60, 90,
#'                 75, 85, 95,
#'                 70, 65, 85,
#'                 60, 75, 80),
#'               nrow = 4, byrow = TRUE)
#' colnames(mat) <- c("Criterion 1", "Criterion 2", "Criterion 3")
#' weights <- c(0.4, 0.3, 0.3)
#' beneficial.vector <- c(1, 2, 3)
#' apply.PIV(mat, weights, beneficial.vector)
apply.PIV <- function(mat, weights, beneficial.vector) {

  X <- mat
  for (j in seq_len(ncol(mat))) {
    X[, j] <- (X[, j] / sqrt(sum(X[, j]^2))) * weights[j]
  }


  for (j in seq_len(ncol(mat))) {
    if (j %in% beneficial.vector) {
      X[, j] <- max(X[, j]) - X[, j]
    } else {
      X[, j] <- X[, j] - min(X[, j])
    }
  }

  D <- rowSums(X)

  return(D)
}
