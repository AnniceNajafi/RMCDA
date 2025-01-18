#' Apply Range of Value (ROV) method
#'
#' @param mat A numeric matrix containing the values for different properties
#' of different alternatives.
#' @param weights A numeric vector containing the weights of each property.
#' @param beneficial.vector A numeric vector containing the column indices of beneficial
#' criteria. Non-beneficial criteria are assumed to be the remaining columns.
#' @return A numeric vector containing the calculated ROV scores for each alternative.
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
#' apply.ROV(mat, weights, beneficial.vector)
apply.ROV <- function(mat, weights, beneficial.vector) {

  X <- mat
  weights <- weights / sum(weights)

  for (j in seq_len(ncol(mat))) {
    D <- max(X[, j]) - min(X[, j])
    if (D == 0) D <- 1
    if (j %in% beneficial.vector) {
      X[, j] <- (X[, j] - min(X[, j])) / D
    } else {
      X[, j] <- (max(X[, j]) - X[, j]) / D
    }
  }

  #Calculate ROV scores
  u <- rowSums(X * weights)


  return(u)
}
