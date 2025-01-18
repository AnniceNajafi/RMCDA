#' Apply Grey Relational Analysis (GRA) method
#'
#' @param mat is a matrix containing the values for different properties
#' of different alternatives
#' @param weights are the weights of each property in the decision-making process
#' @param beneficial.vector is a vector containing the column numbers of beneficial
#' properties. Non-beneficial properties are assumed to be the remaining columns.
#' @param epsilon is a parameter for the GRA method, default is 0.5
#'
#' @return a vector containing the calculated GRA scores
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
#' apply.GRA(mat, weights, beneficial.vector)
apply.GRA <- function(mat, weights, beneficial.vector, epsilon = 0.5) {


  normalized.mat <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))

  for (j in seq_len(ncol(mat))) {
    if (j %in% beneficial.vector) {
      normalized.mat[, j] <- (mat[, j] - min(mat[, j])) /
        (max(mat[, j]) - min(mat[, j]) + 1e-10)
    } else {
      normalized.mat[, j] <- (max(mat[, j]) - mat[, j]) /
        (max(mat[, j]) - min(mat[, j]) + 1e-10)
    }
  }


  deviation.sequence <- 1 - normalized.mat


  gra.coefficient <- epsilon / (deviation.sequence + epsilon)


  gra.scores <- rowSums(gra.coefficient * weights) / nrow(mat)

  return(gra.scores)
}
