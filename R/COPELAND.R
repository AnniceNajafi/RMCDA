#' Apply Copeland Method
#'
#' @param mat A numeric matrix containing the values for different properties
#' of different alternatives.
#' @param beneficial.vector A numeric vector containing the column indices of beneficial
#' criteria. Non-beneficial criteria are assumed to be the remaining columns.
#' @return A numeric vector containing the calculated Copeland scores for each alternative.
#'
#'
#' @examples
#' mat <- matrix(c(80, 60, 90,
#'                 75, 85, 95,
#'                 70, 65, 85,
#'                 60, 75, 80),
#'               nrow = 4, byrow = TRUE)
#' colnames(mat) <- c("Criterion 1", "Criterion 2", "Criterion 3")
#' beneficial.vector <- c(1, 2, 3)
#' apply.Copeland(mat, beneficial.vector)
#' @export apply.Copeland
apply.Copeland <- function(mat, beneficial.vector) {

  X <- matrix(0, nrow = nrow(mat), ncol = nrow(mat))

  #Pairwise comparison of alternatives
  for (i in seq_len(nrow(mat))) {
    for (k in seq_len(nrow(mat))) {
      if (i != k) {
        for (j in seq_len(ncol(mat))) {
          if (j %in% beneficial.vector) {
            if (mat[i, j] > mat[k, j]) {
              X[i, k] <- X[i, k] + 1
            } else if (mat[i, j] < mat[k, j]) {
              X[i, k] <- X[i, k] - 1
            }
          } else {
            if (mat[i, j] > mat[k, j]) {
              X[i, k] <- X[i, k] - 1
            } else if (mat[i, j] < mat[k, j]) {
              X[i, k] <- X[i, k] + 1
            }
          }
        }
      }
    }
  }

  #Calculate total Copeland scores
  total <- rowSums(X)


  return(total)
}
