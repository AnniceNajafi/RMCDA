#' Function to apply BORDA method to data
#'
#' This function implements a simple Borda count approach for a decision matrix.
#' It computes a rank for each criterion and then sums these ranks for each alternative.
#' By specifying which columns are beneficial (i.e., higher values preferred),
#' it automatically treats the remaining columns as non-beneficial (i.e., lower values preferred).
#'
#' @param mat A numeric matrix or data frame. Rows represent alternatives,
#'   columns represent criteria.
#' @param beneficial.vector An integer vector containing the column indices of
#'   criteria that are beneficial (profit). All other columns are treated as
#'   non-beneficial (cost).
#'
#' @return A numeric vector of total Borda scores for each alternative, in the
#'   original row order.
#'
#'
#' @examples
#' # Create a small decision matrix (4 alternatives x 3 criteria)
#' mat <- matrix(c(
#'   5, 9, 2,
#'   7, 3, 8,
#'   6, 5, 4,
#'   4, 7, 9
#' ), nrow = 4, byrow = TRUE)
#'
#' beneficial.vector <- c(1, 3)
#'
#'
#' borda_scores <- apply.BORDA(mat, beneficial.vector)
#' borda_scores
#' @export apply.BORDA
apply.BORDA <- function(mat, beneficial.vector) {


  mat <- as.matrix(mat)

  if (!all(beneficial.vector %in% seq_len(ncol(mat)))) {
    stop("All elements of 'beneficial.vector' must be valid column indices of 'mat'.")
  }


  X <- matrix(0, nrow = nrow(mat), ncol = ncol(mat))

  #Borda-based ranks for each criteria
  for (j in seq_len(ncol(mat))) {
    if (j %in% beneficial.vector) {

      X[, j] <- rank(-mat[, j], ties.method = "first")
    } else {

      X[, j] <- rank(mat[, j], ties.method = "first")
    }
  }


  total <- rowSums(X)

  return(total)
}







