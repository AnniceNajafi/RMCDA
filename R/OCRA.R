#' Apply Operational Competitiveness Rating (OCRA) method
#'
#' The OCRA method independently evaluates alternatives with respect to beneficial
#' (profit) and non-beneficial (cost) criteria, then combines these evaluations into
#' an overall operational competitiveness rating.
#'
#' @param mat A numeric matrix. Rows are alternatives; columns are criteria.
#' @param weights A numeric vector of weights corresponding to criteria columns. Must sum to 1.
#' @param beneficial.vector A numeric vector containing the column indices of beneficial
#' (profit) criteria. Non-beneficial criteria are assumed to be the remaining columns.
#'
#' @return A numeric vector with the OCRA preference values for each alternative.
#' Higher values indicate a more preferred alternative.
#' @export
#'
#' @examples
#' mat <- matrix(c(
#'   7.7, 256, 7.2, 7.3, 7.3,
#'   8.1, 250, 7.9, 7.8, 7.7,
#'   8.7, 352, 8.6, 7.9, 8.0,
#'   8.1, 262, 7.0, 8.1, 7.2,
#'   6.5, 271, 6.3, 6.4, 6.1,
#'   6.8, 228, 7.1, 7.2, 6.5
#' ), nrow = 6, byrow = TRUE)
#'
#' weights <- c(0.239, 0.225, 0.197, 0.186, 0.153)
#' beneficial.vector <- c(1, 3, 4, 5)
#'
#' apply.OCRA(mat, weights, beneficial.vector)
apply.OCRA <- function(mat, weights, beneficial.vector) {

  if (!is.matrix(mat)) {
    stop("'mat' must be a matrix.")
  }
  if (length(weights) != ncol(mat)) {
    stop("Length of 'weights' must match the number of columns in 'mat'.")
  }
  if (abs(sum(weights) - 1) > 1e-9) {
    stop("The sum of 'weights' must be 1.")
  }

  #Helper function for normalization
  ocra_normalization <- function(x, cost = FALSE) {
    if (cost) {
      return((max(x) - x) / min(x))
    } else {
      return((x - min(x)) / min(x))
    }
  }

  n <- nrow(mat)
  m <- ncol(mat)

  I <- rep(0, n)
  O <- rep(0, n)

  for (j in seq_len(m)) {
    if (j %in% beneficial.vector) {

      O <- O + weights[j] * ocra_normalization(mat[, j], cost = FALSE)
    } else {

      I <- I + weights[j] * ocra_normalization(mat[, j], cost = TRUE)
    }
  }


  I <- I - min(I)
  O <- O - min(O)

  total <- I + O
  pref_values <- total - min(total)

  return(pref_values)
}
