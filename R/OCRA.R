#' Apply Operational Competitiveness Rating (OCRA) method
#'
#' The OCRA method independently evaluates alternatives with respect to beneficial
#' (profit) and non-beneficial (cost) criteria, then combines these evaluations into
#' an overall operational competitiveness rating.
#'
#' @param mat A numeric matrix. Rows are alternatives; columns are criteria.
#' @param weights A numeric vector of weights corresponding to criteria columns. Must sum to 1.
#' @param types An integer vector of the same length as `weights`. Use 1 for a profit criterion
#'  and -1 for a cost criterion.
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
#' types <- c(1, -1, 1, 1, 1)
#'
#' apply.OCRA(mat, weights, types)
apply.OCRA <- function(mat, weights, types) {

  if (!is.matrix(mat)) {
    stop("'mat' must be a matrix.")
  }
  if (length(weights) != ncol(mat)) {
    stop("Length of 'weights' must match the number of columns in 'mat'.")
  }
  if (length(types) != ncol(mat)) {
    stop("Length of 'types' must match the number of columns in 'mat'.")
  }
  if (abs(sum(weights) - 1) > 1e-9) {
    stop("The sum of 'weights' must be 1.")
  }
  if (!all(types %in% c(1, -1))) {
    stop("'types' must contain only 1 (profit) or -1 (cost).")
  }

  ocra_normalization <- function(x, cost = FALSE) {

    if (cost) {

      return((max(x) - x) / min(x))
    } else {

      return((x - min(x)) / min(x))
    }
  }

  n <- nrow(mat)
  m <- ncol(mat)


  I <- rep(0, n)  #cost criteria
  O <- rep(0, n)  #profit criteria

  for (j in seq_len(m)) {
    if (types[j] == -1) {

      I <- I + weights[j] * ocra_normalization(mat[, j], cost = TRUE)
    } else {

      O <- O + weights[j] * ocra_normalization(mat[, j], cost = FALSE)
    }
  }


  I <- I - min(I)
  O <- O - min(O)

  total <- I + O
  pref_values <- total - min(total)

  return(pref_values)
}
