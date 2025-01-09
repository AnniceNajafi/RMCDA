#' Apply Multi-Attributive Border Approximation Area Comparison (MABAC)
#'
#' @description
#' R implementation of the MABAC method.
#' The MABAC method computes the distance between each alternative and the
#' Boundary Approximation Area (BAA), based on a weighted normalized decision matrix.
#'
#' @param mat A numeric matrix. Rows are alternatives; columns are criteria.
#' @param weights A numeric vector of weights corresponding to criteria columns. Must sum to 1.
#' @param types An integer vector of the same length as `weights`. Use 1 for a profit criterion
#'  and -1 for a cost criterion.
#'
#' @return A numeric vector with the MABAC preference values for each alternative.
#' A higher value indicates a more preferred alternative.
#' @export
#'
#' @examples
#' # Example usage:
#' mat <- matrix(c(
#'   22600, 3800, 2,   5, 1.06, 3.00, 3.5,  2.8, 24.5, 6.5,
#'   19500, 4200, 3,   2, 0.95, 3.00, 3.4,  2.2, 24.0, 7.0,
#'   21700, 4000, 1,   3, 1.25, 3.20, 3.3,  2.5, 24.5, 7.3,
#'   20600, 3800, 2,   5, 1.05, 3.25, 3.2,  2.0, 22.5, 11.0,
#'   22500, 3800, 4,   3, 1.35, 3.20, 3.7,  2.1, 23.0, 6.3,
#'   23250, 4210, 3,   5, 1.45, 3.60, 3.5,  2.8, 23.5, 7.0,
#'   20300, 3850, 2,   5, 0.90, 3.25, 3.0,  2.6, 21.5, 6.0
#' ), nrow = 7, byrow = TRUE)
#'
#' weights <- c(0.146, 0.144, 0.119, 0.121, 0.115, 0.101, 0.088, 0.068, 0.050, 0.048)
#' types <- c(-1, 1, 1, 1, -1, -1, 1, 1, 1, 1)
#'
#' apply.MABAC(mat, weights, types)
apply.MABAC <- function(mat, weights, types) {


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

  n <- nrow(mat)

  #--------------------------------------#
  #Normalize matrix (profit or cost)

  normalized_mat <- matrix(NA, nrow = n, ncol = ncol(mat))
  for (j in seq_len(ncol(mat))) {
    col_j <- mat[, j]
    min_val <- min(col_j)
    max_val <- max(col_j)

    if (types[j] == 1) {
      #Profit criterion: higher is better
      normalized_mat[, j] <- (col_j - min_val) / (max_val - min_val)
    } else {
      #Cost criterion: lower is better
      normalized_mat[, j] <- (max_val - col_j) / (max_val - min_val)
    }
  }

  #--------------------------------------------------#
  #Calculate Weighted Matrix: (normalized + 1) * w

  #Expand 'weights' to match dimensions of matrix
  weighted_mat <- sweep((normalized_mat + 1), 2, weights, FUN = "*")

  #------------------------------------------------------#
  #Border Approximation Area: G = product per column ^
  #         (1 / number_of_alternatives)
  #------------------------------------------------------#
  #Product of each column across alternatives => a vector of length = ncol(mat)
  product_by_column <- apply(weighted_mat, 2, prod)
  G <- product_by_column^(1 / n)

  #---------------------------------------------------#
  #Distance from G: Q = weighted_mat - G (by col) #
  #---------------------------------------------------#
  #We'll broadcast G by column
  Q <- sweep(weighted_mat, 2, G, FUN = "-")


  pref_values <- rowSums(Q)

  return(pref_values)
}
