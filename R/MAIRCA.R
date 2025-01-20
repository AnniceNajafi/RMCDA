#' Apply Multi-Attributive Real Ideal Comparative Analysis (MAIRCA)
#'
#' R implementation of the MAIRCA method.
#' The MAIRCA method computes the gap between ideal (theoretical) and empirical
#' ratings to rank alternatives.
#'
#' @param mat A numeric matrix. Rows are alternatives; columns are criteria.
#' @param weights A numeric vector of weights corresponding to criteria columns. Must sum to 1.
#' @param types An integer vector of the same length as `weights`. Use 1 for a profit criterion
#'  and -1 for a cost criterion.
#'
#' @return A numeric vector with the MAIRCA preference values for each alternative.
#' Higher values indicate more preferred alternatives.
#'
#'
#' @examples
#' # Example usage
#' mat <- matrix(c(70, 245, 16.4, 19,
#'                 52, 246, 7.3, 22,
#'                 53, 295, 10.3, 25,
#'                 63, 256, 12.0, 8,
#'                 64, 233, 5.3, 17),
#'               nrow = 5, byrow = TRUE)
#' weights <- c(0.04744, 0.02464, 0.51357, 0.41435)
#' types <- c(1, 1, 1, 1)
#' apply.MAIRCA(mat, weights, types)
#' @export apply.MAIRCA
apply.MAIRCA <- function(mat, weights, types) {


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
  Tp <- (1 / n) * weights  # 1 / n for each criterion, scaled by weights


  normalized_mat <- matrix(NA, nrow = n, ncol = ncol(mat))
  for (j in seq_len(ncol(mat))) {
    min_val <- min(mat[, j])
    max_val <- max(mat[, j])

    if (types[j] == 1) {
      #Profit criterion: higher is better
      normalized_mat[, j] <- (mat[, j] - min_val) / (max_val - min_val)
    } else {
      #Cost criterion: lower is better
      normalized_mat[, j] <- (max_val - mat[, j]) / (max_val - min_val)
    }
  }


  #Compute real rating matrix aka Tr
  #Multiply each column of the normalized_mat by the corresponding element in Tp
  Tr <- -sweep(normalized_mat, 2, Tp, FUN = "*")

  #Compute Total Gap Matrix (G) = Tp - Tr
  G <- sweep(Tr, 2, Tp, FUN = "-")

  #Find preference vals
  pref_values <- rowSums(G)

  return(pref_values)
}
