#' Apply CILOS Weighting Method
#'
#' @param mat A numeric matrix representing decision criteria values.
#' @param beneficial.vector A numeric vector indicating the column indices of beneficial criteria.
#'
#' @return A numeric vector of calculated weights.
#'
#' @examples
#'
#' mat <- matrix(
#'   c(75.5, 95, 770, 187, 179, 239, 237,
#'     420, 91, 1365, 1120, 875, 1190, 200,
#'     74.2, 70, 189, 210, 112, 217, 112,
#'     2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
#'     21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
#'     0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
#'     0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06),
#'   nrow = 7, byrow = TRUE
#' )
#' beneficial.vector <- c(1, 2, 3, 6, 7)
#' apply.CILOS(mat, beneficial.vector)
#' @importFrom pracma nullspace
#' @export apply.CILOS
apply.CILOS <- function(mat, beneficial.vector) {

  normalize.criteria <- function(mat, beneficial.vector = NULL) {
    norm.mat <- mat
    if (!is.null(beneficial.vector)) {
      for (j in seq_len(ncol(mat))) {
        if (!(j %in% beneficial.vector)) {
          norm.mat[, j] <- min(mat[, j]) / mat[, j]
        }
      }
    }
    return(norm.mat)
  }

  sum.normalize <- function(mat) {
    return(t(t(mat) / colSums(mat)))
  }

  nmat <- normalize.criteria(mat, beneficial.vector)
  nmat <- sum.normalize(nmat)

  selected.max <- nmat[apply(nmat, 2, which.max), ]
  diag.max <- diag(selected.max)

  preference.mat <- t(t(t((diag.max) - t(selected.max))) / diag.max)

  influence.mat <- preference.mat - diag(colSums(preference.mat))

  null.vector <- nullspace(influence.mat)

  final.weights <- (null.vector / sum(null.vector))[,1]

  return(final.weights)
}
