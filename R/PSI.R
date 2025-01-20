#' Apply Preference Selection Index (PSI) method
#'
#' @param mat A numeric matrix containing the values for different properties
#' of different alternatives.
#' @param beneficial.vector A numeric vector containing the column indices of beneficial
#' criteria. Non-beneficial criteria are assumed to be the remaining columns.
#' @return A numeric vector containing the calculated PSI scores for each alternative.
#'
#' @examples
#' mat <- matrix(c(80, 60, 90,
#'                 75, 85, 95,
#'                 70, 65, 85,
#'                 60, 75, 80),
#'               nrow = 4, byrow = TRUE)
#' colnames(mat) <- c("Criterion 1", "Criterion 2", "Criterion 3")
#' beneficial.vector <- c(1, 2, 3)
#' apply.PSI(mat, beneficial.vector)
#' @export apply.PSI
apply.PSI <- function(mat, beneficial.vector) {

  X <- mat
  for (j in seq_len(ncol(mat))) {
    if (j %in% beneficial.vector) {
      X[, j] <- X[, j] / max(X[, j])
    } else {
      X[, j] <- min(X[, j]) / X[, j]
    }
  }


  R <- colMeans(X)
  Z <- (X - matrix(R, nrow = nrow(X), ncol = ncol(X), byrow = TRUE))^2
  PV <- colSums(Z)
  T <- 1 - PV
  P <- T / sum(T)
  I <- rowSums(X * matrix(P, nrow = nrow(X), ncol = ncol(X), byrow = TRUE))


  return(I)
}
