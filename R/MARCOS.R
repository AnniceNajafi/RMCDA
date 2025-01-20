#' Apply Measurement of Alternatives and Ranking according to Compromise Solution (MARCOS)
#'
#' @param mat is a matrix and contains the values for different properties
#' of different alternatives.
#' @param weights are the weights of each property in the decision-making process.
#' @param beneficial.vector is a vector that contains the column number of beneficial
#' properties.
#' @return a vector containing the aggregated appraisal scores.
#'
#'
#' @examples
#'
#' mat <- matrix(c(660, 1000, 1600, 18, 1200,
#'                 800, 1000, 1600, 24, 900,
#'                 980, 1000, 2500, 24, 900,
#'                 920, 1500, 1600, 24, 900,
#'                 1380, 1500, 1500, 24, 1150,
#'                 1230, 1000, 1600, 24, 1150,
#'                 680, 1500, 1600, 18, 1100,
#'                 960, 2000, 1600, 12, 1150), nrow = 8, byrow = TRUE)
#' weights <- c(0.1061, 0.3476, 0.3330, 0.1185, 0.0949)
#' beneficial.vector <- c(2, 3, 4, 5)  # Columns 2, 3, 4, and 5 are beneficial
#' apply.MARCOS(mat, weights, beneficial.vector)
#' @export apply.MARCOS
apply.MARCOS <- function(mat, weights, beneficial.vector) {

  #Here, we create the extended matrix with ideal and anti-ideal vals
  n <- nrow(mat)
  m <- ncol(mat)

  ideal <- apply(mat, 2, function(x) ifelse(seq_along(x) %in% beneficial.vector, max(x), min(x)))
  anti_ideal <- apply(mat, 2, function(x) ifelse(seq_along(x) %in% beneficial.vector, min(x), max(x)))

  extended_mat <- rbind(mat, ideal, anti_ideal)

  #Normalize
  normalized_mat <- matrix(NA, nrow = nrow(extended_mat), ncol = m)

  for (j in 1:m) {
    for (i in 1:nrow(extended_mat)) {
      if (j %in% beneficial.vector) {
        normalized_mat[i, j] <- extended_mat[i, j] / extended_mat[n + 1, j]
      } else {
        normalized_mat[i, j] <- extended_mat[n + 1, j] / extended_mat[i, j]
      }
    }
  }


  weighted_mat <- t(weights * t(normalized_mat))


  S <- rowSums(weighted_mat)
  k_neg <- S / S[n + 2]
  k_pos <- S / S[n + 1]

  #Utility functions
  f_k_pos <- k_neg[1:n] / (k_pos[1:n] + k_neg[1:n])
  f_k_neg <- k_pos[1:n] / (k_pos[1:n] + k_neg[1:n])
  f_k <- (k_pos[1:n] + k_neg[1:n]) / (1 + (1 - f_k_pos) / f_k_pos + (1 - f_k_neg) / f_k_neg)

  return(f_k)
}
