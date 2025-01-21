#' Multi-objective Optimization on the Basis of Simple Ratio Analysis (MOOSRA)
#'
#' @param mat A matrix of decision-making criteria values for different alternatives.
#' @param weight_vector A vector of weights for the criteria.
#' @param beneficial.vectorA vector of column indices for beneficial criteria.
#' @param show_graph A logical indicating whether to display the ranking graph (default = TRUE).
#' @param verbose A logical indicating whether to print detailed output (default = TRUE).
#'
#' @return A matrix containing the alternatives and their calculated scores, sorted by rank.
#' @examples
#' mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
#' 420, 91, 1365, 1120, 875, 1190, 200,
#' 74.2, 70, 189, 210, 112, 217, 112,
#' 2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
#' 21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
#' 0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
#' 0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)
#' weights <- c(0.1, 0.2, 0.3, 0.1, 0.1, 0.1, 0.1)
#' beneficial.vector<- c(1, 2, 3, 6, 7)
#' apply.MOOSRA(mat, weights, beneficial.vector)
#' @export apply.MOOSRA
apply.MOOSRA <- function(mat, weights, beneficial.vector) {

  normalized_data <- sweep(mat, 2, apply(mat, 2, function(x) sqrt(sum(x^2))), "/")
  weighted_data <- sweep(normalized_data, 2, weights, "*")

  benefit_indices <- beneficial.vector
  cost_indices <- setdiff(seq_len(ncol(mat)), beneficial.vector)

  s_plus <- if (length(benefit_indices) > 0) rowSums(weighted_data[, benefit_indices, drop = FALSE]) else rep(0, nrow(weighted_data))
  s_minus <- if (length(cost_indices) > 0) rowSums(weighted_data[, cost_indices, drop = FALSE]) else rep(0, nrow(weighted_data))

  scores <- s_plus / s_minus
  results <- cbind(1:nrow(mat), scores)

  return(results)
}
