#' Apply Integrated Determination of Objective Criteria Weights (IDOCRIW) method
#'
#' @param mat is a matrix containing the values for different properties
#' of different alternatives
#' @param beneficial.vector is a vector containing the column numbers of beneficial criteria
#'
#' @return a vector containing the calculated weights for the criteria
#'
#' @examples
#' mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
#' 420, 91, 1365, 1120, 875, 1190, 200,
#' 74.2, 70, 189, 210, 112, 217, 112,
#' 2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
#' 21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
#' 0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
#' 0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)
#' colnames(mat) <- c("Toughness Index", "Yield Strength", "Young's Modulus",
#' "Density", "Thermal Expansion", "Thermal Conductivity", "Specific Heat")
#' rownames(mat) <- c("AI 2024-T6", "AI 5052-O", "SS 301 FH",
#' "SS 310-3AH", "Ti-6AI-4V", "Inconel 718", "70Cu-30Zn")
#' beneficial.vector <- c(1, 2, 3, 6, 7)
#' apply.idocriw(mat, beneficial.vector)
#' @export apply.IDOCRIW
apply.IDOCRIW <- function(mat, beneficial.vector) {


  normalized_matrix <- sweep(mat, 2, colSums(mat), "/")


  entropy_values <- rep(0, ncol(normalized_matrix))
  for (col_idx in seq_len(ncol(normalized_matrix))) {
    adjusted_column <- ifelse(normalized_matrix[, col_idx] == 0, 1e-9, normalized_matrix[, col_idx])
    entropy_values[col_idx] <- -sum(adjusted_column * log(adjusted_column)) / log(ncol(normalized_matrix))
  }

  diversity_index <- 1 - entropy_values
  initial_weights <- diversity_index / sum(diversity_index)


  adjusted_matrix <- mat
  non_beneficial_indices <- setdiff(seq_len(ncol(mat)), beneficial.vector)
  for (col_idx in non_beneficial_indices) {
    adjusted_matrix[, col_idx] <- min(mat[, col_idx]) / adjusted_matrix[, col_idx]
  }

  adjusted_matrix <- sweep(adjusted_matrix, 2, colSums(adjusted_matrix), "/")
  max_values <- apply(adjusted_matrix, 2, max)
  priority_matrix <- diag(max_values)

  for (row_idx in seq_along(max_values)) {
    max_row <- which(adjusted_matrix[, row_idx] == max_values[row_idx])[1]
    priority_matrix[row_idx, ] <- adjusted_matrix[max_row, ]
  }

  max_priority <- apply(priority_matrix, 2, max)
  adjustment_matrix <- priority_matrix
  for (col_idx in seq_len(ncol(adjustment_matrix))) {
    adjustment_matrix[, col_idx] <- (-adjustment_matrix[, col_idx] + max_priority[col_idx]) / max_values[col_idx]
  }
  diag(adjustment_matrix) <- -colSums(adjustment_matrix)

  #Define internal function for optimization
  optimization_target <- function(weight_vector) {
    weighted_priority <- adjustment_matrix
    for (row_idx in seq_len(nrow(weighted_priority))) {
      for (col_idx in seq_len(ncol(weighted_priority))) {
        if (weighted_priority[row_idx, col_idx] != 0) {
          weighted_priority[row_idx, col_idx] <- weighted_priority[row_idx, col_idx] * weight_vector[col_idx]
        }
      }
    }
    return(sum(abs(rowSums(weighted_priority))))
  }

  #Optimize
  initial_variables <- rep(1, ncol(adjustment_matrix))
  optimization_result <- optim(par = initial_variables, fn = optimization_target, method = "L-BFGS-B",
                               lower = rep(1e-7, length(initial_variables)), upper = rep(1, length(initial_variables)))

  #Final weights
  final_weights <- optimization_result$par * initial_weights
  final_weights <- final_weights / sum(final_weights)

  return(final_weights)
}
