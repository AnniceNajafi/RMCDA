#' Apply MULTIMOORA method
#'
#' @param mat A matrix of decision-making criteria values.
#' @param beneficial.vector A vector containing the column indices of beneficial criteria (1-based indexing).
#' @return A list of matrices containing rankings for MOORA, MOORA RP, and MULTIMOORA methods.
#' @examples
#' mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
#'                 420, 91, 1365, 1120, 875, 1190, 200,
#'                 74.2, 70, 189, 210, 112, 217, 112,
#'                 2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53), nrow = 4, byrow = TRUE)
#' beneficial.vector <- c(1, 3) # Columns 1 and 3 are beneficial
#' apply.MULTIMOORA(mat, beneficial.vector)
#' @export apply.MULTIMOORA
apply.MULTIMOORA <- function(mat, beneficial.vector) {

  normalized_data <- sweep(mat, 2, sqrt(colSums(mat^2)), "/")


  all_criteria <- seq_len(ncol(mat))
  non_beneficial.vector <- setdiff(all_criteria, beneficial.vector)

  #MOORA: Calculate scores for beneficial and non-beneficial criteria
  s_plus <- if (length(beneficial.vector) > 0) {
    rowSums(normalized_data[, beneficial.vector, drop = FALSE])
  } else {
    rep(0, nrow(mat))
  }
  s_minus <- if (length(non_beneficial.vector) > 0) {
    rowSums(normalized_data[, non_beneficial.vector, drop = FALSE])
  } else {
    rep(0, nrow(mat))
  }
  moora_scores <- s_plus - s_minus


  best_values <- sapply(all_criteria, function(i) {
    if (i %in% beneficial.vector) {
      max(normalized_data[, i])
    } else {
      min(normalized_data[, i])
    }
  })
  moora_rp_scores <- apply(abs(sweep(normalized_data, 2, best_values, "-")), 1, max)


  multi_scores <- mat[, 1]
  for (i in seq_len(ncol(mat))) {
    if (i %in% beneficial.vector) {
      multi_scores <- multi_scores * mat[, i]
    } else {
      multi_scores <- multi_scores / mat[, i]
    }
  }


  moora_scores <- moora_scores / max(moora_scores)
  moora_rp_scores <- moora_rp_scores / max(moora_rp_scores)
  multi_scores <- multi_scores / max(multi_scores)


  rankings_1 <- cbind(1:nrow(mat), moora_scores)
  rankings_2 <- cbind(1:nrow(mat), moora_rp_scores)
  rankings_3 <- cbind(1:nrow(mat), multi_scores)


  list(MOORA = rankings_1, MOORA_RP = rankings_2, MULTIMOORA = rankings_3)
}
