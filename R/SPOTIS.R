#' Apply the Stable Preference Ordering Towards Ideal Solution (SPOTIS) method
#'
#' @param matrix A numeric matrix or data frame where rows represent alternatives and columns represent criteria.
#' @param weights A numeric vector of weights for each criterion. The sum of weights must equal 1.
#' @param types A numeric vector indicating the type of each criterion: 1 for profit and -1 for cost.
#' @param bounds A numeric matrix where each row contains the minimum and maximum bounds for each criterion.
#'
#' @return A numeric vector of preference scores for alternatives. Lower scores indicate better alternatives.
#' @export
#' @examples
#' # Decision matrix
#' matrix <- matrix(c(10.5, -3.1, 1.7,
#'                    -4.7, 0, 3.4,
#'                    8.1, 0.3, 1.3,
#'                    3.2, 7.3, -5.3), nrow = 4, byrow = TRUE)
#'
#' # Criteria bounds
#' bounds <- matrix(c(-5, 12,
#'                    -6, 10,
#'                    -8, 5), nrow = 3, byrow = TRUE)
#'
#' # Criteria weights
#' weights <- c(0.2, 0.3, 0.5)
#'
#' # Criteria types
#' types <- c(1, -1, 1)
#'
#' # Apply SPOTIS
#' preferences <- apply.SPOTIS(matrix, weights, types, bounds)
#' print(round(preferences, 4))
apply.SPOTIS <- function(matrix, weights, types, bounds) {


  if (!is.matrix(matrix) && !is.data.frame(matrix)) {
    stop("The decision matrix must be a numeric matrix or data frame.")
  }
  if (!is.numeric(weights) || length(weights) != ncol(matrix) || sum(weights) != 1) {
    stop("Weights must be a numeric vector of length equal to the number of criteria, with a sum of 1.")
  }
  if (!is.numeric(types) || length(types) != ncol(matrix) || !all(types %in% c(1, -1))) {
    stop("Types must be a numeric vector of length equal to the number of criteria, containing only 1 or -1.")
  }
  if (!is.matrix(bounds) || nrow(bounds) != ncol(matrix) || ncol(bounds) != 2) {
    stop("Bounds must be a matrix with one row per criterion and two columns (min and max).")
  }
  if (any(bounds[, 1] == bounds[, 2])) {
    stop("Bounds for each criterion must have distinct minimum and maximum values.")
  }

  #Ideal Solution Point (ISP)
  isp <- bounds[cbind(1:ncol(matrix), (types + 1) / 2 + 1)]

  #Normalized distances matrix
  norm_distances <- abs((matrix - isp) / (bounds[, 1] - bounds[, 2]))

  preference_scores <- rowSums(norm_distances * weights)

  return(preference_scores)
}

#' Generate bounds for criteria from a decision matrix
#'
#' @param matrix A numeric matrix or data frame where rows represent alternatives and columns represent criteria.
#'
#' @return A numeric matrix with two columns: minimum and maximum bounds for each criterion.
#' @export
#' @examples
#' # Decision matrix
#' matrix <- matrix(c(96, 145, 200,
#'                    100, 145, 200,
#'                    120, 170, 80,
#'                    140, 180, 140,
#'                    100, 110, 30), nrow = 5, byrow = TRUE)
#'
#' # Generate bounds
#' bounds <- generate.SPOTIS.bounds(matrix)
#' print(bounds)
generate.SPOTIS.bounds <- function(matrix) {
  if (!is.matrix(matrix) && !is.data.frame(matrix)) {
    stop("The decision matrix must be a numeric matrix or data frame.")
  }

  bounds <- cbind(
    apply(matrix, 2, min),
    apply(matrix, 2, max)
  )

  return(bounds)
}
