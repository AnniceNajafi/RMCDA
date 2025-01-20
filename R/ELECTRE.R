#' Apply ELECTRE I method
#'
#' @param alternatives A matrix or data frame where rows represent alternatives and columns represent criteria.
#' @param weights A numeric vector of weights for each criterion.
#' @param concordance.threshold A numeric value (0 to 1) specifying the concordance threshold.
#' @param discordance.threshold A numeric value (0 to 1) specifying the discordance threshold.
#'
#' @return Ranking scores for each alternative.
#'
#' @examples
#' alternatives <- data.frame(
#'   cost = c(500, 600, 450),
#'   quality = c(8, 7, 9),
#'   delivery = c(4, 3, 5),
#'   support = c(9, 8, 7)
#' )
#'
#' # Define criteria weights
#' weights <- c(0.3, 0.4, 0.2, 0.1)
#'
#' # Apply ELECTRE I method
#' results <- apply.ELECTRE1(
#'   alternatives = alternatives,
#'   weights = weights,
#'   concordance.threshold = 0.6,
#'   discordance.threshold = 0.7
#' )
#' @export apply.ELECTRE1
apply.ELECTRE1 <- function(alternatives, weights, concordance.threshold, discordance.threshold) {


  if (!is.data.frame(alternatives) && !is.matrix(alternatives)) {
    stop("Alternatives must be a matrix or data frame.")
  }
  if (!is.numeric(weights) || length(weights) != ncol(alternatives)) {
    stop("Weights must be a numeric vector with length equal to the number of criteria.")
  }
  if (concordance.threshold < 0 || concordance.threshold > 1) {
    stop("Concordance threshold must be between 0 and 1.")
  }
  if (discordance.threshold < 0 || discordance.threshold > 1) {
    stop("Discordance threshold must be between 0 and 1.")
  }


  num.alternatives <- nrow(alternatives)
  concordance.matrix <- matrix(0, nrow = num.alternatives, ncol = num.alternatives)
  discordance.matrix <- matrix(0, nrow = num.alternatives, ncol = num.alternatives)

  #Concordance and discordance indices
  for (i in 1:num.alternatives) {
    for (j in 1:num.alternatives) {
      if (i != j) {
        better.criteria <- alternatives[i, ] >= alternatives[j, ]
        concordance.matrix[i, j] <- sum(weights[better.criteria])

        worse.criteria <- alternatives[i, ] < alternatives[j, ]
        if (any(worse.criteria)) {
          discordance.matrix[i, j] <- max(abs(alternatives[i, worse.criteria] - alternatives[j, worse.criteria])) /
            max(abs(alternatives[i, ] - alternatives[j, ]))
        }
      }
    }
  }

  #Infinite discordance values
  discordance.matrix[is.infinite(discordance.matrix)] <- 0


  preference.matrix <- (concordance.matrix >= concordance.threshold) &
    (discordance.matrix <= discordance.threshold)
  preference.matrix <- as.data.frame(preference.matrix)


  rankings <- rowSums(preference.matrix)

  return(rankings)
}
