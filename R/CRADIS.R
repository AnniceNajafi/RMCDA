#' Function to apply CRiteria Aggregation for Decision Information Synthesis (CRADIS)
#'
#' @param mat is a matrix containing the values for different properties of different alternatives
#' @param weights are the weights of each property in the decision-making process
#' @param beneficial.vector is a vector that contains the column numbers of beneficial criteria
#' @return a vector containing the preference values for each alternative
#' @export
#'
#' @examples
#'
#' mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
#' 420, 91, 1365, 1120, 875, 1190, 200,
#' 74.2, 70, 189, 210, 112, 217, 112,
#' 2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
#' 21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
#' 0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
#' 0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)
#' colnames(mat) <- c("Toughness Index", "Yield Strength", "Young's Modulus",
#'                   "Density", "Thermal Expansion", "Thermal Conductivity", "Specific Heat")
#' rownames(mat) <- c("AI 2024-T6", "AI 5052-O", "SS 301 FH",
#'                    "SS 310-3AH", "Ti-6AI-4V", "Inconel 718", "70Cu-30Zn")
#' weights <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)
#' beneficial.vector <- c(1, 2, 3)
#' apply_CRADIS(mat, weights, beneficial.vector)
apply.CRADIS <- function(mat, weights, beneficial.vector) {

  if (length(weights) != ncol(mat)) {
    stop("Unable to proceed. Number of weights must match the number of criteria (columns) in the matrix.")
  }

  if (any(beneficial.vector < 1 | beneficial.vector > ncol(mat))) {
    stop("Unable to proceed. beneficial.vector must contain column numbers within the range of the matrix.")
  }

  normalized.mat <- matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
  min.vector <- apply(mat, 2, min)
  max.vector <- apply(mat, 2, max)

  for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
      if (j %in% beneficial.vector) {
        normalized.mat[i, j] <- (mat[i, j] - min.vector[j]) / (max.vector[j] - min.vector[j])
      } else {
        normalized.mat[i, j] <- (max.vector[j] - mat[i, j]) / (max.vector[j] - min.vector[j])
      }
    }
  }


  weighted.mat <- t(weights * t(normalized.mat))

  #Calculate deviations from ideal and anti-ideal solutions
  ideal.solution <- apply(weighted.mat, 2, max)
  anti.ideal.solution <- apply(weighted.mat, 2, min)

  Sp <- rowSums(matrix(rep(ideal.solution, each = nrow(weighted.mat)), nrow = nrow(weighted.mat)) - weighted.mat)
  Sm <- rowSums(weighted.mat - matrix(rep(anti.ideal.solution, each = nrow(weighted.mat)), nrow = nrow(weighted.mat)))

  Sop <- sum(ideal.solution - apply(weighted.mat, 2, max))
  Som <- sum(apply(weighted.mat, 2, max) - apply(weighted.mat, 2, min))

  Kp <- Sop / Sp
  Km <- Sm / Som

  preference.values <- (Kp + Km) / 2

  return(preference.values)
}
