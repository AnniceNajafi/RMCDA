#' Apply TODIM (TOmada de Decisao Interativa e Multicriterio)
#'
#' @description
#' Implements the core TODIM logic in R
#' #' **What is \eqn{\theta}?**
#' In the TODIM formula, \eqn{\theta} acts as an “attenuation factor” or penalty
#' for negative dominance differences. Specifically, when \(p_i < p_j\), the term
#' \(-\frac{1}{\theta}\) is used in computing the dominance. This parameter allows
#' you to adjust how severely negative differences weigh in the final scoring.
#' A common default is 1, but you could experiment with other values if you want
#' to amplify or reduce the penalty effect.
#'
#' If you set \code{teta = 1}, it uses the standard TODIM approach.
#' If you do not want to vary this parameter, you can leave it at its default value of 1.
#'
#' **Steps**:
#' 1. Normalize each criterion column:
#'    - Beneficial criteria: \(x_{ij} / \sum(x_{ij})\)
#'    - Non-beneficial: \(\frac{1/x_{ij}}{\sum(1/x_{ij})}\)
#' 2. Normalize the weights by dividing each weight by the maximum weight.
#' 3. Compute the dominance matrix \(\mathbf{D}\). For each pair \((i,j)\):
#'    \[
#'      D_{ij} = \begin{cases}
#'         \displaystyle \sum_{k} \left(\frac{w_k (p_i - p_j)}{\sum w_k}\right)^{\frac12}, & p_i > p_j \\
#'         0, & p_i = p_j \\
#'         \displaystyle -\frac{1}{\theta} \left(\sum w_k \frac{(p_j - p_i)}{w_k}\right)^{\frac12}, & p_i < p_j
#'      \end{cases}
#'    \]
#' 4. Obtain the total dominance flow \(\mathbf{r}\) by summing each row of \(\mathbf{D}\).
#'    Rescale \(\mathbf{r}\) to the [0, 1] interval.
#'
#'
#' @param mat A numeric matrix where each row is an alternative and each column is a criterion.
#' @param weights A numeric vector of weights for each criterion (same length as number of columns of \code{mat}).
#' @param beneficial.vector A vector of column indices corresponding to beneficial criteria
#'   (i.e., the larger the value, the better). Columns not listed here will be treated as non-beneficial.
#' @param teta A numeric scalar (\eqn{\theta} in TODIM). Default is 1.
#'
#' @return A numeric vector of rescaled scores, one per alternative (row).
#'
#' @examples
#' # Small synthetic example
#' mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
#' 420, 91, 1365, 1120, 875, 1190, 200,
#' 74.2, 70, 189, 210, 112, 217, 112,
#' 2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
#' 21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
#' 0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
#' 0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)
#'
#' colnames(mat)<-c("Toughness Index",	"Yield Strength",	"Young's Modulus",
#' "Density",	"Thermal Expansion",	"Thermal Conductivity","Specific Heat")
#' rownames(mat)<-c("AI 2024-T6", "AI 5052-O","SS 301 FH",
#' "SS 310-3AH","Ti-6AI-4V","Inconel 718","70Cu-30Zn")
#' weights <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)
#' beneficial.vector<-c(1,2,3)
#'
#' apply.TODIM(mat, weights, beneficial.vector, teta=1)
#'
#' @export
apply.TODIM <- function(mat, weights, beneficial.vector, teta = 1) {

  X <- as.matrix(mat)
  n <- nrow(X)
  m <- ncol(X)


  for (j in beneficial.vector) {
    X[, j] <- X[, j] / sum(X[, j])
  }

  non.beneficial <- setdiff(seq_len(m), beneficial.vector)
  for (j in non.beneficial) {
    X[, j] <- (1 / X[, j]) / sum(1 / X[, j])
  }


  weights <- weights / max(weights)
  sumW <- sum(weights)

  #Dominance matrix D
  D <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i != j) {
        for (k in seq_len(m)) {
          p_i <- X[i, k]
          p_j <- X[j, k]
          diff <- p_i - p_j

          if (diff > 0) {

            D[i, j] <- D[i, j] + ((weights[k] * diff) / sumW)^0.5
          } else if (abs(diff) < .Machine$double.eps) {

            D[i, j] <- D[i, j] + 0
          } else {


            D[i, j] <- D[i, j] + (-1 / teta) * (
              sumW * ((p_j - p_i) / weights[k])
            )^0.5
          }
        }
      }
    }
  }


  r <- rowSums(D)
  min_r <- min(r)
  max_r <- max(r)
  if (abs(max_r - min_r) < .Machine$double.eps) {
    #Avoid division by zero in case all D sums are identical
    r <- rep(1, length(r))
  } else {
    r <- (r - min_r) / (max_r - min_r)
  }

  return(r)
}

