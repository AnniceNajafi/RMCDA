#' Function to apply Reference Ideal Method (RIM)
#' Note:
#' function is rewritten from the MCDM package to match the formatting of the R RMCDA package
#' SOURCE: https://github.com/cran/MCDM/blob/master/R/RIM.R
#'
#'
#' The apply.RIM function implements the Reference Ideal Method (RIM) for
#' multi-criteria decision making (MCDM) problems, allowing for degenerate intervals,
#' i.e. cases where A == C or D == B.
#'
#' @param mat A matrix m x n containing the values of the m alternatives
#' for the n criteria.
#' @param weights A numeric vector of length n, containing the weights for the criteria.
#' The sum of the weights must be equal to 1.
#' @param AB A matrix (2 x n), where the first row of AB corresponds to the A extreme,
#' and the second row of AB corresponds to the B extreme of the domain (universe of discourse)
#' for each criterion.
#' @param CD A matrix (2 x n), where the first row of CD corresponds to the C extreme,
#' and the second row of CD corresponds to the D extreme of the ideal reference for each criterion.
#'
#'
#' Degenerate intervals:
#'
#' 1. If the first element of AB matches the first element of CD, then the interval between A and C collapses to a point.
#'    - Any value x within this range is treated under a fallback rule:
#'      - If x equals both A and C, the normalized value is set to 1.
#'      - Otherwise, the normalized value is set to 0.
#'
#' 2. If the second element of CD matches the second element of AB, then the interval between D and B collapses to a point.
#'    - A similar fallback applies:
#'      - If x equals both D and B, the normalized value is set to 1.
#'      - Otherwise, the normalized value is set to 0.
#'
#' These fallback rules ensure the function does not stop but, instead, issues a warning and assigns
#' a default. Adjust these defaults if your MCDM context requires different handling.
#'
#' @return
#' A data frame containing:
#'
#' - Alternatives: The index of each alternative.
#' - R: The R index (score) for each alternative.
#' - Ranking: The ranking of the alternatives based on the R score.
#'
#' Reference:
#' Cables, E.; Lamata, M.T.; Verdegay, J.L. (2016).
#' RIM-reference ideal method in multicriteria decision making.
#' Information Science, 337-338, 1-10.
#'
#'
#' @examples
#'
#' # Example decision matrix
#' mat <- matrix(
#'   c(30,40,25,27,45,0,
#'     9,0,0,15,2,1,
#'     3,5,2,3,3,1,
#'     3,2,3,3,3,2,
#'     2,2,1,4,1,2),
#'   nrow = 5, ncol = 6, byrow = TRUE
#' )
#'
#' #Example weights vector (must sum to 1)
#' weights <- c(0.2262,0.2143,0.1786,0.1429,0.119,0.119)
#'
#' #Example AB matrix
#' AB <- matrix(
#'   c(23,60,0,15,0,10,
#'     1,3,1,3,1,5),
#'   nrow = 2, ncol = 6, byrow = TRUE
#' )
#'
#' #Example CD matrix
#' CD <- matrix(
#'   c(30,35,10,15,0,0,
#'     3,3,3,3,4,5),
#'   nrow = 2, ncol = 6, byrow = TRUE
#' )
#'
#'
#' apply.RIM(mat, weights, AB, CD)
#'
#'@export apply.RIM
apply.RIM <- function(mat, weights, AB, CD){

  # 0. Argument checks
  if (!is.matrix(mat)) {
    stop("'mat' must be a matrix with the values of the alternatives.")
  }
  if (missing(weights)) {
    stop("A vector containing n weights (summing up to 1) should be provided.")
  }
  if (abs(sum(weights) - 1) > 1e-9) {
    stop("The sum of 'weights' is not equal to 1.")
  }
  if (length(weights) != ncol(mat)) {
    stop("Length of 'weights' does not match the number of criteria.")
  }
  if (!is.matrix(AB) || ncol(AB) != ncol(mat)) {
    stop("Dimensions of 'AB' do not match the number of criteria.")
  }
  if (!is.matrix(CD) || ncol(CD) != ncol(mat)) {
    stop("Dimensions of 'CD' do not match the number of criteria.")
  }


  N <- matrix(nrow = nrow(mat), ncol = ncol(mat))

  for (j in seq_len(ncol(mat))) {
    A <- AB[1, j]
    B <- AB[2, j]
    C_ <- CD[1, j]
    D_ <- CD[2, j]

    for (i in seq_len(nrow(mat))) {
      x <- mat[i, j]

      #We have to check to see if x is within the domain [A, B]
      if (x < A || x > B) {
        warning(paste0(
          "Value x = ", x, " is outside [A, B] = [", A, ", ", B,
          "] for row i=", i, ", column j=", j, ". Setting N=0."
        ))
        N[i, j] <- 0
        next
      }

      #Either x is in [C, D]
      if (x >= C_ && x <= D_) {
        N[i, j] <- 1

        #OR x is in [A, C]
      } else if (x >= A && x <= C_) {

        denom <- abs(A - C_)
        if (denom < 1e-15) {
          #Degenerate interval: A == C
          if (abs(x - A) < 1e-15) {
            N[i, j] <- 1  #x is exactly the same as A=C
          } else {
            #x is "in" a zero-length interval. Assign default = 0
            N[i, j] <- 0
            warning(
              sprintf(
                "Degenerate interval [A,C] (A=%g, C=%g) at col j=%d. ",
                A, C_, j
              ),
              "Setting N=0 for row=", i
            )
          }
        } else {

          N[i, j] <- 1 - (
            min(abs(x - C_), abs(x - D_)) / denom
          )
        }

        #OR x is in [D, B]
      } else if (x >= D_ && x <= B) {

        denom <- abs(D_ - B)
        if (denom < 1e-15) {
          #Degenerate interval: D == B
          if (abs(x - B) < 1e-15) {
            N[i, j] <- 1
          } else {
            N[i, j] <- 0
            warning(
              sprintf(
                "Degenerate interval [D,B] (D=%g, B=%g) at col j=%d. ",
                D_, B, j
              ),
              "Setting N=0 for row=", i
            )
          }
        } else {

          N[i, j] <- 1 - (
            min(abs(x - C_), abs(x - D_)) / denom
          )
        }

      } else {
        #If none of the above conditions are met, x cannot be normalized
        #Consequently, assign 0 or handle differently if needed.
        N[i, j] <- 0
        warning(paste0(
          "Value x = ", x,
          " does not fall into [A,C], [C,D], or [D,B]. ",
          "Setting N=0 for row i=", i, ", column j=", j
        ))
      }
    }
  }


  W  <- diag(weights)
  NW <- N %*% W


  pos.Dis <- numeric(nrow(mat))
  neg.Dis <- numeric(nrow(mat))

  for (i in seq_len(nrow(mat))) {
    pos.Dis[i] <- sqrt(sum((NW[i, ] - weights)^2))
    neg.Dis[i] <- sqrt(sum(NW[i, ]^2))
  }

  #R index
  R <- neg.Dis / (neg.Dis + pos.Dis)

  return(data.frame(
    Alternatives = seq_len(nrow(mat)),
    R            = R,
    Ranking      = rank(-R, ties.method = "first")
  ))
}
