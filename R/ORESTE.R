#' Apply the ORESTE (Organisation Rangement Et SynThèsE de données relationnelles) Method
#'
#' @description
#' Criteria with indexes in
#' \code{beneficial.vector} are interpreted as beneficial (maximize), whereas
#' others are cost-type (minimize). Rankings are performed for both the data
#' matrix and the weights, then combined in the ORESTE manner.
#'
#' @param mat A numeric matrix with each row representing an alternative and each column a criterion.
#' @param weights A numeric vector of weights for each criterion (same length as number of columns).
#' @param beneficial.vector An integer vector of column indices specifying which criteria are "max" (beneficial).
#' @param alpha A numeric parameter controlling the relative weight of data-based and weight-based ranks.
#' @return A numeric vector of ORESTE scores (summed ranks) for each alternative.
#'
#' @examples
#' mat <- matrix(c(10, 2,
#'                 20, 4,
#'                 15, 5),
#'               nrow = 3, byrow = TRUE)
#' weights <- c(0.7, 0.3)
#' beneficial.vector <- c(1)   # 1st column "max", 2nd column "min"
#'
#' apply.ORESTE(mat, weights, beneficial.vector, alpha = 0.4)
#'
#' @export
apply.ORESTE <- function(mat,
                         weights,
                         beneficial.vector,
                         alpha   = 0.4) {


  if (ncol(mat) != length(weights)) {
    stop("Number of weights must match the number of columns in 'mat'.")
  }

  n_alts <- nrow(mat)
  n_crit <- ncol(mat)

  criterion_type <- rep("min", n_crit)
  criterion_type[beneficial.vector] <- "max"


  X <- matrix(as.numeric(mat), nrow = n_alts, ncol = n_crit)

  w <- rank(-weights, ties.method = "min")

  for (j in seq_len(n_crit)) {
    if (criterion_type[j] == "max") {
      X[, j] <- rank(-X[, j], ties.method = "min")
    } else {
      X[, j] <- rank( X[, j], ties.method = "min")
    }
  }


  r_ind <- matrix(0, nrow = n_alts, ncol = n_crit)
  for (i in seq_len(n_alts)) {
    for (j in seq_len(n_crit)) {
      r_ind[i, j] <- alpha * X[i, j] + (1 - alpha) * w[j]
    }
  }

  r_ind_flat <- as.vector(r_ind)
  ranked     <- rank(r_ind_flat, ties.method = "min")


  ranked_mat <- matrix(ranked, nrow = n_alts, ncol = n_crit, byrow = FALSE)

  #Sum across each row (which corresponds to each alternative)
  total <- rowSums(ranked_mat)



  return(total)
}
