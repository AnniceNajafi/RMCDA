#' Apply WISP (Integrated Simple Weighted Sum Product)
#'
#' @description
#' Performs the WISP method calculations, returning a utility score for each alternative.
#' Columns whose indices appear in \code{beneficial.vector} are treated as beneficial (max);
#' all other columns are treated as non-beneficial (min).
#'
#' @param mat A numeric matrix with alternatives in rows and criteria in columns.
#' @param beneficial.vector An integer vector of column indices that are beneficial ("max") criteria.
#'   All columns not in \code{beneficial.vector} are assumed to be "min".
#' @param weights A numeric vector of weights, one for each criterion (same length as the number of columns of \code{mat}).
#' @param simplified A logical. If \code{FALSE} (default), uses all four partial utilities;
#'   if \code{TRUE}, uses only \code{n_wsd} and \code{n_wpr} in the final aggregation.
#'
#' @details
#' **Steps**:
#' 1. Normalize \code{weights} so they sum to 1.
#' 2. \code{best[j]} = max of column \code{j} in \code{mat}.
#' 3. Normalize matrix by dividing each column \code{j} by \code{best[j]},
#'    then multiply each entry by \code{weights[j]}.
#' 4. For each row \code{i}:
#'    - Add up \code{v_p[i]} and multiply \code{w_p[i]} for each beneficial criterion column.
#'    - Add up \code{v_m[i]} and multiply \code{w_m[i]} for each non-beneficial column.
#' 5. Compute partial utilities:
#'    - \code{u.wsd} = (v_p - v_m)
#'    - \code{u.wpd} = (w_p - w_m)
#'    - \code{u.wsr}, \code{u.wpr} depend on whether we have \emph{only} beneficial or \emph{only} non-beneficial criteria,
#'      or a mix of both.
#' 6. Normalize each partial utility to \([0,1]\) and combine into a final utility vector.
#'
#' @return A numeric vector of length \code{nrow(mat)} with the final WISP utility scores.
#'
#' @examples
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
#'
#' # Suppose the first two columns are beneficial, and the 3rd is non-beneficial
#' beneficial.vector <- c(1,2, 4)
#' weights <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)
#'
#' # Get the WISP scores
#' apply.WISP(mat, beneficial.vector, weights, simplified=FALSE)
#'
#' @export
apply.WISP <- function(mat,
                       beneficial.vector,
                       weights,
                       simplified = FALSE) {


  weights <- weights / sum(weights)

  X <- as.matrix(mat)

  n <- nrow(X)
  m <- ncol(X)

  eps <- 1e-16
  best <- apply(X, 2, max) + eps


  for (j in seq_len(m)) {
    X[, j] <- (X[, j] / best[j]) * weights[j]
  }


  non_beneficial <- setdiff(seq_len(m), beneficial.vector)

  #Initialize vectors (v_p, v_m, w_p, w_m) for each row
  #        v_p, v_m => sum-based
  #        w_p, w_m => product-based
  v_p <- numeric(n)
  v_m <- numeric(n)
  w_p <- rep(1, n)
  w_m <- rep(1, n)


  for (i in seq_len(n)) {

    for (j in beneficial.vector) {
      v_p[i] <- v_p[i] + X[i, j]
      w_p[i] <- w_p[i] * X[i, j]
    }

    for (j in non_beneficial) {
      v_m[i] <- v_m[i] + X[i, j]
      w_m[i] <- w_m[i] * X[i, j]
    }
  }

  #Compute partial utilities
  # WSD & WPD: difference-based
  u_wsd <- v_p - v_m
  u_wpd <- w_p - w_m

  #WSR & WPR: ratio-based
  has_beneficial <- (length(beneficial.vector) > 0)
  has_nonbeneficial <- (length(non_beneficial) > 0)

  if (has_beneficial && !has_nonbeneficial) {

    u_wsr <- v_p
    u_wpr <- w_p
  } else if (!has_beneficial && has_nonbeneficial) {

    u_wsr <- 1 / (v_m + eps)
    u_wpr <- 1 / (w_m + eps)
  } else {

    u_wsr <- v_p / (v_m + eps)
    u_wpr <- w_p / (w_m + eps)
  }


  # n.x = (1 + x) / (1 + max(x))
  normalize_01 <- function(vec) {
    (1 + vec) / (1 + max(vec))
  }

  n_wsd <- normalize_01(u_wsd)
  n_wpd <- normalize_01(u_wpd)
  n_wsr <- normalize_01(u_wsr)
  n_wpr <- normalize_01(u_wpr)

  #Final utility
  if (!simplified) {
    #IF simplified=FALSE => average of all four
    u <- (n_wsd + n_wpd + n_wsr + n_wpr) / 4
  } else {
    #ELSE => average of WSD & WPR only
    u <- (n_wsd + n_wpr) / 2
  }

  return(u)
}
