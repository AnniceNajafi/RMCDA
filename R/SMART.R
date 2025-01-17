#' Apply the SMART Method
#'
#' @description
#' This function implements the SMART (Simple Multi-Attribute Rating Technique) method in R.
#'
#' @param dataset A numeric matrix or data frame of size (n x m), rows = alternatives, columns = criteria.
#' @param grades A numeric vector of length \code{m} (one grade per criterion).
#'   They get transformed into weights via \eqn{(2^{1/2})^{grades}} and normalized.
#' @param lower A numeric vector of length \code{m} with lower bounds for each criterion.
#' @param upper A numeric vector of length \code{m} with upper bounds for each criterion.
#' @param beneficial.vector A numeric vector containing column indices that are beneficial ("max").
#'
#' @return A matrix (or data frame) named \code{result} with two columns:
#'   \itemize{
#'     \item \code{index}: The row index (alternative).
#'     \item \code{score}: The final SMART score for that alternative.
#'   }
#'   The rows of \code{result} are sorted by \code{score} in descending order.
#'
#' @details
#' The main steps are:
#' \enumerate{
#'   \item Transform \code{grades} into weights via \eqn{w_i = (2^{1/2})^{grades_i}} and normalize \eqn{w}.
#'   \item For each column \eqn{i}:
#'     \itemize{
#'       \item Compute a scaled value
#'         \eqn{X_{:,i} = 4 + \log_2\bigl(\bigl[(X_{:,i} - lower[i])/(upper[i]-lower[i])\bigr]\cdot 64\bigr)}
#'         if \eqn{i} is beneficial, or
#'         \eqn{X_{:,i} = 10 - \log_2\bigl(\bigl[(X_{:,i} - lower[i])/(upper[i]-lower[i])\bigr]\cdot 64\bigr)}
#'         if \eqn{i} is non-beneficial.
#'     \item If \eqn{(X_{:,i} - lower[i])/(upper[i]-lower[i])} is zero or negative, the log term might be invalid,
#'           so ensure your data/bounds are set correctly.
#'     }
#'   \item Compute final scores \eqn{Y_j = \sum_i X_{j,i} \cdot w_i} for each alternative \eqn{j}.
#'   \item Return a sorted \code{result} table that includes \code{(index, score)}.
#' }
#'
#' @examples
#' # Example usage
#' data_mat <- matrix(c(10, 20, 15,  7,
#'                      30,  5,  8, 25),
#'                    nrow = 2, byrow = TRUE)
#' # Suppose we have 4 criteria (2 rows, 4 columns)
#' # We'll treat columns 1, 2, 3 as beneficial, and column 4 as non-beneficial
#' benef_vec <- c(1, 2, 3)
#'
#' # Grades for each of 4 criteria
#' grades <- c(2, 2, 1, 3)
#' lower  <- c(0, 0, 0,  0)
#' upper  <- c(40, 40, 40, 40)
#'
#' # Run SMART
#' result <- apply.SMART(dataset = data_mat,
#'                     grades = grades,
#'                     lower  = lower,
#'                     upper  = upper,
#'                     beneficial.vector = benef_vec)
#'
#' result
#'
#' @export
apply.SMART <- function(dataset,
                        grades,
                        lower,
                        upper,
                        beneficial.vector,
                        graph   = TRUE,
                        verbose = TRUE) {


  X <- as.matrix(dataset)

  #Transform grades into weights

  w <- grades
  for (i in seq_along(w)) {
    w[i] <- (sqrt(2))^w[i]
  }
  w <- w / sum(w)

  n_alt <- nrow(X)
  n_crit <- ncol(X)


  for (i in seq_len(n_crit)) {

    #We'll assume user provides valid bounds; otherwise:
    denom <- upper[i] - lower[i]
    if (denom <= 0) {
      stop(sprintf("Invalid bounds for criterion %d: upper <= lower.", i))
    }


    frac_vec <- (X[, i] - lower[i]) / denom * 64

    #If beneficial
    if (i %in% beneficial.vector) {

      X[, i] <- 4 + log2(frac_vec)
    } else {

      X[, i] <- 10 - log2(frac_vec)
    }
  }


  S <- sweep(X, 2, w, FUN = "*")
  Y <- rowSums(S)


  result <- cbind(index = seq_len(n_alt), score = Y)


  return(result)
}
