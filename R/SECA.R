#' Apply Simultaneous Evaluation of Criteria and Alternatives (SECA) method
#'
#' @param mat A numeric matrix containing the values for different properties
#' of different alternatives.
#' @param beneficial.vector A numeric vector containing the column indices of beneficial
#' properties. Non-beneficial properties are assumed to be the remaining columns.
#' @param beta A numeric value controlling the balance between criteria variability and
#' similarity. Default is 3.
#'
#' @return A numeric vector containing the calculated weights for each criterion.
#'
#' @examples
#' mat <- matrix(c(80, 60, 90,
#'                 75, 85, 95,
#'                 70, 65, 85,
#'                 60, 75, 80),
#'               nrow = 4, byrow = TRUE)
#' colnames(mat) <- c("Criterion 1", "Criterion 2", "Criterion 3")
#' beneficial.vector <- c(1, 2, 3)
#' apply.SECA(mat, beneficial.vector)
#' @import nloptr
#' @importFrom stats cor runif
#' @export apply.SECA
apply.SECA <- function(mat, beneficial.vector, beta = 3) {

  X <- mat
  for (j in seq_len(ncol(mat))) {
    if (j %in% beneficial.vector) {
      X[, j] <- min(X[, j]) / X[, j]
    } else {
      X[, j] <- X[, j] / max(X[, j])
    }
  }


  std <- apply(X, 2, function(x) sqrt(sum((x - mean(x))^2) / (nrow(X) - 1)))
  std <- std / sum(std)

  #Calculate similarity matrix
  sim.mat <- 1 - cor(X)
  sim.mat <- rowSums(sim.mat) / sum(rowSums(sim.mat))

  #DTarget function
  target_function <- function(variables) {
    Lmb_a <- min(rowSums(X * variables))
    Lmb_b <- sum((variables - std)^2)
    Lmb_c <- sum((variables - sim.mat)^2)
    Lmb <- Lmb_a - beta * (Lmb_b + Lmb_c)
    return(-Lmb)
  }

  #Internal function for gradient of the target function
  gradient_function <- function(variables) {
    grad <- numeric(length(variables))
    for (k in seq_along(variables)) {
      perturbed <- variables
      epsilon <- 1e-8
      perturbed[k] <- variables[k] + epsilon
      grad[k] <- (target_function(perturbed) - target_function(variables)) / epsilon
    }
    return(grad)
  }

  #sum of weights = 1
  constraint_function <- function(variables) {
    return(sum(variables) - 1)
  }

  #Internal function for gradient of the equality constraint
  constraint_gradient <- function(variables) {
    return(rep(1, length(variables)))
  }

  #Set up optimization
  N <- ncol(mat)
  set.seed(42)
  start_vals <- runif(N, 0.001, 1.0)
  start_vals <- start_vals / sum(start_vals)

  result <- nloptr::nloptr(
    x0 = start_vals,
    eval_f = target_function,
    eval_grad_f = gradient_function,
    lb = rep(0.0001, N),
    ub = rep(1.0, N),
    eval_g_eq = constraint_function,
    eval_jac_g_eq = constraint_gradient,
    opts = list(
      "algorithm" = "NLOPT_LD_SLSQP",
      "xtol_rel" = 1.0e-8
    )
  )

  return(result$solution)
}
