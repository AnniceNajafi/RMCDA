#' Apply Multi-Attribute Utility Theory (MAUT) Method
#'
#' @param mat is a matrix containing values for different properties of different alternatives
#' @param weights are the weights of each property in the decision-making process
#' @param beneficial.vector is a vector containing the column numbers of beneficial properties
#' @param utility.functions is a vector specifying the utility function for each criterion ('exp', 'step', 'quad', 'log', 'ln')
#' @param step.size is a numeric value used for the step utility function (default is 1)
#'
#' @return a matrix containing the calculated utility scores
#'
#' @export apply.MAUT
#' @examples
#' mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237, 420, 91), nrow = 3, byrow = TRUE)
#' weights <- c(0.3, 0.5, 0.2)
#' beneficial.vector <- c(1, 3)
#' utility.functions <- c("exp", "log", "quad")
#' step.size <- 1
#' result <- apply.MAUT(mat, weights, beneficial.vector, utility.functions, step.size)
apply.MAUT <- function(mat, weights, beneficial.vector, utility.functions, step.size = 1){

  X <- as.matrix(mat)

  for (j in seq_len(ncol(X))) {
    if (j %in% beneficial.vector) {
      X[, j] <- (X[, j] - min(X[, j])) / (max(X[, j]) - min(X[, j]) + 1e-15)
    } else {
      X[, j] <- 1 + (min(X[, j]) - X[, j]) / (max(X[, j]) - min(X[, j]) + 1e-15)
    }
  }

  u.exp <- function(x) (exp(x^2) - 1) / 1.72
  u.step <- function(x, op) ceiling(op * x) / op
  u.log <- function(x) log10(9 * x + 1)
  u.ln <- function(x) log((exp(1) - 1) * x + 1)
  u.quad <- function(x) (2 * x - 1)^2

  for (i in seq_len(ncol(X))) {
    if (utility.functions[i] == "exp") {
      X[, i] <- sapply(X[, i], u.exp)
    } else if (utility.functions[i] == "step") {
      X[, i] <- sapply(X[, i], u.step, op = step.size)
    } else if (utility.functions[i] == "quad") {
      X[, i] <- sapply(X[, i], u.quad)
    } else if (utility.functions[i] == "log") {
      X[, i] <- sapply(X[, i], u.log)
    } else if (utility.functions[i] == "ln") {
      X[, i] <- sapply(X[, i], u.ln)
    }
  }

  for (i in seq_len(ncol(X))) {
    X[, i] <- X[, i] * weights[i]
  }

  Y <- rowSums(X)
  result <- cbind(1:length(Y), Y)
  colnames(result) <- c("Alternative", "Score")

  return(result)
}


