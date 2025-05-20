#' Apply Weighted Sum Model (WSM) or Weighted Product Model (WPM) on data
#'
#' @param mat is a matrix and contains the values for different properties
#' of different alternatives.
#' @param weights are the weights of each property in the decision making process
#' @param beneficial.vector is a vector that contains the column number of beneficial
#' properties.
#' @param method can either be 'WSM' or 'WPM', set to 'WSM' by default.
#'
#' @return a vector containing the calculated preference score, run rank(-apply.WSM(mat, beneficial.vector, weights))
#' to get the ranks.
#'
#' @examples
#' mat <- matrix(c(250, 200, 300, 275, 225, 16, 16, 32, 32, 16, 12, 8, 16, 8, 16, 5, 3, 4, 4, 2), nrow=5, ncol=4)
#' colnames(mat)<-c("Price", "Storage space", "Camera", "Looks")
#' rownames(mat)<-paste0("Mobile ", seq(1, 5, 1))
#' beneficial.vector <- c(2, 3, 4)
#' weights <- c(0.25, 0.25, 0.25, 0.25)
#' apply.WSM_WPM(mat, beneficial.vector, weights, "WSM")
#'
#' @importFrom matrixStats rowProds
#' @export apply.WSM_WPM
apply.WSM_WPM <- function(mat, beneficial.vector, weights, method="WSM"){

  if(!(method %in% c("WSM", "WPM"))){

    stop("Error, unable to proceed, please provide a valid method. Options include 'WSM' or 'WPM'.")

  }
  #Internal function for normalization
  norm.WSM <- function(matrix, beneficial.vector) {

    m <- nrow(matrix)
    n <- ncol(matrix)

    result_matrix <- matrix(0, nrow = m, ncol = n)

    for (j in 1:n) {
      if (j %in% beneficial.vector) {

        X_max <- max(matrix[, j])
        denominator <- sum(X_max - matrix[, j])
        result_matrix[, j] <- 1 - (X_max - matrix[, j]) / denominator
      } else {

        X_min <- min(matrix[, j])
        denominator <- sum(matrix[, j] - X_min)
        result_matrix[, j] <- 1 - (matrix[, j] - X_min) / denominator
      }
    }

    return(result_matrix)
  }


  result <- norm.WSM(mat, beneficial.vector)

  if(method=="WSM"){

    preference.scores <- rowSums(t(weights*t(norm.WSM(mat, beneficial.vector))))

  }else{
    preference.scores <- rowProds(t(t(norm.WSM(mat, beneficial.vector))^weights))
  }


  return(preference.scores)
}
