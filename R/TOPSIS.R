#' Apply TOPSIS on matrix A with weight of criteria stored in vector w
#'
#' @param A the matrix A with row names corresponding to alternatives and column
#' names corresponding to criteria
#' @param w the weight vector corresponding to the weight of each criteria
#'
#' @return performance scores obtained through TOPSIS
#'
#' @examples
#' A <- matrix(c(250, 200, 300, 275, 225, 16, 16, 32, 32, 16, 12, 8, 16, 8, 16, 5, 3, 4, 4, 2), nrow=5, ncol=4)
#' colnames(A)<-c("Price", "Storage space", "Camera", "Looks")
#' rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
#' A[,"Price"] <- -A[,"Price"]
#' apply.TOPSIS(A, c(1/4, 1/4, 1/4, 1/4))
#' @export apply.TOPSIS
apply.TOPSIS <- function(A, w){

  if(length(w)!=ncol(A)){

    stop("The dimensions of the weight vector and matrix do not match. Unable to proceed.")
  }

  normalized.A <- t(t(A)/sqrt(colSums(A^2)))
  normalized.A <- normalized.A*w

  S.pos <- sqrt(rowSums((t(t(normalized.A) - apply(normalized.A, 2, max)))^2))
  S.neg <- sqrt(rowSums((t(t(normalized.A) - apply(normalized.A, 2, min)))^2))

  performance.score <- S.neg/( S.pos + S.neg)

  return(performance.score)
}
