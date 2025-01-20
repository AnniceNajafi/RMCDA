#' Find entropy of each criteria
#'
#' @param A the matrix A with row names corresponding to alternatives and column
#' names corresponding to criteria
#' @return the entropy value corresponding to each criteria
#'
#' @examples
#' A <- matrix(c(250, 200, 300, 275, 225, 16, 16, 32, 32, 16, 12, 8, 16, 8, 16, 5, 3, 4, 4, 2), nrow=5, ncol=4)
#' colnames(A)<-c("Price", "Storage space", "Camera", "Looks")
#' rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
#' A[,"Price"] <- -A[,"Price"]
#' apply.entropy(A)
#' @export apply.entropy
apply.entropy <- function(A){

  normalized.A <- t(t(A)/colSums(A))

  e <- 1-(-1/log(nrow(A))*colSums(normalized.A*log(normalized.A)))

  entropy <- e/sum(e)

  return(entropy)

}
