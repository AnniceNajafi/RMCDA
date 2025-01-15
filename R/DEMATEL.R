#' Apply DEMATEL method
#'
#' @param comparisons.mat the matrix containing information related to pairwise comparisons of
#' criteria
#'
#' @return a list containing two vectors one holding D-R and the other D+R
#' @export
#'
#' @examples
#' comparisons.mat <- matrix(c(0, 3, 3, 4,
#' 1, 0, 2, 1,
#' 1, 2, 0, 2,
#' 1, 2, 1, 0), nrow=4)
#' rownames(comparisons.mat)<-c("Price/cost", "Storage Space", "Camera", "Processor")
#' colnames(comparisons.mat)<-c("Price/cost", "Storage Space", "Camera", "Processor")
#' apply.DEMATEL(comparisons.mat)
apply.DEMATEL <- function(comparisons.mat){

  X <- comparisons.mat/max(rowSums(comparisons.mat))


  D <- rowSums(X %*% inv(diag(dim(X)[1])-X))

  R <- colSums(X %*% inv(diag(dim(X)[1])-X))

  D.minus.R <- D - R

  D.plus.R <- D + R

  return(list(D.minus.R, D.plus.R))
}
