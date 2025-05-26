#' Function for applying VIKOR to data
#'
#' @param A the comparison matrix
#' @param weights the weights of criteria
#' @param nu weight of the maximum utility strategy - set by default to 0.5
#'
#' @return a list containing the names of Qi followed by values of Qi, Si, Ri,
#' condition 1, and condition 2.
#'
#' @examples
#' A <- matrix(c(250, 200, 300, 275,
#'  225, 16, 16, 32,
#'   32, 16, 12, 8,
#'    16, 8, 16, 5,
#'     3, 4, 4, 2), nrow=5, ncol=4)
#' colnames(A)<-c("Price", "Memory", "Camera", "Looks")
#' rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
#' A[,"Price"] <- -A[,"Price"]
#' apply.VIKOR(A, c(0.35, 0.3, 0.2, 0.15))
#' @export apply.VIKOR
apply.VIKOR <- function(A, weights, nu = 0.5){

  colMaxs <- apply(A, 2, function(x) max(x, na.rm = TRUE))
  colMins <- apply(A, 2, function(x) min(x, na.rm = TRUE))

  processed.table <- t(apply(A, 1, function(row) {
    -weights * (colMaxs - row) / (colMins - colMaxs)
  }))

  Si <- rowSums(processed.table)
  Ri <- apply(processed.table, 1, function(x) max(x, na.rm = TRUE))

  S.neg <- max(Si); R.neg <- max(Ri)
  S.star <- min(Si); R.star <- min(Ri)

  Qi <- nu * (Si-S.star)/(S.neg-S.star)+(1-nu)*(Ri-R.star)/(R.neg-R.star)

  condition.i <- sort(Qi)[2] - sort(Qi)[1] >= 1/(length(Qi)-1)

  condition.ii <- (names(sort(Qi)[1]) == names(sort(Ri)[1])) || (names(sort(Qi)[1]) == names(sort(Si)[1]))

  return(list(names(sort(Qi)), Qi, Si, Ri, condition.i, condition.ii))

}
