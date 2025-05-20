#' Weighted Aggregated Sum Product Assessment (WASPAS)
#'
#' @param mat is a matrix and contains the values for different properties
#' of different alternatives
#' @param weights are the weights of each property in the decision making process
#' @param beneficial.vector is a vector that contains the column number of beneficial
#' properties
#' @param lambda a value between 0 and 1, used in the calculation of the W index
#'
#' @return the Q index from WASPAS
#'
#'
#' @examples
#' mat <- matrix(c(0.04, 0.11, 0.05, 0.02, 0.08, 0.05, 0.03, 0.1, 0.03,
#' 1.137, 0.854, 1.07, 0.524, 0.596, 0.722, 0.521, 0.418, 0.62,
#' 960, 1920, 3200, 1280, 2400, 1920, 1600, 1440, 2560), nrow=9)
#' colnames(mat)<-c("Dimensional Deviation (DD)", "Surface Roughness (SR)",
#' "Material Removal Rate (MRR)")
#'
#' rownames(mat)<-paste0("A", 1:9)
#' beneficial.vector <- c(3)
#' weights <- c(0.1047, 0.2583, 0.6369)
#' apply.WASPAS(mat, weights, beneficial.vector, 0.5)
#' @importFrom matrixStats rowProds
#' @export apply.WASPAS
apply.WASPAS <- function(mat, weights, beneficial.vector, lambda){

  mat->weighted.mat

  weighted.mat[,beneficial.vector] <- t(mat[, beneficial.vector]/t(apply(mat[, beneficial.vector, drop = FALSE], 2, max)))
  weighted.mat[,-beneficial.vector] <- t(apply(mat[, -beneficial.vector, drop = FALSE], 2, min)/t(mat[,-beneficial.vector]))


  new.weighted.mat <- t(weights*t(weighted.mat))

  Q1 <- rowSums(new.weighted.mat)


  edited.mat <- as.data.frame(t(t(weighted.mat)^weights))


  Q2 <- transform(edited.mat, prod=rowProds(as.matrix(edited.mat)))$prod

  Qi <- lambda*(Q1)+(1-lambda)*(Q2)

  return(Qi)
}
