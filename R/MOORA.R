#' Apply Multi-Objective Optimization on the basis of Ratio Analysis (MOORA)
#'
#' @param mat is a matrix and contains the values for different properties
#' of different alternatives
#' @param weights are the weights of each property in the decision making process
#' @param beneficial.vector is a vector that contains the column number of beneficial
#' properties.
#'
#' @return a vector containing the calculated quantitative utility
#'
#'
#' @examples
#' mat <- matrix(c(60, 6.35, 6.8, 10, 2.5, 4.5, 3,
#' 0.4, 0.15, 0.1, 0.2, 0.1, 0.08, 0.1,
#' 2540, 1016, 1727.2, 1000, 560, 1016, 177,
#' 500, 3000, 1500, 2000, 500, 350, 1000,
#' 990, 1041, 1676, 965, 915, 508, 920), nrow=7)
#' colnames(mat)<-c("Load capacity", "Repeatability", "Maximum tip speed",
#' "Memory capacity", "Manipulator reach")
#' rownames(mat)<-paste0("A", 1:7)
#' weights <- c(0.1574, 0.1825, 0.2385, 0.2172, 0.2043)
#' beneficial.vector <- c(1, 3, 4, 5)
#' apply.MOORA(mat, weights, beneficial.vector)
#' @export apply.MOORA
apply.MOORA <- function(mat, weights, beneficial.vector){

  weighted.normalized.mat <- t(weights * (t(mat)/(sqrt(colSums(mat^2)))))


  if(length(beneficial.vector)>1){
    A <- rowSums(weighted.normalized.mat[,beneficial.vector])
  }else{
    A <- (weighted.normalized.mat[,beneficial.vector])
  }

  if((ncol(mat)-length(beneficial.vector))>1){
    B <- rowSums(weighted.normalized.mat[,-beneficial.vector])
  }else{
    B <- (weighted.normalized.mat[,-beneficial.vector])
  }

  result <- A - B

  return(result)


}
