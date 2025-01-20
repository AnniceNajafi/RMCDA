#' Apply Combinative Distance-based Assessment (CODAS)
#'
#' @param mat is a matrix and contains the values for different properties
#' of different alternatives
#' @param weights are the weights of each property in the decision making process
#' @param beneficial.vector is a vector that contains the column number of beneficial
#' properties.
#' @param psi threshold parameter
#'
#' @return a vector containing the calculated quantitative utility
#'
#'
#' @examples
#'
#' mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
#' 420, 91, 1365, 1120, 875, 1190, 200,
#' 74.2, 70, 189, 210, 112, 217, 112,
#' 2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
#' 21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
#' 0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
#' 0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)
#' colnames(mat)<-c("Toughness Index",	"Yield Strength",	"Young's Modulus",
#'                  "Density",	"Thermal Expansion",	"Thermal Conductivity",	"Specific Heat")
#'rownames(mat)<-c("AI 2024-T6", "AI 5052-O","SS 301 FH",
#'"SS 310-3AH",
#'"Ti-6AI-4V",
#'"Inconel 718",
#'"70Cu-30Zn")
#'weights <- c(0.28, 0.14, 0.05, 0.24, 0.19, 0.05, 0.05)
#'beneficial.vector<-c(1,2,3)
#'psi <- 0.02
#'apply.CODAS(mat, weights, beneficial.vector, psi)
#' @export apply.CODAS
apply.CODAS <- function(mat, weights, beneficial.vector, psi){

  max.min.vector <- c()

  for(col in 1:ncol(mat)){
    if(col %in% beneficial.vector){
      max.min.vector <- c(max.min.vector, max(mat[,col]))
    }else{
      max.min.vector <- c(max.min.vector, min(mat[,col]))
    }
  }


  normalized.mat.beneficial <- t(t(mat[,beneficial.vector])/max.min.vector[beneficial.vector])

  normalized.mat.non.beneficial <- t(max.min.vector[-beneficial.vector]/t(mat[,-beneficial.vector]))



  normalized.mat <- matrix(NA, nrow=nrow(mat), ncol=ncol(mat))

  rownames(normalized.mat) <- rownames(mat)

  colnames(normalized.mat) <- colnames(mat)


  normalized.mat[,beneficial.vector] <- normalized.mat.beneficial

  normalized.mat[,-beneficial.vector] <- normalized.mat.non.beneficial



  weighted.normalized.mat <- t(weights*t(normalized.mat))

  negative.ideal <- apply(weighted.normalized.mat, 2, min)


  E_i <- sqrt(rowSums((sweep(weighted.normalized.mat, 2, negative.ideal, FUN = "-"))^2 ))


  T_i <- rowSums(
    abs(sweep(weighted.normalized.mat , 2, negative.ideal, FUN = "-"))
  )

  h_ik <- matrix(NA, nrow=length(E_i), ncol=length(E_i))

  for(i in 1:length(E_i)){
    for(k in 1:length(E_i)){

      h_ik[i,k] <-E_i[i]-E_i[k]+(psi*(E_i[i]-E_i[k])*(T_i[i]-T_i[k]))

    }
  }

  assessment.score <- rowSums(h_ik)

  return(assessment.score)


}
