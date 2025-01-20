#' Apply Additive Ratio Assessment (ARAS)
#'
#' @param mat is a matrix and contains the values for different properties
#' of different alternatives
#' @param weights are the weights of each property in the decision making process
#' @param beneficial.vector is a vector that contains the column number of beneficial
#' properties.
#' @return a vector containing the utility degree related to each alternative,
#' higher utility indicates better ranking.
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
#'apply.ARAS(mat, weights, beneficial.vector)
#' @export apply.ARAS
apply.ARAS <- function(mat, weights, beneficial.vector){

  min.vector <- apply(mat, 2, min)
  max.vector <- apply(mat, 2, max)

  optimal.value <- c()

  for(i in 1:ncol(mat)){

    if(i %in% beneficial.vector){

      optimal.value <- c(optimal.value, max.vector[i])

    }else{
      optimal.value <- c(optimal.value, min.vector[i])
    }

  }


  edited.mat <- matrix(NA, nrow=nrow(mat)+1, ncol=ncol(mat))



  for(i in 1:(nrow(mat)+1)){
    for(j in 1:ncol(mat)){

      if(j %in% beneficial.vector){
        if(i==1){
          edited.mat[i,j]<-optimal.value[j]/(sum(mat[,j])+optimal.value[j])
        }else{
          edited.mat[i,j]<-mat[i-1,j]/(sum(mat[,j])+optimal.value[j])
        }

      }else{

        if(i==1){
          edited.mat[i,j]<-1/optimal.value[j]
        }else{
          edited.mat[i,j] <- 1/mat[i-1,j]
        }

      }
    }
  }


  edited.mat[,-beneficial.vector] <- t(t(edited.mat[,-beneficial.vector])/colSums(edited.mat[,-beneficial.vector]))


  edited.mat <- edited.mat*weights

  Si <- rowSums(edited.mat)


  S0 <- Si[1]

  Ki <- Si/S0

  return(Ki)


}
