#' Apply COmbined COmpromise SOlution (COCOSO)
#'
#' @param mat is a matrix and contains the values for different properties
#' of different alternatives
#' @param weights are the weights of each property in the decision making process
#' @param beneficial.vector is a vector that contains the column number of beneficial
#' properties.
#' @return a vector containing the aggregated appraisal scores
#' @export
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
#'apply.COCOSO(mat, weights, beneficial.vector)
apply.COCOSO <- function(mat, weights, beneficial.vector){

  min.vector <- apply(mat, 2, min)
  max.vector <- apply(mat, 2, max)


  t(t(mat - min.vector)/(max.vector-min.vector))

  normalized.mat <- matrix(NA, nrow=nrow(mat), ncol=ncol(mat))

  for(i in 1:nrow(mat)){

    for(j in 1:ncol(mat)){

      if(j %in% beneficial.vector){

        normalized.mat[i,j] <- (mat[i,j]-min.vector[j])/(max.vector[j]-min.vector[j])

      }else{

        normalized.mat[i,j] <- (max.vector[j]-mat[i,j])/(max.vector[j]-min.vector[j])

      }

    }
  }

  weighted.normalized.mat <- t(weights*t(normalized.mat))


  powered.mat <- t(t(normalized.mat)^weights)


  S_i <- rowSums(weighted.normalized.mat)
  P_i <- rowSums(powered.mat)


  K.a <- (P_i+S_i)/sum(P_i+S_i)

  K.b <- S_i/min(S_i) + P_i/min(P_i)

  K.c <- ((0.5*S_i)+(0.5*P_i))/((0.5*max(S_i))+(0.5*max(P_i)))



  aggregated.appraisal.scores <- (K.a*K.b*K.c)^1/3+1/3*(K.a+K.b+K.c)


  return(aggregated.appraisal.scores)

}
