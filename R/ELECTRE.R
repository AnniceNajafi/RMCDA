#' Apply ELECTRE I method
#'
#' @param mat A matrix or data frame where rows represent alternatives and columns represent criteria.
#' @param weights A numeric vector of weights for each criterion.
#'
#' @return a list containing three matrices, the first one is the intersection of concordance and discordance
#' matrices, the second one is the concordance matrix, and the third one is the discordance matrix.
#'
#' @examples
#' mat <- matrix(c(25, 10, 30, 20, 30, 10, 15, 20, 30, 30, 30, 10), nrow=3)
#' colnames(mat)<-c("c1", "c2", "c3", "c4")
#' rownames(mat)<-c("a1", "a2", "a3")
#' weights <- c(0.2, 0.15, 0.4, 0.25)
#'
#' # Apply ELECTRE I method
#' results <- apply.ELECTRE1(mat, weights)
#'
#' @export apply.ELECTRE1
apply.ELECTRE1 <- function(mat, weights) {


  norm.mat <- t(t(mat/sqrt(colSums(mat^2))) * weights)


  concordance.mat <- matrix(NA, nrow=nrow(norm.mat), ncol=nrow(norm.mat))

  for(i in 1:nrow(norm.mat)){

    for(j in 1:nrow(norm.mat)){

      concordance.mat[i,j] <- sum(weights[norm.mat[i,]>norm.mat[j,]])

    }

  }

  C.bar <- sum(colSums(concordance.mat))/(length(concordance.mat)-sqrt(length(concordance.mat)))



  discordance.mat <- matrix(NA, nrow=nrow(norm.mat), ncol=nrow(norm.mat))

  for(i in 1:nrow(norm.mat)){

    for(j in 1:nrow(norm.mat)){

      if(i == j){

        discordance.mat[i,j] <- 0

      }else{

        discordance.mat[i,j] <- abs(min(norm.mat[i,] - norm.mat[j,]))/max(abs(norm.mat[i,] - norm.mat[j,]))


      }

    }

  }


  D.bar <- sum(colSums(discordance.mat))/(length(discordance.mat)-sqrt(length(discordance.mat)))

  concordance.mat>C.bar & discordance.mat>D.bar

  return(list((concordance.mat>C.bar & discordance.mat>D.bar), concordance.mat, discordance.mat))


}
