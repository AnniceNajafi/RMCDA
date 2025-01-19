#' Apply Simple Additive Weighting Method (SAW)
#'
#' @param mat is a matrix and contains the values for different properties
#' of different alternatives
#' @param weights are the weights of each property in the decision making process
#' @param beneficial.vector is a vector that contains the column number of beneficial
#' properties.
#'
#' @return a vector containing the score and corresponding ranking for the SAW function
#' @export
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
#' apply.SAW(mat, weights, beneficial.vector)
apply.SAW <- function(mat, weights, beneficial.vector){

  #Internal function to take the max if beneficial criteria or min if non-beneficial
  #for the denominator
  maxOrMinMatrix <- function(mat, weights, beneficial.vector) {

    if (!is.matrix(mat)) {
      stop("'mat' must be a matrix.")
    }


    results <- sapply(seq_len(ncol(mat)), function(col_idx) {
      column_data <- mat[, col_idx]

      if (col_idx %in% beneficial.vector) {

        max(column_data, na.rm = TRUE)
      } else {

        min(column_data, na.rm = TRUE)
      }
    })


    if (!is.null(colnames(mat))) {
      names(results) <- colnames(mat)
    }

    return(results)
  }


  hold.vals <- maxOrMinMatrix(mat, weights, beneficial.vector)
  edited.mat <- mat

  for(i in 1:nrow(edited.mat)){
    for(j in 1:ncol(edited.mat)){

      if(j %in% beneficial.vector){
        edited.mat[i,j] <- edited.mat[i,j]/hold.vals[j]
      }else{
        edited.mat[i,j] <- hold.vals[j]/edited.mat[i,j]
      }

    }
  }



  A.i <- (edited.mat)%*%weights


  ranking <- nrow(mat) - rank(A.i) + 1



  return(list(A.i, ranking))

}
