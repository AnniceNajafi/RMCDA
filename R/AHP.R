#' Finding the weights for each criteria given a pairwise comparison matrix A in the AHP method
#'
#' @param A the matrix containing information related to pairwise comparisons of
#' criteria
#'
#' @return a list containing the value of CI/RI and a vector containing the weights
#' of each criteria
#' @import dplyr
find.weight <- function(A){

  if(!is.matrix(A) || nrow(A) == 0 || nrow(A) != ncol(A)){
    stop("The pairwise comparison matrix must be a nonempty square matrix.")
  }

  n <- ncol(A)

  # =Saaty's (1980) random consistency index (RI) for n = 1, ..., 15
  reference.RI <- c(0, 0, 0.58, 0.90, 1.12, 1.24, 1.32, 1.41, 1.45, 1.49,
                    1.51, 1.48, 1.56, 1.57, 1.59)

  if(n > length(reference.RI)){
    stop(sprintf(paste0("The consistency check supports pairwise comparison matrices ",
                        "with at most %d criteria/alternatives (got %d)."),
                 length(reference.RI), n))
  }

  norm.A <- t(t(A) / colSums(A)) #normalize matrix

  W <- rowMeans(norm.A) #find weights

  if(n <= 2){

    CR <- 0 #reciprocal matrices of order 1 or 2 are always consistent (RI = 0)

  }else{

    CI <- (1/n*sum((A %*% W)/W)-n)/(n-1)

    CR <- CI/reference.RI[n]

  }

  if(CR<.1){

    print("No serious inconsistencies detected.")

  }else{

    stop("Inconsistencies detected. Unable to proceed.")

  }

  return(list(CR, W))

}

#' Apply AHP on the matrices
#'
#' @param A the matrix containing information related to pairwise comparisons of
#' criteria
#' @param comparing.competitors the list of matrices related to pairwise comparisons
#' of competitors for each criteria
#'
#' @return a list containing
#' I. The weight of each criteria
#' II. The criteria alternative unweighted matrix
#' III. The weighted scores matrix
#' IV. Competitor final scores
#'
#' @examples
#' data <- read.csv(system.file("extdata", "AHP_input_file.csv", package = "RMCDA"), header=FALSE)
#' mat.lst <- read.csv.AHP.matrices(data)
#' mat.lst[[1]]->A
#' mat.lst[[2]]->comparing.competitors
#' results<- apply.AHP(A, comparing.competitors)
#' @export apply.AHP
apply.AHP <- function(A, comparing.competitors){

  criteria.weight <- find.weight(A)

  res.lst <- list()

  criteria.alternatives.mat <- data.frame()

  for(mat.no in 1:length(comparing.competitors)){

    res.lst[[mat.no]] <-find.weight(comparing.competitors[[mat.no]])

    criteria.alternatives.mat <- rbind(criteria.alternatives.mat, res.lst[[mat.no]][[2]])

  }

  colnames(criteria.alternatives.mat)<- colnames(comparing.competitors[[1]])

  rownames(criteria.alternatives.mat)<- colnames(A)

  weighted.scores.mat <- as.matrix(criteria.alternatives.mat)*criteria.weight[[2]]

  alternative.score <- colSums(criteria.alternatives.mat*criteria.weight[[2]])

  return(list(criteria.weight, criteria.alternatives.mat, weighted.scores.mat, alternative.score))

}


#' Apply Analytical Network Process (ANP) on data
#'
#' @param A the matrix containing information related to pairwise comparisons of
#' criteria
#' @param comparing.competitors the list of matrices related to pairwise comparisons
#' of competitors for each criteria
#' @param power the power value of the supermatrix
#'
#' @return the limiting super matrix
#'
#' @examples
#' data <- read.csv(system.file("extdata", "AHP_input_file.csv", package = "RMCDA"), header=FALSE)
#' mat.lst <- read.csv.AHP.matrices(data)
#' mat.lst[[1]]->A
#' mat.lst[[2]]->comparing.competitors
#' apply.ANP(A, comparing.competitors, 2)
#' @export apply.ANP
apply.ANP <- function(A, comparing.competitors, power){
  
  apply.AHP(A, comparing.competitors)->res.lst
  as.numeric(res.lst[[1]][[2]])->A.weight
  as.matrix(res.lst[[2]])->alternatives.weighted.mat
  
  colnames(A)->crit.names
  if (is.null(crit.names)) paste0("C", seq_len(ncol(A)))->crit.names
  colnames(alternatives.weighted.mat)->alt.names
  if (is.null(alt.names)) paste0("A", seq_len(ncol(alternatives.weighted.mat)))->alt.names
  
  length(A.weight)->n
  length(alt.names)->m
  
  matrix(0, nrow = 1 + n + m, ncol = 1 + n + m)->super.mat
  rownames(super.mat) <- colnames(super.mat) <- c("Goal", crit.names, alt.names)
  
  super.mat[2:(n + 1), 1] <- A.weight
  for (j in seq_len(n)) {
    super.mat[(n + 2):(n + 1 + m), 1 + j] <- as.numeric(alternatives.weighted.mat[j, alt.names])
  }
  super.mat[(n + 2):(n + 1 + m), (n + 2):(n + 1 + m)] <- diag(m)
  
  if (power != 1L) Reduce(`%*%`, rep(list(super.mat), power))->super.mat
  
  return(super.mat)
}



#' Apply fuzzy AHP on criteria comparison matrix
#'
#' @param A the comparison matrix
#'
#' @return the fuzzy weights for each criteria
#'
#' @examples
#' # example code
#' data <- read.csv(system.file("extdata", "AHP_input_file.csv", package = "RMCDA"), header=FALSE)
#' mat.lst <- read.csv.AHP.matrices(data)
#' mat.lst[[1]]->A
#' result <- apply.FAHP(A)
#' @export apply.FAHP
apply.FAHP <- function(A){

  mat.1 <- A; mat.2 <- A; mat.3 <- A

  mat.1[mat.1 > 1 & mat.1 < 9] <- mat.1[mat.1 > 1 & mat.1 < 9] - 1

  mat.1[mat.1 < 1] <- sapply(mat.1[mat.1 < 1], function(x) {

    A <- 1 / x

    if (A >= 2 && A <= 8) {

      return(1 / (A + 1))

    } else {

      return(x)

    }
  })


  mat.3[mat.3 > 1 & mat.3 < 9] <- mat.3[mat.3 > 1 & mat.3 < 9] + 1

  mat.3[mat.3 < 1] <- sapply(mat.3[mat.3 < 1], function(x) {

    A <- 1 / x

    if (A >= 2 && A <= 8) {

      return(1 / (A - 1))

    } else {

      return(x)

    }
  })

  n <- nrow(A)

  r1 <- apply(mat.1, 1, prod)^(1/n);r2 <- apply(mat.2, 1, prod)^(1/n);r3 <- apply(mat.3, 1, prod)^(1/n)

  A_curly <- colSums(t(rbind(r1, r2, r3)))

  A_curly_recip <- rev(A_curly)^-1


  fuzzy.weights.df <- data.frame(first_fuzzy_weight = numeric(), second_fuzzy_weight = numeric(), third_fuzzy_weight = numeric())

  for(i in 1:dim(A)[1]){

    fuzzy.weights.df <- rbind(fuzzy.weights.df, c(r1[i], r2[i], r3[i])*A_curly_recip)

  }
  rownames(fuzzy.weights.df)<-colnames(A)

  crisp.weights <- rowMeans(fuzzy.weights.df)

  return(crisp.weights/sum(crisp.weights))

}
