#' Finding the weights for each criteria given a pairwise comparison matrix A in the AHP method
#'
#' @param A the matrix containing information related to pairwise comparisons of
#' criteria
#'
#' @return a list containing the value of CI/RI and a vector containing the weights
#' of each criteria
#' @import dplyr
find.weight <- function(A){

  norm.A <- t(t(A) / colSums(A)) #normalize matrix

  W <- rowMeans(norm.A) #find weights

  CI <- (1/ncol(A)*sum((A %*% W)/W)-ncol(A))/(ncol(A)-1)

  reference.RI <- data.frame(n=seq(2, 10, 1), RI=c(0, .58, .9, 1.12, 1.24, 1.32, 1.41, 1.45, 1.51))

  RI <- (reference.RI %>% filter(n==ncol(A)))$RI


  if(CI/RI<.1){

    print("No serious inconsistencies detected.")

  }else{

    stop("Inconsistencies detected. Unable to proceed.")

  }

  return(list(CI/RI, W))

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
#' @export
#' @examples
#' data <- read.csv(system.file("extdata", "AHP_input_file.csv", package = "RMCDA"), header=FALSE)
#' mat.lst <- read.csv.AHP.matrices(data)
#' mat.lst[[1]]->A
#' mat.lst[[2]]->comparing.competitors
#' results<- apply.AHP(A, comparing.competitors)
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

  weighted.scores.mat <- t(t(criteria.alternatives.mat)*criteria.weight[[2]])

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
#' @export
#' @examples
#' data <- read.csv(system.file("extdata", "AHP_input_file.csv", package = "RMCDA"), header=FALSE)
#' mat.lst <- read.csv.AHP.matrices(data)
#' mat.lst[[1]]->A
#' mat.lst[[2]]->comparing.competitors
#' apply.ANP(A, comparing.competitors, 2)
apply.ANP <- function(A, comparing.competitors, power){

  apply.AHP(A, comparing.competitors)->res.lst #apply AHP

  res.lst[[1]][[2]]->A.weight

  res.lst[[3]]->alternatives.weighted.mat

  super.mat <- matrix(NA, nrow=2*dim(A)[1]+1, ncol=2*dim(A)[1]+1)

  super.mat[2:(dim(A)[1]+1)]<-A.weight

  super.mat[(dim(A)[1]+2):(dim(A)[1]*2+1), 2:(dim(A)[1]+1)]<-alternatives.weighted.mat

  super.mat[(dim(A)[1]+2):(dim(A)[1]*2+1), (dim(A)[1]+2): (2*dim(A)[1]+1)]<-diag(dim(A)[1])

  super.mat[is.na(super.mat)]<-0

  super.mat^power->super.mat

  return(super.mat)
}


#' Apply fuzzy AHP on criteria comparison matrix
#'
#' @param A the comparison matrix
#'
#' @return the fuzzy weights for each criteria
#' @export
#' @examples
#' # example code
#' data <- read.csv(system.file("extdata", "AHP_input_file.csv", package = "RMCDA"), header=FALSE)
#' mat.lst <- read.csv.AHP.matrices(data)
#' mat.lst[[1]]->A
#' result <- apply.FAHP(A)
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

  r1 <- apply(mat.1, 1, prod)^(1/4);r2 <- apply(mat.2, 1, prod)^(1/4);r3 <- apply(mat.3, 1, prod)^(1/4)

  A_curly <- colSums(t(rbind(r1, r2, r3)))

  A_curly_recip <- rev(A_curly)^-1


  fuzzy.weights.df <- data.frame(first_fuzzy_weight = numeric(), second_fuzzy_weight = numeric(), third_fuzzy_weight = numeric())

  for(i in 1:dim(A)[1]){

    fuzzy.weights.df <- rbind(fuzzy.weights.df, c(r1[i], r2[i], r3[i])*A_curly_recip)

  }
  rownames(fuzzy.weights.df)<-colnames(A)

  return(rowMeans(fuzzy.weights.df))

}
