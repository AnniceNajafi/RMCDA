#' Read csv file containing pairwise comparison matrices
#'
#' @param data the matrix containing information related to pairwise comparisons of
#' criteria
#'
#' @return a list containing a matrix A related to pairwise comparison of criteria
#' and a list containing multiple matrices related to pairwise comparisons of different
#' competitor products
#' @export
read.matrices <- function(data){

  data.dim <- length(data)-1

  mat.data <- as.matrix(data[1:(data.dim+1), 1:(data.dim+1)][-1, -1])
  mat.data <- apply(mat.data, 2, as.numeric)

  data[1,-1]->colnames(mat.data)
  colnames(mat.data)->rownames(mat.data)

  mat.data->A

  comparing.competitors<-list() #extract and store a list of matrices, each related to
  #the comparison of alternatives based on each criteria

  for(i in 1:data.dim){

    start.idx <- (i)*(data.dim+2)+2
    end.idx <- start.idx + data.dim-1

    tmp.mat <- as.matrix(data[(start.idx):end.idx, 2:(data.dim+1)])

    tmp.mat <- apply(tmp.mat, 2, as.numeric)

    colnames(tmp.mat)<-data[(start.idx):end.idx, 1]
    rownames(tmp.mat)<-colnames(tmp.mat)

    comparing.competitors[[i]]<-tmp.mat
  }

  return(list(A, comparing.competitors))

}



#' Finding the weights for each criteria given a pairwise comparison matrix A in the AHP method
#'
#' @param A the matrix containing information related to pairwise comparisons of
#' criteria
#'
#' @return a list containing the value of CI/RI and a vector containing the weights
#' of each criteria
#' @export
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


#' Apply CRITIC on comparison matrix
#'
#' @param A the matrix A with row names corresponding to alternatives and column
#' names corresponding to criteria
#' @return the weight percentages related to matrix A obtained through the CRITIC method
#' @export
apply.CRITIC <- function(A){


  normalized.A <- apply(A, 2, function(x) (x - min(x))/(max(x)-min(x)))

  sigma.A <- apply(normalized.A, 2, sd)

  corr.A <- cor(normalized.A)

  measure.conflict <- rowSums(1-corr.A)

  weight.percentage <- measure.conflict*sigma.A/sum(measure.conflict*sigma.A)

  return(weight.percentage)

}


#' Find entropy of each criteria
#'
#' @param A the matrix A with row names corresponding to alternatives and column
#' names corresponding to criteria
#' @return the entropy value corresponding to each criteria
#' @export
find.entropy <- function(A){

  normalized.A <- t(t(A)/colSums(A))

  e <- 1-(-1/log(nrow(A))*colSums(normalized.A*log(normalized.A)))

  entropy <- e/sum(e)

  return(entropy)

}



#' Apply TOPSIS on matrix A with weight of criteria stored in vector w
#'
#' @param A the matrix A with row names corresponding to alternatives and column
#' names corresponding to criteria
#' @param w the weight matrix corresponding to the weight of each criteria
#'
#' @return performance scores obtained through TOPSIS
#' @export
apply.TOPSIS <- function(A, w){

  if(length(w)!=ncol(A)){

    stop("The dimensions of the weight vector and matrix do not match. Unable to proceed.")
  }

  normalized.A <- t(t(A)/sqrt(colSums(A^2)))
  normalized.A <- normalized.A*w

  S.pos <- sqrt(rowSums((t(t(normalized.A) - apply(normalized.A, 2, max)))^2))
  S.neg <- sqrt(rowSums((t(t(normalized.A) - apply(normalized.A, 2, min)))^2))

  performance.score <- S.neg/( S.pos + S.neg)

  return(performance.score)
}













