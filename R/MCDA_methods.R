#' Read csv file containing pairwise comparison matrices for applying AHP or ANP
#'
#' @param data the matrix containing information related to pairwise comparisons of
#' criteria
#'
#' @return a list containing a matrix A related to pairwise comparison of criteria
#' and a list containing multiple matrices related to pairwise comparisons of different
#' competitor products
#' @export
#' @examples
#' data <- read.csv(system.file("extdata", "AHP_input_file.csv", package = "RMCDA"), header=FALSE)
#' mat.lst <- read.csv.AHP.matrices(data)
read.csv.AHP.matrices <- function(data){

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


#' Read csv file containing pairwise comparison matrices for applying SMCDM
#'
#' @param data the matrix containing information related to pairwise comparisons of
#' criteria
#'
#' @return a list containing a matrix A related to pairwise comparison of criteria
#' and a list containing multiple matrices related to pairwise comparisons of different
#' competitor products
#' @export
#' @examples
#' data <- read.csv(system.file("extdata", "SMCDM_input.csv", package = "RMCDA"), header = FALSE)
#' mat.lst <- read.csv.SMCDM.matrices(data)
read.csv.SMCDM.matrices <- function(data){

  empty.idx <- which(apply(data, 1, function(row) all(row == "" | is.na(row)))==1)

  empty.col.idx.comp.mat <- which(apply(data[1:(empty.idx[1]-1),], 2, function(col) all(col == "" | is.na(col)))==1)

  comparison.mat <- data[2:(empty.idx[1]-1), 2:(empty.col.idx.comp.mat-1)]

  colnames(comparison.mat)<-data[1, 2:(empty.col.idx.comp.mat-1)]
  rownames(comparison.mat)<-data[2:(empty.idx[1]-1), 1]

  state.criteria.probs <- data[(empty.idx[1]+1):(empty.idx[2]-1), ]
  state.criteria.probs[1,2:ncol(state.criteria.probs)] -> colnames.state.criteria.probs
  state.criteria.probs[2:nrow(state.criteria.probs), 1] -> rownames.state.criteria.probs

  state.criteria.probs <- state.criteria.probs[2:nrow(state.criteria.probs), 2:ncol(state.criteria.probs)]
  colnames(state.criteria.probs)<-colnames.state.criteria.probs
  rownames(state.criteria.probs)<-rownames.state.criteria.probs

  as.numeric(data[(empty.idx[2]+2):nrow(data),1])->likelihood.vector

  state.criteria.probs.df <- as.data.frame(sapply(state.criteria.probs, as.numeric))

  rownames(state.criteria.probs)->rownames(state.criteria.probs.df)

  state.criteria.probs.df -> state.criteria.probs

  comparison.mat.df <- as.data.frame(sapply(comparison.mat, as.numeric))

  rownames(comparison.mat)->rownames(comparison.mat.df)

  comparison.mat.df -> comparison.mat

  return(list(comparison.mat, state.criteria.probs, likelihood.vector))
}

#' Read csv file containing input to the stratified BWM method
#'
#' @param data input of the csv file
#'
#' @return the inputs to the SBWM method
#' @export
#' @examples
#' data <- read.csv(system.file("extdata", "stratified_BWM_case_study_I_example.csv", package = "RMCDA"), header = FALSE)
#' mat.lst <- read.csv.SBWM.matrices(data)
read.csv.SBWM.matrices <- function(data){

  length(data) -> data.dim

  empty.idx <- which(apply(data, 1, function(row) all(row == "" | is.na(row)))==1)

  empty.col.idx <- which(apply(data[1:(empty.idx[1]-1),], 2, function(col) all(col == "" | is.na(col)))==1)

  comparison.mat <- data[2:(empty.idx[1]-1), 1:(empty.col.idx[1]-1)]

  data[1,1:(empty.col.idx[1]-1)]->colnames(comparison.mat)
  comparison.mat[,1] -> rownames(comparison.mat)
  comparison.mat[,1]<-NULL

  others.to.worst <- data[(empty.idx[1]+2):(empty.idx[2]-1), ]
  others.to.best <- data[(empty.idx[2]+2):(empty.idx[3]-1), ]

  others.to.best[,-1]->others.to.best
  others.to.worst[,-1]->others.to.worst

  colnames(others.to.worst) <- data[empty.idx[1]+1, 2:length(data)]
  colnames(others.to.best) <- data[empty.idx[1]+1, 2:length(data)]

  rownames(others.to.worst) <- data[(empty.idx[1]+2):(empty.idx[2]-1), 1]
  rownames(others.to.best) <- data[(empty.idx[1]+2):(empty.idx[2]-1), 1]

  state.worst.lst <- data[(empty.idx[3]+1):(empty.idx[4]-1), 2:length(data)]

  state.best.lst <- data[(empty.idx[4]+1):(empty.idx[5]-1), 2:length(data)]

  likelihood.vector <- data[(empty.idx[5]+1),2:(which(data[(empty.idx[5]+1),]=="")[1]-1)]

  as.character(state.best.lst)->state.best.lst
  as.character(state.worst.lst)->state.worst.lst
  as.numeric(likelihood.vector)->likelihood.vector

  return(list(comparison.mat, others.to.worst, others.to.best, state.worst.lst, state.best.lst, likelihood.vector))

}

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


#' Apply CRITIC on comparison matrix
#'
#' @param A the matrix A with row names corresponding to alternatives and column
#' names corresponding to criteria
#' @return the weight percentages related to matrix A obtained through the CRITIC method
#' @export
#' @examples
#' A <- matrix(c(250, 200, 300, 275, 225, 16, 16, 32, 32, 16, 12, 8, 16, 8, 16, 5, 3, 4, 4, 2), nrow=5, ncol=4)
#' colnames(A)<-c("Price", "Storage space", "Camera", "Looks")
#' rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
#' A[,"Price"] <- -A[,"Price"]
#' apply.CRITIC(A)
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
#' @examples
#' A <- matrix(c(250, 200, 300, 275, 225, 16, 16, 32, 32, 16, 12, 8, 16, 8, 16, 5, 3, 4, 4, 2), nrow=5, ncol=4)
#' colnames(A)<-c("Price", "Storage space", "Camera", "Looks")
#' rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
#' A[,"Price"] <- -A[,"Price"]
#' find.entropy (A)
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
#' @param w the weight vector corresponding to the weight of each criteria
#'
#' @return performance scores obtained through TOPSIS
#' @export
#' @examples
#' A <- matrix(c(250, 200, 300, 275, 225, 16, 16, 32, 32, 16, 12, 8, 16, 8, 16, 5, 3, 4, 4, 2), nrow=5, ncol=4)
#' colnames(A)<-c("Price", "Storage space", "Camera", "Looks")
#' rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
#' A[,"Price"] <- -A[,"Price"]
#' apply.TOPSIS(A, c(1/4, 1/4, 1/4, 1/4))
#'
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


#' Function for applying VIKOR to data
#'
#' @param A the comparison matrix
#' @param weights the weights of criteria
#' @param nu weight of the maximum utility strategy - set by default to 0.5
#'
#' @return
#' @export
#'
#' @examples
#' A <- matrix(c(250, 200, 300, 275, 225, 16, 16, 32, 32, 16, 12, 8, 16, 8, 16, 5, 3, 4, 4, 2), nrow=5, ncol=4)
#' colnames(A)<-c("Price", "Memory", "Camera", "Looks")
#' rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
#' A[,"Price"] <- -A[,"Price"]
#' apply.VIKOR(A, c(0.35, 0.3, 0.2, 0.15))
apply.VIKOR <- function(A, weights, nu = 0.5){

  colMaxs <- apply(A, 2, function(x) max(x, na.rm = TRUE))
  colMins <- apply(A, 2, function(x) min(x, na.rm = TRUE))

  processed.table <- t(apply(A, 1, function(row) {
    -weights * (colMaxs - row) / (colMins - colMaxs)
  }))

  Si <- rowSums(processed.table)
  Ri <- apply(processed.table, 1, function(x) max(x, na.rm = TRUE))

  S.neg <- max(Si); R.neg <- max(Ri)
  S.star <- min(Si); R.star <- min(Ri)

  Qi <- nu * (Si-S.star)/(S.neg-S.star)+(1-nu)*(Ri-R.star)/(R.neg-R.star)

  condition.i <- sort(Qi)[2] - sort(Qi)[1] >= 1/(length(Qi)-1)

  condition.ii <- (names(sort(Qi)[1]) == names(sort(Ri)[1])) || (names(sort(Qi)[1]) == names(sort(Si)[1]))

  return(list(names(sort(Qi)), Qi, Si, Ri, condition.i, condition.ii))

}

#' Function for applying PROMOTHEE I or II
#'
#' @param A the comparison matrix with the row names indicating the alternatives and colnames
#' indicating the criteria.
#' @param weights the weights of criteria.
#'
#' @return the results of PROMOTHEE
#' @export
#'
#' @examples
#' A <- matrix(c(250, 200, 300, 275, 16, 16, 32, 32, 12, 8, 16, 8, 5, 3, 4, 2), nrow=4)
#' rownames(A)<-c("Mobile 1", "Mobile 2", "Mobile 3", "Mobile 4")
#' colnames(A)<-c("Price", "Memory", "Camera", "Looks")
#' weights <- c(0.35, 0.25, 0.25, 0.15)
#' apply.PROMOTHEE(A, weights)
apply.PROMOTHEE <- function(A, weights, type="II"){

  colMaxs <- apply(A, 2, function(x) max(x, na.rm = TRUE))
  colMins <- apply(A, 2, function(x) min(x, na.rm = TRUE))

  processed.A <- t(apply(A, 1, function(row) {
    (row - colMins) / (colMaxs - colMins)
  }))



  pairwise.diff.all <- function(mat.data) {
    n <- nrow(mat.data)
    result <- list()

    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {

          diff <- mat.data[i, ] - mat.data[j, ]
          result[[paste("D(M", i, "-M", j, ")", sep = "")]] <- diff
        }
      }
    }


    return(do.call(rbind, result))
  }

  pairwise.diffs <- pairwise.diff.all(processed.A)

  pairwise.diffs[pairwise.diffs<0]<-0

  processed.A <- sweep(pairwise.diffs, 2, weights, `*`)

  pairwise.vector <- rowSums(processed.A)/sum(weights)


  row.names <- unique(unlist(lapply(names(pairwise.vector), function(x) strsplit(x, "-")[[1]])))
  row.names <- gsub("D\\(|\\)", "", row.names)


  n <- length(row.names)
  preference.matrix <- matrix(NA, nrow = n, ncol = n, dimnames = list(row.names, row.names))


  for (pair in names(pairwise.vector)) {

    elements <- unlist(strsplit(gsub("D\\(|\\)", "", pair), "-"))
    row_name <- elements[1]
    col_name <- elements[2]


    preference.matrix[row_name, col_name] <- pairwise.vector[pair]
  }


  diag(preference.matrix) <- "-"

  preference.matrix <- preference.matrix[1:nrow(A), 1:ncol(A)]

  preference.matrix[preference.matrix == "-"] <- NA

  numeric.matrix <- matrix(as.numeric(preference.matrix), nrow = nrow(preference.matrix), ncol = ncol(preference.matrix))

  if(type=="II"){

    leaving.flow <- rowMeans(numeric.matrix, na.rm = TRUE)

    entering.flow <- colMeans(numeric.matrix, na.rm = TRUE)

    net.out.ranking <- leaving.flow  - entering.flow

    return(list(leaving.flow, entering.flow, net.out.ranking, order(net.out.ranking)))

  }else if(type=="I"){

    leaving.flow <- rowSums(numeric.matrix, na.rm = TRUE)

    entering.flow <- colSums(numeric.matrix, na.rm = TRUE)

    net.out.ranking <- leaving.flow  - entering.flow

    return(list(leaving.flow, entering.flow, net.out.ranking))

  }else{

    stop("Inavlid type. Unable to proceed. Input should be either type I or type II.")

  }

}



#' Apply SRMP (Simple Ranking Method using Reference Profiles) on data
#'
#' @param evaluations.mat the matrix comparing alternatives based on criteria
#' @param reference.profiles matrix containing reference profile information
#' @param weights of different criteria
#'
#' @return alternatives ranked using SRMP
#' @export
#'
#' @examples
#' evaluations.mat <- matrix(c(41, 46, 43, -2, -4, -5.5, 4, 2, 3), nrow=3)
#' colnames(evaluations.mat) <- c("S", "L", "J")
#' rownames(evaluations.mat) <- c("x", "y", "z")
#' reference.profiles <- matrix(c(42, 45, -5, -3, 2, 4), nrow=2)
#' colnames(reference.profiles) <- c("S", "L", "J")
#' rownames(reference.profiles) <- c("p1", "p2")
#' weights <- c(1/3, 1/3, 1/3)
#' apply.SRMP(evaluations.mat, reference.profiles, weights)
apply.SRMP <- function(evaluations.mat, reference.profiles, weights) {

  comparisons <- c()

  #Initial comparison
  for (i in 1:nrow(evaluations.mat)) {
    comparisons <- c(comparisons, sum((evaluations.mat[i, ] >= reference.profiles[1, ]) * weights))
  }

  #Repeat until no duplicates are found
  ref.idx <- 2

  while (sum(duplicated(comparisons)) > 0) {
    duplicated_indices <- which(duplicated(comparisons) | duplicated(comparisons, fromLast = TRUE))
    comparisons.2 <- comparisons

    for (i in duplicated_indices) {
      #Recalculate using the next reference profile
      comparisons.2[i] <- sum((evaluations.mat[i, ] >= reference.profiles[ref.idx, ]) * weights)
    }

    comparisons[duplicated_indices] <- comparisons.2[duplicated_indices]
    ref.idx <- ref.idx +1
  }

  names(comparisons)<-rownames(evaluations.mat)

  return(comparisons)
}


#' Apply stratified multi-criteria decision making method
#'
#' @param comparison.mat the matrix containing alternatives as row names and criteria
#' as column names and corresponding scores as cell values.
#' @param state.criteria.probs the matrix containing the states as column names and
#' criteria as row names and the corresponding scores as matrix values.
#' @param likelihood.vector the vector containing the likelihood of being in each state.
#' @param independent.event this parameter is set to TRUE by default which indicates only the
#' probability of the occurence of each event is required (strati I and II). If set to FALSE
#' then the user should provide the probabilities of occurrence of all states.
#'
#' @return the SMCDM results
#' @export
#'
#' @examples
#' data <- read.csv(system.file("extdata", "SMCDM_input.csv", package = "RMCDA"), header=FALSE)
#' mat.lst <- read.csv.SMCDM.matrices(data)
#' comparison.mat <- mat.lst[[1]]
#' state.criteria.probs <- mat.lst[[2]]
#' likelihood.vector <- mat.lst[[3]]
#' apply.SMCDM(comparison.mat, state.criteria.probs, likelihood.vector)
apply.SMCDM <- function(comparison.mat, state.criteria.probs, likelihood.vector, independent.events = TRUE){

  if(independent.events == TRUE){

    extended.states <- t(apply(state.criteria.probs[,2:ncol(state.criteria.probs)], 1, function(x) {
      apply(combn(ncol(state.criteria.probs[,2:ncol(state.criteria.probs)]), 2), 2, function(y) mean(x[y])) }))

    all.event.happened.state <- rowMeans(state.criteria.probs[,2:ncol(state.criteria.probs)])

    extended.states <- cbind(extended.states, all.event.happened.state)

    colnames(extended.states) <- paste0("state.", seq(ncol(state.criteria.probs), ncol(state.criteria.probs)+ncol(extended.states)-1, 1))

    state.df <- cbind(state.criteria.probs, extended.states)

    likelihood.vector.stratum.2 <- likelihood.vector[-1]/likelihood.vector[1]

    likelihood.vector.stratum.3 <- apply(combn(likelihood.vector.stratum.2, 2), 2, prod)

    likelihood.vector.stratum.4 <- prod(likelihood.vector.stratum.2)

    coefficients <- c(
      likelihood.vector.stratum.4,  #Coefficient of p^3
      sum(likelihood.vector.stratum.3),  #Coefficient of p^2
      sum(likelihood.vector.stratum.2)+1,  #Coefficient of p^1
      -1                            #Constant term at the end
    )
    #Find the roots of the equation
    roots <- polyroot(rev(coefficients))

    non.Im.root <- Re(roots[Im(roots) == 0])

    if(length(non.Im.root)==0){

      tolerance <- 1e-10
      non.Im.root <- Re(roots[abs(Im(roots)) < tolerance])
    }

    p.vector <- c(non.Im.root, non.Im.root*likelihood.vector.stratum.2, non.Im.root^2*likelihood.vector.stratum.3, non.Im.root^3*likelihood.vector.stratum.4)


  }else{

    state.df <- state.criteria.probs

    likelihood.vector.stratum.2 <- likelihood.vector[2:4]

    likelihood.vector.stratum.3 <- likelihood.vector[5:7]

    likelihood.vector.stratum.4 <- likelihood.vector[8]

    p.vector <- likelihood.vector


  }


  criteria.percentages <- as.matrix(state.df) %*% (p.vector)

  option.val <- as.matrix(comparison.mat) %*% (as.matrix(state.df) %*% (p.vector))

  return(option.val)
}


#' Function for applying the Best-Worst Method
#'
#' @param criteria.lst list of criteria
#' @param worst.criteria the worst criteria
#' @param best.criteria the best criteria
#' @param best.criteria.preference the comparison of the best criteria to others
#' @param worst.criteria.preference the comparison of the worst criteria to others
#'
#' @return the result of BWM
#' @export
#'
#' @examples
#' criteria.lst <- c("C1", "C2", "C3")
#' worst.criteria <- "C1
#' best.criteria <- "C3"
#' best.criteria.preference <- c(8, 2, 1)
#' worst.criteria.preference <- c(1, 5, 8)
#' apply.BWM(criteria.lst, worst.criteria, best.criteria, best.criteria.preference, worst.criteria.preference)
apply.BWM <- function(criteria.lst, worst.criteria, best.criteria, best.criteria.preference, worst.criteria.preference){

  best.idx <- which(best.criteria == criteria.lst)
  worst.idx <- which(worst.criteria == criteria.lst)

  n_vars <- length(criteria.lst)+1
  objective <- c(rep(0, length(criteria.lst)), 1)

  #Initialize lists for constraints
  constraints <- list()
  directions <- character()
  rhs <- numeric()


  for (j in 1:length(best.criteria.preference)) {
    coef <- rep(0, n_vars)
    coef[best.idx] <- 1

    if(j==best.idx){
      coef[j]=0
    }else{
      coef[j] <- -1 * best.criteria.preference[j]
    }
    coef[n_vars] <- -1


    constraints[[length(constraints) + 1]] <- coef
    directions <- c(directions, "<=")
    rhs <- c(rhs, 0)

  }


  for (j in 1:length(worst.criteria.preference)) {
    coef <- rep(0, n_vars)
    coef[j] <- 1

    if(j==worst.idx){
      coef[j]=0
    }else{
      coef[worst.idx] <- -1 * worst.criteria.preference[j]
    }
    coef[n_vars] <- -1

    constraints[[length(constraints) + 1]] <- coef
    directions <- c(directions, "<=")
    rhs <- c(rhs, 0)

  }


  constraints[[length(constraints) + 1]] <- c(rep(1, length(criteria.lst)), 0)
  directions <- c(directions, "=")
  rhs <- c(rhs, 1)

  constraint_matrix <- do.call(rbind, constraints)

  identity.crit <- diag(length(criteria.lst))

  identity.crit <- cbind(identity.crit, rep(0, nrow(identity.crit)))
  constraint_matrix <- rbind(constraint_matrix, identity.crit)

  directions <- c(directions, rep(">=", nrow(identity.crit)))
  rhs <- c(rhs, rep("0", nrow(identity.crit)))

  #Solve the linear programming problem
  result <- lpSolve::lp("min", objective, constraint_matrix, directions, rhs)

  return(result$solution)
}



#' Function for applying the Stratified Best-Worst Method (SBWM)
#'
#' @param comparison.mat the comparison matrix containing the alternatives as column names
#' and the criteria as row names.
#' @param others.to.worst the comparison of the criteria to the worst criteria for each state,
#' column names should be states and the row names are criteria
#' @param others.to.best the comparison of the criteria to the best criteria for each state,
#' column names should be states and the row names are criteria
#' @param state.worst.lst the vector containing the name of the worst criteria in each state
#' @param state.best.lst the vector containing the name of the best criteria in each state
#' @param likelihood.vector the vector containing the likelihood of being in each state.
#'
#' @return the result of SBWM
#' @export
#'
#' @examples
#' data <- read.csv(system.file("extdata", "stratified_BWM_case_study_I_example.csv", package = "RMCDA"), header = FALSE)
#' mat.lst <- read.csv.SBWM.matrices(data)
#' comparison.mat <- mat.lst[[1]]
#' others.to.worst <- mat.lst[[2]]
#' others.to.best <- mat.lst[[3]]
#' state.worst.lst <- mat.lst[[4]]
#' state.best.lst <- mat.lst[[5]]
#' likelihood.vector <- mat.lst[[6]]
#' apply.SBWM(comparison.mat, others.to.worst, others.to.best, state.worst.lst, state.best.lst, likelihood.vector)
apply.SBWM <- function(comparison.mat, others.to.worst, others.to.best, state.worst.lst, state.best.lst, likelihood.vector){

  apply.SMCDM.internal.SBWM <- function(comparison.mat, state.criteria.probs, likelihood.vector){

    likelihood.vector.stratum.2 <- likelihood.vector[-1]/likelihood.vector[1]

    likelihood.vector.stratum.3 <- apply(combn(likelihood.vector.stratum.2, 2), 2, prod)

    likelihood.vector.stratum.4 <- prod(likelihood.vector.stratum.2)

    coefficients <- c(
      likelihood.vector.stratum.4,  #Coefficient of p^3
      sum(likelihood.vector.stratum.3),  #Coefficient of p^2
      sum(likelihood.vector.stratum.2)+1,  #Coefficient of p^1
      -1                            #Constant term at the end
    )

    #Find the roots of the equation
    roots <- polyroot(rev(coefficients))

    non.Im.root <- Re(roots[Im(roots) == 0])

    if(length(non.Im.root)==0){

      tolerance <- 1e-10
      non.Im.root <- Re(roots[abs(Im(roots)) < tolerance])
    }

    p.vector <- c(non.Im.root, non.Im.root*likelihood.vector.stratum.2, non.Im.root^2*likelihood.vector.stratum.3, non.Im.root^3*likelihood.vector.stratum.4)

    criteria.percentages <-  as.matrix(state.criteria.probs) %*% (p.vector)

    option.val <- as.matrix(comparison.mat) %*% (as.matrix(state.criteria.probs) %*% (p.vector))

    return(option.val)
  }

  if(sum(dim(others.to.worst)==dim(others.to.best))!=2){
    stop("Unable to proceed. Matrix dimensions mismatch.")
  }

  state.weights.lst <- list()

  for(state.no in 1:ncol(others.to.worst)){

    state.weights <- apply.BWM(rownames(others.to.best), state.worst.lst[state.no], state.best.lst[state.no], as.numeric(others.to.best[,state.no]), as.numeric(others.to.worst[,state.no]))

    state.weights.lst[[state.no]] <- state.weights
  }

  state.weights.mat <- do.call(cbind, state.weights.lst)

  state.weights.mat <-as.data.frame(state.weights.mat)[-nrow(state.weights.mat),]

  rownames(state.weights.mat)<- rownames(comparison.mat)

  colnames(state.weights.mat)<- paste0("state.", seq(0, ncol(state.weights.mat)-1, 1))

  rownames(t(comparison.mat))->alt.names

  comparison.mat <- apply(t(comparison.mat), 2, function(x) as.numeric(x))

  rownames(comparison.mat)<-alt.names

  res <- apply.SMCDM.internal.SBWM(comparison.mat, as.matrix(state.weights.mat), as.numeric(likelihood.vector))

  return(res)
}


#' Plot decision tree
#'
#' @param A the comparison matrix
#' @param comparing.competitors the list of matrices related to pairwise comparisons
#' of competitors for each criteria
#' @return the decision tree plot
#' @export
plot.AHP.decision.tree <- function(A, comparing.competitors){

  nodes <- c("Choose alternative",rownames(comparing.competitors[[1]]), rownames(A))

  edges <- c()


  for(idx in seq(1, length(rownames(A)), 1)){
    edges <- c(edges, "Choose alternative")
    edges <- c(edges, rownames(A)[idx])
  }

  for(idx in seq(1,length(rownames(comparing.competitors[[1]])), 1)){
    for(idx.prod in seq(1, length(rownames(A)), 1)){
      edges <- c(edges, rownames(A)[idx.prod])
      edges <- c(edges, rownames(comparing.competitors[[1]])[idx])
    }
  }

  edge_list <- matrix(edges, ncol = 2, byrow = TRUE)
  weights <- c(results[[1]][[2]],
               as.vector(results[[3]]))

  g <- graph_from_edgelist(edge_list)

  E(g)$weight <- weights

  # Normalize the weights for better visualization (optional)
  max_width <- 5  # Maximum edge width
  edge_widths <- max_width * (weights / max(weights))


  p <- plot(g,
       layout = layout_as_tree(g, root = 1),  # Tree-like layout with root at the top
       vertex.size = 50,  # Adjust node size
       vertex.label.cex = input$vertex_font,  # Font size for labels
       vertex.label.color = "black",  # Label color
       vertex.color="#9B7EBD",
       edge.arrow.size = input$edge_font,  # Arrow size
       edge.width = edge_widths*input$edge_font,  # Edge widths based on weights
       asp = input$asp,
       main = "AHP Decision Tree with Weighted Edges")
  return(p)
}

#' Plot spider plot
#'
#' @param data the result of MCDA scores
#' @param colors the color scheme of choice
#'
#' @return the spider plot
#' @export
plot.spider <- function(data, colors=palette("default")){

  as.data.frame(data)->data
  rownames(data)->criteria
  data <- rbind(rep(1, ncol(data)), rep(0, ncol(data)), data)

  radarchart(data,
             axistype = 2,
             pcol=colors,
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.3,
             plwd = 2,
             plty = 1,
             title = "Spider Chart")

  legend(x = "topright",
         legend = criteria,
         col = colors,
         pch = 15,
         bty = "n")
}



