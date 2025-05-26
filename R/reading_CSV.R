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
#' data <- read.csv(system.file("extdata", "AHP_input_file.csv",
#'  package = "RMCDA"), header=FALSE)
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
#' data <- read.csv(system.file("extdata",
#' "stratified_BWM_case_study_I_example.csv",
#'  package = "RMCDA"), header = FALSE)
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
