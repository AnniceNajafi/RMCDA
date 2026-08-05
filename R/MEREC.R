#' Apply MEREC (MEthod based on the Removal Effects of Criteria) on comparison matrix
#'
#' Unlike the other methods in this package, MEREC cannot infer the direction of a
#' criterion from the sign of its column: the method normalizes by ratios and takes
#' logarithms, so negated cost columns would produce undefined values. The direction
#' of each criterion must therefore be supplied explicitly through \code{optimizations}
#' and matrix \code{A} must hold the original, strictly positive values.
#'
#' @param A the matrix A with row names corresponding to alternatives and column
#' names corresponding to criteria. All entries must be strictly positive.
#' @param optimizations a character vector with one entry per criterion, either
#' \code{"max"} for beneficial criteria or \code{"min"} for non-beneficial (cost)
#' criteria.
#'
#' @return the weight percentages related to matrix A obtained through the MEREC method
#'
#' @examples
#' A <- matrix(c(250, 200, 300, 275,
#'  225, 16, 16, 32,
#'   32, 16, 12, 8,
#'    16, 8, 16, 5,
#'     3, 4, 4, 2), nrow=5, ncol=4)
#' colnames(A)<-c("Price", "Storage space", "Camera", "Looks")
#' rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
#' apply.MEREC(A, c("min", "max", "max", "max"))
#' @export apply.MEREC
apply.MEREC <- function(A, optimizations){

  if(length(optimizations)!=ncol(A)){

    stop("The dimensions of the optimizations vector and matrix do not match. Unable to proceed.")
  }

  optimizations <- tolower(trimws(as.character(optimizations)))

  if(!all(optimizations %in% c("max", "min"))){

    stop("The optimizations vector may only contain 'max' or 'min'. Unable to proceed.")
  }

  A <- as.matrix(A)

  if(any(is.na(A)) || any(A<=0)){

    stop("MEREC requires strictly positive values. Cost criteria should be marked through the optimizations vector instead of being negated. Unable to proceed.")
  }

  #Larger normalized values correspond to poorer performance
  normalized.A <- sapply(seq_len(ncol(A)), function(j){

    if(optimizations[j]=="max") min(A[,j])/A[,j] else A[,j]/max(A[,j])

  })

  normalized.A <- matrix(normalized.A, nrow=nrow(A), ncol=ncol(A))

  abs.log.A <- abs(log(normalized.A))

  #Overall performance of each alternative
  S <- log(1+rowMeans(abs.log.A))

  #Performance of each alternative after removing each criterion
  S.removed <- sapply(seq_len(ncol(A)), function(j) log(1+rowSums(abs.log.A[,-j, drop=FALSE])/ncol(A)))

  S.removed <- matrix(S.removed, nrow=nrow(A), ncol=ncol(A))

  #Sum of absolute deviations, the removal effect of each criterion
  removal.effect <- colSums(abs(S.removed-S))

  weight.percentage <- removal.effect/sum(removal.effect)

  names(weight.percentage) <- colnames(A)

  return(weight.percentage)

}
