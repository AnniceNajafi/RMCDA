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
#' apply.PROMETHEE(A, weights)
#'
apply.PROMETHEE <- function(A, weights, type="II"){

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
