#' Apply Pre-Order Ranking (partial-order analysis)
#'
#' @description
#' This function is an R translation of the Python \code{po.ranking()} function
#' It merges alternatives that are 'I' (indifferent), constructs a 0/1 partial-order matrix from 'P+' entries,
#' sorts the alternatives by row sums, and then removes transitive edges.
#'
#' The function is an R implementation of the pre-order rank and regime function in the pyDecision package
#' Source: https://github.com/Valdecy/pyDecision/blob/master/pyDecision/algorithm/regime.py
#'
#' @param partial.order.str An \code{n x n} character matrix containing pairwise relations. The main relation codes are:
#'   \itemize{
#'     \item "P+" : the row alternative strictly dominates the column alternative
#'     \item "I"  : the two alternatives are indifferent
#'     \item "R", "-", or other placeholders can appear but are less critical here
#'   }
#'
#' @details
#' \enumerate{
#'   \item \strong{Merging 'I' rows/columns:} If \code{partial.order.str[i,j] == "I"}, then
#'   row/column \code{i} is merged into row/column \code{j} (the same logic used in the original Python).
#'   The merged alternative name becomes something like "a2; a1" if alt2 merges alt1.
#'
#'   \item \strong{Partial-Order Matrix:} A 0/1 matrix \code{partial.order.mat} is constructed, marking 'P+' as \code{1}.
#'
#'   \item \strong{Ranking by row sums:} Each row's sum in \code{partial.order.mat} is computed. Alternatives are sorted
#'   by ascending row sum, then reversed if the total sum is nonzero (mirroring the Python logic).
#'
#'   \item \strong{Transitive-edge removal:} We reduce each row by subtracting any row it dominates.
#' }
#'
#' @return A \strong{list} with elements:
#' \describe{
#'   \item{\code{partial.order.str}}{An updated \code{partial.order.str} after merges. Dimensions may be smaller than the input.}
#'   \item{\code{partial.order.mat}}{An \code{n' x n'} numeric matrix of 0/1, where 1 indicates 'P+'.}
#'   \item{\code{alts}}{A character vector of alternative labels, possibly merged (e.g., "a2; a1").}
#'   \item{\code{alts_rank}}{The final ordering of alternatives from most dominating to least dominating.}
#'   \item{\code{rank}}{A 0/1 matrix after removing transitive edges.}
#' }
#'
#' @examples
#' # Create a small 3x3 partial-order matrix
#' po_str <- matrix(c("P+", "P+", "R",
#'                    "R",   "-",   "I",
#'                    "R",   "I",   "-"), nrow=3, byrow=TRUE)
#'
#' # Apply the pre-order ranking
#' res <- apply.po.ranking(po_str)
#'
#' # View partial-order matrix and sorted alternatives
#' print(res$partial.order.mat)
#' print(res$alts_rank)
#'
#' @export
apply.po.ranking <- function(partial.order.str) {
  partial.order.str <- as.matrix(partial.order.str)
  n <- nrow(partial.order.str)


  alts <- paste0("a", seq_len(n))


  i_seq <- rev(seq_len(n))
  while (length(i_seq) > 0) {
    i <- i_seq[1]
    merged <- FALSE
    j_seq <- rev(seq_len(ncol(partial.order.str)))
    for (j in j_seq) {
      if (i != j && partial.order.str[i, j] == "I") {

        alts[j] <- paste(alts[j], alts[i], sep="; ")


        partial.order.str <- partial.order.str[-i, , drop=FALSE]
        partial.order.str <- partial.order.str[, -i, drop=FALSE]
        alts <- alts[-i]

        merged <- TRUE
        break
      }
    }
    if (merged) {
      n <- nrow(partial.order.str)
      i_seq <- rev(seq_len(n))
    } else {
      i_seq <- i_seq[-1]
    }
  }


  n_new <- nrow(partial.order.str)

  partial.order.mat <- matrix(0, n_new, n_new)
  for (i in seq_len(n_new)) {
    for (j in seq_len(n_new)) {
      if (partial.order.str[i, j] == "P+") {
        partial.order.mat[i, j] <- 1
      }
    }
  }

  row_sum <- rowSums(partial.order.mat)
  idx_sorted <- order(row_sum, decreasing=FALSE)
  alts_rank <- alts[idx_sorted]
  if (sum(row_sum) != 0) {
    alts_rank <- rev(alts_rank)
  }

  rank_mat <- partial.order.mat
  for (i in seq_len(n_new)) {
    for (j in seq_len(n_new)) {
      if (rank_mat[i, j] == 1) {
        tmp <- rank_mat[i, ] - rank_mat[j, ]
        tmp[tmp < 0] <- 0
        tmp[tmp > 1] <- 1
        rank_mat[i, ] <- tmp
      }
    }
  }

  list(
    partial.order.str = partial.order.str,
    partial.order.mat = partial.order.mat,
    alts      = alts,
    alts_rank = alts_rank,
    rank      = rank_mat
  )
}


#' Apply REGIME method (using a beneficial.vector)
#'
#' @description
#' This function implements the REGIME method of pairwise comparisons to produce a
#' character matrix (\code{cp.matrix}) that marks each pair of alternatives as either
#' \code{"P+"} (row dominates column), \code{"I"} (indifferent), or \code{"-"} (for diagonals).
#'
#' It uses a \code{beneficial.vector} of column indices for "max" criteria. Columns not in
#' \code{beneficial.vector} are treated as "min". The function can optionally run
#' \code{\link{apply.po.ranking}} on the resulting matrix for partial-order analysis.
#'
#' @param mat A numeric matrix of size \code{n x m} (n alternatives, m criteria).
#' @param beneficial.vector An integer vector of columns that are beneficial ("max").
#'   All other columns are assumed to be "min".
#' @param weights A numeric vector of length \code{m}, containing weights for each criterion.
#' @param doPreOrder A logical. If \code{TRUE}, the function also calls
#'   \code{\link{apply.po.ranking}} on the resulting \code{cp.matrix} and returns both
#'   the matrix and the partial-order results in a list.
#'
#' @details
#' \enumerate{
#'   \item \strong{Weights Normalization:} We first normalize \code{weights} so they sum to 1.
#'   \item \strong{Pairwise Comparison Matrix \code{g_ind}:}
#'     For each pair of alternatives \code{i,k}, and each criterion \code{j}:
#'     \itemize{
#'       \item If \code{j} is beneficial (max) and \code{X[i,j] >= X[k,j]}, we add \code{weights[j]} to \code{g_ind[i,k]}.
#'         Otherwise subtract \code{weights[j]}.
#'       \item If \code{j} is non-beneficial (min) and \code{X[i,j] < X[k,j]}, we add \code{weights[j]}.
#'         Otherwise subtract.
#'     }
#'   \item \strong{cp.matrix:}
#'     \itemize{
#'       \item "P+" if \code{g_ind[i,k] > 0}
#'       \item "I"  if \code{g_ind[i,k] == 0} or \code{g_ind[i,k] == g_ind[k,i]}
#'       \item "-"  for diagonal entries (\code{i == k})
#'     }
#'   \item If \code{doPreOrder = TRUE}, we call \code{\link{apply.po.ranking}} on \code{cp.matrix} to merge 'I'
#'   and build a partial order.
#' }
#'
#' @return
#' \itemize{
#'   \item If \code{doPreOrder = FALSE}, returns an \code{n x n} character matrix \code{cp.matrix}.
#'   \item If \code{doPreOrder = TRUE}, returns a list with two elements:
#'     \itemize{
#'       \item \code{cp.matrix} : the character matrix
#'       \item \code{po.result} : the output from \code{\link{apply.po.ranking}}
#'     }
#' }
#'
#' @examples
#' # Example data: 3 alternatives x 2 criteria
#' mat <- matrix(c(10, 5,
#'                 12, 4,
#'                 11, 6), nrow=3, byrow=TRUE)
#'
#' # Suppose first column is beneficial, second is non-beneficial
#' benef.vec <- c(1)  # means col1 is "max", col2 is "min"
#' wts <- c(0.6, 0.4)
#'
#' # Call apply.REGIME without partial-order
#' regime.out <- apply.REGIME(mat, benef.vec, wts, doPreOrder=FALSE)
#' print(regime.out)
#'
#' # Or with partial-order
#' regime.out2 <- apply.REGIME(mat, benef.vec, wts, doPreOrder=TRUE)
#' print(regime.out2$cp.matrix)
#' print(regime.out2$po.result)
#'
#' @export
apply.REGIME <- function(mat,
                         beneficial.vector,
                         weights,
                         doPreOrder = FALSE) {


  weights <- weights / sum(weights)

  X <- as.matrix(mat)
  n <- nrow(X)
  m <- ncol(X)


  g_ind <- matrix(0, n, n)

  all_cols <- seq_len(m)
  non_beneficial <- setdiff(all_cols, beneficial.vector)

  for (i in seq_len(n)) {
    for (k in seq_len(n)) {
      if (i != k) {
        for (j in seq_len(m)) {
          if (j %in% beneficial.vector) {
            # 'max'
            if (X[i, j] >= X[k, j]) {
              g_ind[i, k] <- g_ind[i, k] + weights[j]
            } else {
              g_ind[i, k] <- g_ind[i, k] - weights[j]
            }
          } else {
            # 'min'
            if (X[i, j] < X[k, j]) {
              g_ind[i, k] <- g_ind[i, k] + weights[j]
            } else {
              g_ind[i, k] <- g_ind[i, k] - weights[j]
            }
          }
        }
      }
    }
  }


  cp.matrix <- matrix("-", n, n)
  for (i in seq_len(n)) {
    for (k in seq_len(n)) {
      if (i != k) {
        if (g_ind[i, k] > 0) {
          cp.matrix[i, k] <- "P+"
        }
        #'I' if tie or symmetrical
        if (abs(g_ind[i, k]) < 1e-14 ||
            abs(g_ind[i, k] - g_ind[k, i]) < 1e-14) {
          cp.matrix[i, k] <- "I"
        }
      }
    }
  }

  #Optionally run po.ranking
  if (isTRUE(doPreOrder)) {
    po.res <- apply.po.ranking(cp.matrix)
    return(list(
      cp.matrix = cp.matrix,
      po.result = po.res
    ))
  } else {
    return(cp.matrix)
  }
}
