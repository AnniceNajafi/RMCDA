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

  df <- data
  for (j in seq_len(ncol(df))) df[, j] <- as.character(df[, j])

  r <- 1
  repeat {
    if (r > nrow(df)) stop("Unexpected end while searching for criteria header.")
    row.blank <- all(is.na(df[r, ]) | trimws(df[r, ]) == "")
    row.comment <- grepl("^\\s*#", paste0(df[r, 1]), perl = TRUE)
    if (!row.blank && !row.comment) break
    r <- r + 1
  }

  header.row <- df[r, , drop = FALSE]
  last.col <- suppressWarnings(max(which(!(is.na(header.row) | header.row == ""))))
  if (!is.finite(last.col) || last.col < 2) stop("Malformed criteria header.")
  col.names <- as.character(unlist(header.row[1, 2:last.col, drop = TRUE]))
  if (any(trimws(col.names) == "")) stop("Empty column name in criteria header.")

  rr <- r + 1
  rows.idx <- integer()
  while (rr <= nrow(df) && !all(is.na(df[rr, ]) | trimws(df[rr, ]) == "")) { rows.idx <- c(rows.idx, rr); rr <- rr + 1 }
  if (length(rows.idx) != length(col.names)) stop(sprintf("Criteria matrix must be square: found %d rows and %d columns.", length(rows.idx), length(col.names)))

  row.names <- as.character(unlist(df[rows.idx, 1, drop = TRUE]))
  if (any(trimws(row.names) == "")) stop("Empty row name in criteria matrix.")
  if (any(duplicated(row.names))) stop("Duplicate row names in criteria matrix.")

  raw.vals <- as.matrix(df[rows.idx, 2:last.col, drop = FALSE])
  x <- trimws(as.character(raw.vals))
  x[x == ""] <- NA
  is.frac <- grepl("/", x, fixed = TRUE)
  out <- suppressWarnings(as.numeric(x))
  if (any(is.frac, na.rm = TRUE)) {
    idx <- which(is.frac & !is.na(x))
    out[idx] <- sapply(idx, function(i){
      p <- strsplit(x[i], "/", fixed = TRUE)[[1]]
      if (length(p) != 2) return(NA_real_)
      num <- suppressWarnings(as.numeric(trimws(p[1])))
      den <- suppressWarnings(as.numeric(trimws(p[2])))
      if (is.na(num) || is.na(den) || den == 0) return(NA_real_)
      num/den
    })
  }
  matrix(out, nrow = length(rows.idx), ncol = length(col.names)) -> A
  colnames(A) <- col.names
  rownames(A) <- row.names
  if (nrow(A) != ncol(A)) stop(sprintf("Criteria matrix must be square: found %d×%d.", nrow(A), ncol(A)))
  if (!identical(rownames(A), colnames(A))) stop("Criteria row/column names must match and be in the same order.")
  if (any(duplicated(colnames(A)))) stop("Duplicate criteria names found.")

  length(row.names) -> data.dim
  vector("list", data.dim) -> comparing.competitors
  colnames(A) -> names(comparing.competitors)

  rr + 1 -> next.start

  r <- next.start
  repeat {
    if (r > nrow(df)) stop("Unexpected end while searching for alternatives header #1.")
    row.blank <- all(is.na(df[r, ]) | trimws(df[r, ]) == "")
    row.comment <- grepl("^\\s*#", paste0(df[r, 1]), perl = TRUE)
    if (!row.blank && !row.comment) break
    r <- r + 1
  }

  header.row <- df[r, , drop = FALSE]
  last.col <- suppressWarnings(max(which(!(is.na(header.row) | header.row == ""))))
  if (!is.finite(last.col) || last.col < 2) stop("Malformed alternatives header #1.")
  col.names <- as.character(unlist(header.row[1, 2:last.col, drop = TRUE]))
  if (any(trimws(col.names) == "")) stop("Empty column name in alternatives header #1.")

  rr <- r + 1
  rows.idx <- integer()
  while (rr <= nrow(df) && !all(is.na(df[rr, ]) | trimws(df[rr, ]) == "")) { rows.idx <- c(rows.idx, rr); rr <- rr + 1 }
  if (length(rows.idx) != length(col.names)) stop(sprintf("Alternatives matrix #1 must be square: found %d rows and %d columns.", length(rows.idx), length(col.names)))

  row.names <- as.character(unlist(df[rows.idx, 1, drop = TRUE]))
  if (any(trimws(row.names) == "")) stop("Empty row name in alternatives matrix #1.")
  if (any(duplicated(row.names))) stop("Duplicate row names in alternatives matrix #1.")

  raw.vals <- as.matrix(df[rows.idx, 2:last.col, drop = FALSE])
  x <- trimws(as.character(raw.vals))
  x[x == ""] <- NA
  is.frac <- grepl("/", x, fixed = TRUE)
  out <- suppressWarnings(as.numeric(x))
  if (any(is.frac, na.rm = TRUE)) {
    idx <- which(is.frac & !is.na(x))
    out[idx] <- sapply(idx, function(i){
      p <- strsplit(x[i], "/", fixed = TRUE)[[1]]
      if (length(p) != 2) return(NA_real_)
      num <- suppressWarnings(as.numeric(trimws(p[1])))
      den <- suppressWarnings(as.numeric(trimws(p[2])))
      if (is.na(num) || is.na(den) || den == 0) return(NA_real_)
      num/den
    })
  }
  matrix(out, nrow = length(rows.idx), ncol = length(col.names)) -> tmp.mat
  colnames(tmp.mat) <- col.names
  rownames(tmp.mat) <- row.names
  if (nrow(tmp.mat) != ncol(tmp.mat)) stop(sprintf("Alternatives matrix #1 must be square; got %d×%d.", nrow(tmp.mat), ncol(tmp.mat)))
  if (!identical(rownames(tmp.mat), colnames(tmp.mat))) stop("Alternatives matrix #1 row/column names must match and be in the same order.")
  nrow(tmp.mat) -> m.alt
  rownames(tmp.mat) -> alt.names
  tmp.mat -> comparing.competitors[[1]]
  rr + 1 -> next.start

  if (data.dim > 1) {
    for (k in 2:data.dim) {
      r <- next.start
      repeat {
        if (r > nrow(df)) stop(sprintf("Unexpected end while searching for alternatives header #%d.", k))
        row.blank <- all(is.na(df[r, ]) | trimws(df[r, ]) == "")
        row.comment <- grepl("^\\s*#", paste0(df[r, 1]), perl = TRUE)
        if (!row.blank && !row.comment) break
        r <- r + 1
      }

      header.row <- df[r, , drop = FALSE]
      last.col <- suppressWarnings(max(which(!(is.na(header.row) | header.row == ""))))
      if (!is.finite(last.col) || last.col < 2) stop(sprintf("Malformed alternatives header #%d.", k))
      col.names <- as.character(unlist(header.row[1, 2:last.col, drop = TRUE]))
      if (any(trimws(col.names) == "")) stop(sprintf("Empty column name in alternatives header #%d.", k))

      rr <- r + 1
      rows.idx <- integer()
      while (rr <= nrow(df) && !all(is.na(df[rr, ]) | trimws(df[rr, ]) == "")) { rows.idx <- c(rows.idx, rr); rr <- rr + 1 }
      if (length(rows.idx) != length(col.names)) stop(sprintf("Alternatives matrix #%d must be square: found %d rows and %d columns.", k, length(rows.idx), length(col.names)))

      row.names <- as.character(unlist(df[rows.idx, 1, drop = TRUE]))
      if (any(trimws(row.names) == "")) stop(sprintf("Empty row name in alternatives matrix #%d.", k))
      if (any(duplicated(row.names))) stop(sprintf("Duplicate row names in alternatives matrix #%d.", k))

      raw.vals <- as.matrix(df[rows.idx, 2:last.col, drop = FALSE])
      x <- trimws(as.character(raw.vals))
      x[x == ""] <- NA
      is.frac <- grepl("/", x, fixed = TRUE)
      out <- suppressWarnings(as.numeric(x))
      if (any(is.frac, na.rm = TRUE)) {
        idx <- which(is.frac & !is.na(x))
        out[idx] <- sapply(idx, function(i){
          p <- strsplit(x[i], "/", fixed = TRUE)[[1]]
          if (length(p) != 2) return(NA_real_)
          num <- suppressWarnings(as.numeric(trimws(p[1])))
          den <- suppressWarnings(as.numeric(trimws(p[2])))
          if (is.na(num) || is.na(den) || den == 0) return(NA_real_)
          num/den
        })
      }

      matrix(out, nrow = length(rows.idx), ncol = length(col.names)) -> tmp.mat
      colnames(tmp.mat) <- col.names
      rownames(tmp.mat) <- row.names

      if (!all(dim(tmp.mat) == c(m.alt, m.alt))) stop(sprintf("Alternatives matrix #%d must be %d×%d; got %d×%d.", k, m.alt, m.alt, nrow(tmp.mat), ncol(tmp.mat)))
      if (!identical(rownames(tmp.mat), alt.names) || !identical(colnames(tmp.mat), alt.names)) {
        if (setequal(rownames(tmp.mat), alt.names) && setequal(colnames(tmp.mat), alt.names)) tmp.mat[alt.names, alt.names, drop = FALSE] -> tmp.mat else stop(sprintf("Alternatives matrix #%d names do not match the first alternatives matrix.", k))
      }

      tmp.mat -> comparing.competitors[[k]]
      rr + 1 -> next.start
    }
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
