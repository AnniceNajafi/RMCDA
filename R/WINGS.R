#' Apply WINGS (Weighted Influence Non-linear Gauge System)
#'
#' @description
#' This function implements the core calculations of the WINGS method,
#' ignoring any plotting or quadrant labeling. It returns three vectors:
#' - r_plus_c: (R + C) for each row/column
#' - r_minus_c: (R - C) for each row/column
#' - weights: normalized weights derived from (R + C).
#'
#' @param mat A square numeric matrix. The WINGS method is typically
#'   applied on an n x n cross-impact or adjacency matrix.
#'
#' @return A list with three elements: \code{r_plus_c}, \code{r_minus_c},
#'   and \code{weights}.
#' @export
#'
#' @examples
#'
#' mat <- matrix(c(75.5, 95, 770, 187, 179, 239, 237,
#' 420, 91, 1365, 1120, 875, 1190, 200,
#' 74.2, 70, 189, 210, 112, 217, 112,
#' 2.8, 2.68, 7.9, 7.9, 4.43, 8.51, 8.53,
#' 21.4, 22.1, 16.9, 14.4, 9.4, 11.5, 19.9,
#' 0.37, 0.33, 0.04, 0.03, 0.016, 0.31, 0.29,
#' 0.16, 0.16, 0.08, 0.08, 0.09, 0.07, 0.06), nrow=7)
#'
#' colnames(mat)<-c("Toughness Index",	"Yield Strength",	"Young's Modulus",
#' "Density",	"Thermal Expansion",	"Thermal Conductivity","Specific Heat")
#' rownames(mat)<-c("AI 2024-T6", "AI 5052-O","SS 301 FH",
#' "SS 310-3AH","Ti-6AI-4V","Inconel 718","70Cu-30Zn")
#'
#' result <- apply.WINGS(mat)
#' result$r_plus_c    # (R + C)
#' result$r_minus_c   # (R - C)
#' result$weights     # Weights
apply.WINGS <- function(mat) {

  D <- mat


  total_sum <- sum(D)
  C <- D / total_sum

  #Construct identity matrix, compute T = C %*% solve(I - C)
  n <- nrow(D)
  I <- diag(n)
  T <- C %*% solve(I - C)  #matrix inverse

  c_i <- colSums(T)
  r_i <- rowSums(T)

  r_plus_c  <- r_i + c_i
  r_minus_c <- r_i - c_i


  weights <- r_plus_c / sum(r_plus_c)

  return(list(
    r_plus_c  = r_plus_c,
    r_minus_c = r_minus_c,
    weights   = weights
  ))
}

