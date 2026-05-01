#' Function for applying the Full Consistency Method (FUCOM)
#'
#' Determines the weights of criteria using the Full Consistency Method
#' proposed by Pamucar, Stevic, and Sremac (2018). The decision-maker ranks
#' the criteria from most to least important and supplies the comparative
#' priority of each ranked criterion relative to the next-ranked one. The
#' weights are then obtained by solving a nonlinear optimization model that
#' minimizes the deviation from full consistency.
#'
#' @param criteria.lst a character vector of criteria names ordered from the
#'   most important to the least important
#' @param comparative.priority a numeric vector of length
#'   \code{length(criteria.lst) - 1}. Element \code{k} is the comparative
#'   priority \eqn{\varphi_{k/(k+1)}} of the \eqn{k}-th ranked criterion over
#'   the \eqn{(k+1)}-th ranked criterion (i.e. the desired ratio
#'   \eqn{w_{j(k)} / w_{j(k+1)}}). All values must be positive.
#'
#' @return a named numeric vector of weights (in the order of
#'   \code{criteria.lst}) that sum to 1. The deviation from full consistency
#'   \eqn{\chi} is attached as an attribute named \code{"chi"}; values close
#'   to zero indicate full consistency.
#'
#' @references Pamucar, D., Stevic, Z., & Sremac, S. (2018). A new model for
#'   determining weight coefficients of criteria in MCDM models: Full
#'   Consistency Method (FUCOM). Symmetry, 10(9), 393.
#'
#' @examples
#' # Four criteria ranked from most to least important: C2 > C1 > C3 > C4
#' criteria.lst <- c("C2", "C1", "C3", "C4")
#' # Comparative priorities: phi_{C2/C1}=1.75, phi_{C1/C3}=1.43, phi_{C3/C4}=1.80
#' comparative.priority <- c(1.75, 1.43, 1.80)
#' apply.FUCOM(criteria.lst, comparative.priority)
#' @export apply.FUCOM
apply.FUCOM <- function(criteria.lst, comparative.priority){

  n <- length(criteria.lst)

  if(n < 2){

    stop("At least two criteria are required.")
  }

  if(length(comparative.priority) != n - 1){

    stop("The length of comparative.priority must equal length(criteria.lst) - 1.")
  }

  if(any(comparative.priority <= 0) || any(!is.finite(comparative.priority))){

    stop("All comparative priority values must be positive and finite.")
  }

  #Decision variables: x = (w_1, w_2, ..., w_n, chi), with w in ranking order.

  eval_f <- function(x){

    return(x[n + 1])
  }

  eval_g_ineq <- function(x){

    w <- x[1:n]
    chi <- x[n + 1]

    g <- numeric(0)

    #|w_{j(k)} / w_{j(k+1)} - phi_{k/(k+1)}| <= chi
    #rewritten (w_{j(k+1)} > 0) as:
    #  w_{j(k)} - phi * w_{j(k+1)} - chi * w_{j(k+1)} <= 0
    #  phi * w_{j(k+1)} - w_{j(k)} - chi * w_{j(k+1)} <= 0
    for(k in 1:(n - 1)){

      phi <- comparative.priority[k]
      g <- c(g, w[k]     - phi * w[k + 1] - chi * w[k + 1])
      g <- c(g, phi * w[k + 1] - w[k]     - chi * w[k + 1])
    }

    #Mathematical transitivity:
    #|w_{j(k)} / w_{j(k+2)} - phi_{k/(k+1)} * phi_{(k+1)/(k+2)}| <= chi
    if(n >= 3){

      for(k in 1:(n - 2)){

        phi.prod <- comparative.priority[k] * comparative.priority[k + 1]
        g <- c(g, w[k]          - phi.prod * w[k + 2] - chi * w[k + 2])
        g <- c(g, phi.prod * w[k + 2] - w[k]          - chi * w[k + 2])
      }
    }

    #sum(w) = 1, expressed as two inequalities
    g <- c(g, sum(w) - 1)
    g <- c(g, 1 - sum(w))

    return(g)
  }

  x0 <- c(rep(1 / n, n), 0.1)
  lb <- c(rep(1e-8, n), 0)
  ub <- c(rep(1,    n), Inf)

  opts <- list(
    "algorithm"   = "NLOPT_LN_COBYLA",
    "xtol_rel"    = 1e-10,
    "maxeval"     = 10000
  )

  result <- nloptr::nloptr(
    x0          = x0,
    eval_f      = eval_f,
    lb          = lb,
    ub          = ub,
    eval_g_ineq = eval_g_ineq,
    opts        = opts
  )

  weights <- result$solution[1:n]
  weights <- weights / sum(weights)
  chi     <- result$solution[n + 1]

  names(weights) <- criteria.lst
  attr(weights, "chi") <- chi

  return(weights)
}
