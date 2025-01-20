#' Apply Ordinal Priority Approach (OPA)
#'
#' This function applies the Ordinal Priority Approach (OPA) to determine the optimal weights
#' for experts, criteria, and alternatives based on expert opinions, ranks, and criterion importance.
#'
#' @param expert.opinion.lst A list of matrices where each matrix represents the rankings of alternatives
#'                           for each criterion as assessed by a particular expert. Each row corresponds
#'                           to an alternative, and each column corresponds to a criterion.
#' @param expert.rank A numeric vector specifying the rank or weight of importance for each expert.
#' @param criterion.rank.lst A list of numeric vectors where each vector represents the rank or weight
#'                           of importance for the criteria as assessed by each expert.
#'
#' @return A list of matrices where each matrix represents the optimal weights for the alternatives
#'         and criteria for a specific expert.
#'
#' @examples
#' # Input Data
#' expert.x.alt <- matrix(c(1, 3, 2, 2, 1, 3), nrow = 3)
#' colnames(expert.x.alt) <- c("c", "q")
#' rownames(expert.x.alt) <- c("alt1", "alt2", "alt3")
#'
#' expert.y.alt <- matrix(c(1, 2, 3, 3, 1, 2), nrow = 3)
#' colnames(expert.y.alt) <- c("c", "q")
#' rownames(expert.y.alt) <- c("alt1", "alt2", "alt3")
#'
#' expert.opinion.lst <- list(expert.x.alt, expert.y.alt)
#' expert.rank <- c(1, 2)  # Ranks of experts
#'
#' # Criterion ranks for each expert
#' criterion.x.rank <- c(1, 2)
#' criterion.y.rank <- c(2, 1)  # Adjusted criterion rank for expert y
#' criterion.rank.lst <- list(criterion.x.rank, criterion.y.rank)
#'
#' # Apply OPA
#' weights <- apply.OPA(expert.opinion.lst, expert.rank, criterion.rank.lst)
#' print(weights)
#' @import lpSolve
#' @export apply.OPA
apply.OPA <- function(expert.opinion.lst, expert.rank, criterion.rank.lst){

  #Count the number of experts, criteria, and alternatives
  n_experts <- length(expert.opinion.lst)
  n_criteria <- ncol(expert.opinion.lst[[1]])
  n_alternatives <- nrow(expert.opinion.lst[[1]])


  n_weights <- n_experts * n_criteria * n_alternatives
  n_vars <- n_weights + 1  #Additional variable for Z in lp equations

  #Generate variable names for weights
  weight_names <- c()
  for (expert in 1:n_experts) {
    for (criterion in 1:n_criteria) {
      for (alt in 1:n_alternatives) {
        weight_names <- c(weight_names, paste0("w", letters[24 + expert], colnames(expert.opinion.lst[[1]])[criterion], alt))
      }
    }
  }
  #Add Z as the last variable
  weight_names <- c(weight_names, "Z")

  #Objective function is to maximize Z
  f_obj <- c(rep(0, n_weights), 1)  #Coefficients: 0 for weights, 1 for Z

  #Initialize constraints, RHS, and directions
  constraints <- matrix(0, nrow = 0, ncol = n_vars)
  rhs <- c()
  direction <- c()

  #Iterate through each expert, each criterion (column), and create constraints
  for (expert in 1:n_experts) {
    for (criterion in 1:n_criteria) {
      #Extract the column for this expert and criterion
      rankings <- expert.opinion.lst[[expert]][, criterion]
      ranked_indices <- order(rankings)  #Sort alternatives by rank (lowest to highest)

      #Rank multiplier for expert and criterion
      rank_expert <- expert.rank[expert]
      rank_criterion <- criterion.rank.lst[[expert]][criterion]  #Use specific criterion rank for the expert

      #Compare each rank pair: lowest - next lowest
      for (i in 1:(n_alternatives - 1)) {
        #Get indices for alternatives
        index_low <- (expert - 1) * (n_criteria * n_alternatives) + (criterion - 1) * n_alternatives + ranked_indices[i]
        index_next_low <- (expert - 1) * (n_criteria * n_alternatives) + (criterion - 1) * n_alternatives + ranked_indices[i + 1]

        #Get rank (value) for the positive weight
        rank_positive <- expert.opinion.lst[[expert]][ranked_indices[i], criterion]

        #Compute multiplier
        multiplier <- rank_expert * rank_criterion * rank_positive

        #Constraint: Z <= (Weight_low - Weight_next_low) * multiplier
        constraint <- rep(0, n_vars)
        constraint[index_low] <- multiplier
        constraint[index_next_low] <- -multiplier
        constraint[n_vars] <- -1  #
        constraints <- rbind(constraints, constraint)
        rhs <- c(rhs, 0)
        direction <- c(direction, ">=")
      }

      # Add a single constraint for the highest-ranked weight
      index_highest <- (expert - 1) * (n_criteria * n_alternatives) + (criterion - 1) * n_alternatives + ranked_indices[n_alternatives]

      rank_highest <- expert.opinion.lst[[expert]][ranked_indices[n_alternatives], criterion]

      multiplier_highest <- rank_expert * rank_criterion * rank_highest

      constraint <- rep(0, n_vars)
      constraint[index_highest] <- multiplier_highest  #Positive for highest-ranked weight
      constraint[n_vars] <- -1  #Coefficient for Z
      constraints <- rbind(constraints, constraint)
      rhs <- c(rhs, 0)
      direction <- c(direction, "<=")
    }
  }

  #Add the normalization constraint: sum of all weights = 1
  norm_constraint <- c(rep(1, n_weights), 0)
  constraints <- rbind(constraints, norm_constraint)
  rhs <- c(rhs, 1)
  direction <- c(direction, "=")

  colnames(constraints) <- weight_names

  ####### Solve the linear programming problem #####
  lp_result <- lpSolve::lp("max", f_obj, constraints, direction, rhs)


  if (lp_result$status == 0) {

    solution <- lp_result$solution
    weights <- solution[1:n_weights]
    Z <- solution[n_vars]

    weights_matrix <- array(weights, dim = c(n_alternatives, n_criteria, n_experts))
    weights_list <- lapply(1:n_experts, function(e) weights_matrix[,,e])


  } else {
    stop("Linear programming problem could not be solved. Please check paramerters.\n")
  }


  return(weights_list)

}


