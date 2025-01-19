#' Function to apply the Evaluation based on Distance from Average Solution (EDAS) method
#'
#' @param mat is a matrix and contains the values for different properties
#' of different alternatives. Non-beneficial columns need to have negative values
#' @param weights are the weights of each property in the decision making process
#' @return the AS_i index from EDAS from which the final ranking can be found
#' @export
#'
#' @examples
#' mat <- matrix(c(250, 200, 300, 275, 225,
#' 16, 16, 32, 32, 16,
#' 12, 8, 16, 8, 16,
#' 5, 3, 4, 4, 2), nrow=5)
#' colnames(mat)<-c("Price/cost", "Storage Space", "Camera", "Looks")
#' rownames(mat)<-paste0("Mobile", 1:5)
#' mat[,"Price/cost"]<--mat[,"Price/cost"]
#' weights <- c(0.35, 0.25, 0.25, 0.15)
#' apply.EDAS(mat, weights)
apply.EDAS <- function(mat, weights){

  PDA <- t(t(mat)-colMeans(mat))

  PDA [PDA  < 0] <- 0

  PDA <-t(t(PDA)/(colMeans(mat)))

  PDA <- abs(PDA)

  weighted.PDA <- t(t(PDA)*weights)

  NDA <- t(colMeans(mat)-t(mat))

  NDA [NDA  < 0] <- 0

  NDA <-t(t(NDA)/(colMeans(mat)))

  NDA <- abs(NDA)

  weighted.NDA <- t(t(NDA)*weights)


  SP_i <- rowSums(weighted.PDA)
  SN_i <- rowSums(weighted.NDA)

  NSP_i <- SP_i/max(SP_i)
  NSN_i <- 1-SN_i/max(SN_i)

  AS_i <- 1/2*(NSP_i+NSN_i)

  return(AS_i)

}
