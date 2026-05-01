#Standalone validation script: reproduce the FUCOM weights reported in
#Sodur et al. (2024), "Application of the full consistency method (FUCOM) -
#Cosine similarity framework in 5G infrastructure investment planning",
#Heliyon 10, e30664. doi:10.1016/j.heliyon.2024.e30664
#
#Run from the RMCDA-main project root with:
#  Rscript inst/examples/validate_FUCOM_heliyon2024.R

#Make sure dependencies are installed
required <- c("nloptr")
missing  <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

#Load the function under test (sourcing avoids needing the installed package)
source("R/FUCOM.R")

#Inputs from the paper, Section 4.2 / Table 2
criteria.lst         <- c("C3", "C1", "C2", "C4", "C5")
comparative.priority <- c(3, 4/3, 3/2, 7/6)

weights <- apply.FUCOM(criteria.lst, comparative.priority)

#Paper Table 1 global weights for the main criteria
paper <- c(C3 = 0.5283, C1 = 0.1761, C2 = 0.1320, C4 = 0.0881, C5 = 0.0755)

cat("Comparative priorities used:\n")
print(setNames(comparative.priority,
               c("phi_C3/C1", "phi_C1/C2", "phi_C2/C4", "phi_C4/C5")))

cat("\nFUCOM weights from apply.FUCOM():\n")
print(round(weights, 4))

cat("\nPaper-reported weights (Table 1):\n")
print(paper)

cat("\nAbsolute difference (computed - paper):\n")
print(round(as.numeric(weights[names(paper)]) - paper, 5))

cat(sprintf("\nDeviation from full consistency (chi): %.3e\n",
            attr(weights, "chi")))
cat(sprintf("Sum of weights: %.6f\n", sum(weights)))
