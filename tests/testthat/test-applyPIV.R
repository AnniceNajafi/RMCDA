library(testthat)

test_that("apply.PIV works for provided example and returns the expected structure", {

  mat <- matrix(c(80, 60, 90,
                                   75, 85, 95,
                                   70, 65, 85,
                                   60, 75, 80),
                                   nrow = 4, byrow = TRUE)
  colnames(mat) <- c("Criterion 1", "Criterion 2", "Criterion 3")
  weights <- c(0.4, 0.3, 0.3)
  beneficial.vector <- c(1, 2, 3)
  results <- apply.PIV(mat, weights, beneficial.vector)

  expect_length(results, nrow(mat))

  expect_type(results, "double")
})
