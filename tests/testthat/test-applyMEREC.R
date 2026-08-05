library(testthat)

test_that("apply.MEREC works for provided example and returns the expected structure", {

  A <- matrix(c(250, 200, 300, 275, 225, 16, 16, 32, 32, 16, 12, 8, 16, 8, 16, 5, 3, 4, 4, 2), nrow=5, ncol=4)
  colnames(A)<-c("Price", "Storage space", "Camera", "Looks")
  rownames(A)<-paste0("Mobile ", seq(1, 5, 1))
  result <- apply.MEREC(A, c("min", "max", "max", "max"))
  expect_type(result, "double")

  expect_length(result, ncol(A))

  expect_equal(sum(result), 1)

  expect_true(all(result>=0))

  expect_equal(names(result), colnames(A))

})

test_that("apply.MEREC reproduces the weights reported by Keshavarz-Ghorabaee et al. (2021)", {

  A <- matrix(c(450, 10, 100, 220, 5, 8000, 9100, 8200, 9300, 8400, 54, 2, 31, 1, 23, 145, 160, 153, 162, 158), nrow=5, ncol=4)
  colnames(A)<-paste0("C", seq(1, 4, 1))
  rownames(A)<-paste0("A", seq(1, 5, 1))
  result <- apply.MEREC(A, c("max", "max", "min", "min"))

  expect_equal(unname(round(result, 4)), c(0.5752, 0.0141, 0.4016, 0.0091))

})

test_that("apply.MEREC reproduces the comparative example of Keshavarz-Ghorabaee et al. (2021)", {

  A <- matrix(c(23, 20, 17, 12, 15, 14, 21, 20, 19, 8,
                264, 220, 231, 210, 243, 222, 262, 256, 266, 218,
                2.37, 2.2, 1.98, 1.73, 2, 1.89, 2.43, 2.6, 2.1, 1.94,
                0.05, 0.04, 0.15, 0.2, 0.14, 0.13, 0.06, 0.07, 0.06, 0.11,
                167, 171, 192, 195, 187, 180, 160, 163, 157, 190,
                8900, 9100, 10800, 12300, 12600, 13200, 10300, 11400, 11200, 13400,
                8.71, 8.23, 9.91, 10.21, 9.34, 9.22, 8.93, 8.44, 9.04, 10.11), nrow=10, ncol=7)
  colnames(A)<-paste0("C", seq(1, 7, 1))
  rownames(A)<-paste0("A", seq(1, 10, 1))
  result <- apply.MEREC(A, c("max", "max", "max", "min", "min", "min", "min"))

  expect_equal(unname(round(result, 3)), c(0.324, 0.055, 0.086, 0.368, 0.044, 0.077, 0.045))

})

test_that("apply.MEREC reproduces the overall performance values of the illustrative example", {

  A <- matrix(c(450, 10, 100, 220, 5, 8000, 9100, 8200, 9300, 8400, 54, 2, 31, 1, 23, 145, 160, 153, 162, 158), nrow=5, ncol=4)
  optimizations <- c("max", "max", "min", "min")

  normalized.A <- sapply(seq_len(ncol(A)), function(j){
    if(optimizations[j]=="max") min(A[,j])/A[,j] else A[,j]/max(A[,j])
  })
  S <- log(1+rowMeans(abs(log(normalized.A))))

  expect_equal(round(S, 2), c(0.77, 0.71, 0.65, 1.09, 0.21))

})

test_that("apply.MEREC is invariant to the scale of each criterion", {

  A <- matrix(c(450, 10, 100, 220, 5, 8000, 9100, 8200, 9300, 8400, 54, 2, 31, 1, 23, 145, 160, 153, 162, 158), nrow=5, ncol=4)
  optimizations <- c("max", "max", "min", "min")
  scaled.A <- t(t(A)*c(10, 0.5, 100, 2))

  expect_equal(apply.MEREC(scaled.A, optimizations), apply.MEREC(A, optimizations))

})

test_that("apply.MEREC assigns no weight to a constant criterion", {

  A <- matrix(c(450, 10, 100, 220, 5, 7, 7, 7, 7, 7, 54, 2, 31, 1, 23), nrow=5, ncol=3)
  result <- apply.MEREC(A, c("max", "max", "min"))

  expect_equal(unname(result[2]), 0)

})

test_that("apply.MEREC rejects malformed input", {

  A <- matrix(c(450, 10, 100, 220, 5, 8000, 9100, 8200, 9300, 8400, 54, 2, 31, 1, 23, 145, 160, 153, 162, 158), nrow=5, ncol=4)

  expect_error(apply.MEREC(A, c("max", "max", "min")), "do not match")

  expect_error(apply.MEREC(A, c("max", "max", "min", "cost")), "may only contain")

  negated.A <- A
  negated.A[,3] <- -negated.A[,3]
  expect_error(apply.MEREC(negated.A, c("max", "max", "min", "min")), "strictly positive")

})
