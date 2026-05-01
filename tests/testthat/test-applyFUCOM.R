library(testthat)

test_that("apply.FUCOM reproduces the Heliyon 2024 5G paper main-criteria weights", {

  #Sodur et al. (2024), Heliyon, doi:10.1016/j.heliyon.2024.e30664, Section 4.2.
  #Main criteria ranking from expert opinion (Table 2): C.3 > C.1 > C.2 > C.4 > C.5
  #1-9 scale scores: C.3=1, C.1=3, C.2=4, C.4=6, C.5=7
  #Comparative priorities phi_{k/(k+1)} = omega_{k+1}/omega_k:
  #  phi_{C3/C1}=3, phi_{C1/C2}=4/3, phi_{C2/C4}=3/2, phi_{C4/C5}=7/6
  criteria.lst         <- c("C3", "C1", "C2", "C4", "C5")
  comparative.priority <- c(3, 4/3, 3/2, 7/6)

  weights <- apply.FUCOM(criteria.lst, comparative.priority)

  #Paper Table 1 global weights of main criteria
  expected <- c(C3 = 0.5283, C1 = 0.1761, C2 = 0.1320, C4 = 0.0881, C5 = 0.0755)

  expect_equal(as.numeric(weights[names(expected)]),
               as.numeric(expected),
               tolerance = 1e-3)

  expect_equal(sum(weights), 1, tolerance = 1e-6)

  #This is a fully-consistent FUCOM problem; chi should be ~ 0
  expect_lt(attr(weights, "chi"), 1e-3)
})
