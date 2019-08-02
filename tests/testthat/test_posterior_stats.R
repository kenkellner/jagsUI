context("Test posterior stat calculation")

test_that("Overlap0 calculation is correct", {
    expect_equal(overlap_0(-2.5, 3.5), 1)
    expect_equal(overlap_0(1.5, 3.5), 0)
    expect_equal(overlap_0(-3,0), 1)
    expect_equal(overlap_0(0, 2.5), 1)
})

test_that("Calculation of f statistic is correct", {

  set.seed(123)
  test <- c(runif(10,-3,-1),runif(20,1,3))
  test <- matrix(test, nrow=10)
  expect_equal(calc_f(test, mean(test)), 2/3 )
  set.seed(123)
  test <- c(runif(10,-10,-8),runif(20,1,2))
  test <- matrix(test, nrow=10)
  expect_equal(calc_f(test, mean(test)), 1/3 )
 
  test[1,1] <- NA
  expect_equal(calc_f(test, mean(test,na.rm=T)), 0.3103, tol=1e-4 )
})


