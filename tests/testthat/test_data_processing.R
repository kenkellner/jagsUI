context("Test input processing")

test_that("Issues with parameters to save inputs are handled",{
    bad_params1 <- list("a","b","c")
    bad_params2 <- c(1, 2, 3)
    good_params <- c("a", "b", "c")
    good_params2 <- c("a", "b", "c", "deviance")

    expect_error(check_params(bad_params1), 
                 "Parameters to save must be character vector")
    expect_error(check_params(bad_params2),
                 "Parameters to save must be character vector")
    expect_equal(check_params(good_params, DIC=FALSE), good_params)
    expect_equal(check_params(good_params2, DIC=TRUE), good_params2)
    expect_equal(check_params(good_params, DIC=TRUE), good_params2)
})

test_that("Input data is checked properly",{
  

  bad_inp_data <- matrix(1:9,nrow=3)
  good_list <- list(a=1,b=2,c=3)
  empty_list <- list()
  bad_list <- list(1,2,3)
  nn_list <- list(el1=1, el2=2, el3="a")
  nn_list2 <- list(el1=1, el2=2, el3="a", el4="b")

  expect_error(check_data(bad_inp_data), 
               "Input data is class matrix, it should be a named list")

  expect_equal(check_data(good_list), good_list)
  expect_error(check_data(empty_list),
               "Data list is empty", fixed=TRUE)
  expect_error(check_data(bad_list), 
               "All elements of data list must be named", fixed=TRUE)
  expect_error(check_data(nn_list),
               "Data list element(s) [el3] are not numeric", fixed=TRUE)
  expect_error(check_data(nn_list2), 
               "Data list element(s) [el3, el4] are not numeric", fixed=TRUE)

})

test_that("Parallel settings are checked properly",{
  cores_here <- parallel::detectCores()
  miss_inp <- list(n.iter=10, n.burnin=5, n.chains=3)
  good_inp <- list(n.iter=10, n.burnin=5, n.chains=3, n.cores=3)
  over_inp <- list(n.iter=10, n.burnin=5, 
                   n.chains=cores_here+1, n.cores=cores_here+1)

  expect_equal(check_parallel(miss_inp), 
               list(n.iter=10,n.burnin=5,n.chains=3,n.cores=3))
  expect_equal(check_parallel(good_inp), good_inp)

  expect_error(check_parallel(over_inp),
      paste0('More cores requested (',over_inp$n.cores,') than available (',
                    cores_here,')'), fixed=TRUE)

})

test_that("MCMC info is checked properly", {
  
  bad_mcmc <- list(n.iter = 10, n.burnin=11, parallel=FALSE)
  bad_mcmc2 <- list(n.iter = 10, n.burnin=10, parallel=FALSE)
  good_mcmc <- list(n.iter=10, n.burnin=5, parallel=FALSE)

  expect_equal(check_mcmc_info(good_mcmc), good_mcmc)
  expect_error(check_mcmc_info(bad_mcmc), 
               "Number of iterations must be larger than burn-in")
  expect_error(check_mcmc_info(bad_mcmc2), 
               "Number of iterations must be larger than burn-in")

  mcmc_nopar <- list(n.iter = 10, n.burnin=5, n.chains=3, parallel=TRUE)
  expect_equal(check_mcmc_info(mcmc_nopar),
               list(n.iter=10, n.burnin=5, n.chains=3, 
                    parallel=TRUE, n.cores=3))

})


