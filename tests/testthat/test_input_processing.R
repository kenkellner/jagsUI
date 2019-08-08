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

test_that("Run info is checked properly",{
  cores_here <- parallel::detectCores()
  
  miss_inp <- list(parallel=TRUE, modules=c(), factories=c())
  expect_equal(check_run_info(miss_inp, 3), 
               list(parallel=TRUE, modules=c(), factories=c(),
                    n.cores=min(3,cores_here)))

  good_inp <- list(parallel=TRUE, n.cores=cores_here, 
                   modules=c(), factories=c())
  expect_equal(check_run_info(good_inp, 3), good_inp)
 
  over_inp <- list(parallel=TRUE, n.cores=cores_here+1, 
                   modules=c(), factories=c())
  expect_error(check_run_info(over_inp, cores_here+1),
      paste0('More cores requested (',over_inp$n.cores,') than available (',
                    cores_here,')'), fixed=TRUE)

  no_par <- list(parallel=FALSE, modules=c(), factories=c())
  expect_equal(check_run_info(no_par, 3), no_par)

})

test_that("MCMC info is checked properly", {
  
  cores_here <- parallel::detectCores()
  
  bad_mcmc <- list(n.iter = 10, n.burnin=11, parallel=FALSE)
  bad_mcmc2 <- list(n.iter = 10, n.burnin=10, parallel=FALSE)
  good_mcmc <- list(n.iter=10, n.burnin=5, parallel=FALSE)

  expect_equal(check_mcmc_info(good_mcmc), good_mcmc)
  expect_error(check_mcmc_info(bad_mcmc), 
               "Number of iterations must be larger than burn-in")
  expect_error(check_mcmc_info(bad_mcmc2), 
               "Number of iterations must be larger than burn-in")
})


