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

test_that("All stats for one parameter calculated correctly", {

  samples <- readRDS('coda_samples.Rds')
  alpha_mat <- mcmc_to_mat(samples, 'alpha')
  ps <- as.numeric(calc_param_stats(alpha_mat))
  expect_equal(length(ps), 11)
  expect_equal(ps, c(51.90933,0.89475,50.31759,51.17975,51.9649,52.46763,
                     53.6077087,1.06771,100.95584,0,1), tol=1e-4)
  #Test if inf value is present
  alpha_inf <- alpha_mat; alpha_inf[1,1] <- Inf
  expect_equal(calc_param_stats(alpha_inf), rep(NA,11))
  #Test if NA is present
  alpha_na <- alpha_mat; alpha_na[1,1] <- NA
  expect_equal(as.numeric(calc_param_stats(alpha_na)), 
               c(51.9180331,0.89598,50.309243,51.198956,51.98700,52.47200,
                 53.60957,1.064499,NA,0,1),tol=1e-4)
  #Test if all NA
  alpha_na[,] <- NA
  expect_equal(calc_param_stats(alpha_na), rep(NA, 11))
  #Test if one row
  expect_equal(as.numeric(calc_param_stats(alpha_mat[1,,drop=FALSE])),
               c(51.870939,0.8998954,51.15826,51.36934,51.6038732,52.239005,
                 52.8106251, NA, 3, 0, 1), tol=1e-4)
  #Test if error
  alpha_mat[1,1] <- 'a'
  out <- expect_message(expect_warning(calc_param_stats(alpha_mat)))
  expect_equal(out, rep(NA,11))
})

test_that("Stats for all parameters are calculated by calc_stats", {
  samples <- readRDS('coda_samples.Rds')
  st <- calc_stats(samples)
  expect_equal(dim(st), c(length(param_names(samples)), 11))
  expect_equal(rownames(st), param_names(samples))
  expect_equal(colnames(st), c('mean','sd','q2.5','q25','q50','q75','q97.5',
                               'Rhat','n.eff','overlap0','f'))
  ref_output <- readRDS('calc_stats_output.Rds')
  expect_equal(st, ref_output)
})

test_that("Calculating stats for a subset of parameters works", {
  samples <- readRDS('coda_samples.Rds') 
  ref_output <- readRDS('calc_stats_output.Rds')
  st_sub <- calc_stats(samples, exclude=c('alpha','mu'))
  expect_equal(ref_output[-c(1,4:19),], st_sub)
  params_sub <- remove_params(samples, exclude=c('alpha','mu'))
  expect_equal(rownames(st_sub), params_sub)
  #case with no parameters not excluded
  expect_true(is.na(calc_stats(samples, 
                               exclude=param_names(samples,simplify=TRUE))))
})

test_that("Calculation of pD/DIC works", {
  samples <- readRDS('coda_samples.Rds')
  expect_equal(calc_DIC(samples), c(pD=6.660906,DIC=40.712014), tol=1e-4)
  dev_ind <- which_params('deviance', param_names(samples))
  no_dev <- select_cols(samples, -dev_ind)
  expect_true(all(is.na(calc_DIC(no_dev))))
  samp_na <- samples
  ind <- which_params('deviance',param_names(samples))
  samp_na[[1]][1,ind] <- NA 
  expect_true(all(is.na(calc_DIC(samp_na))))
  samp_inf <- samples
  samp_inf[[1]][1,ind] <- Inf
  expect_true(all(is.na(calc_DIC(samp_na))))
  samp_inf[[1]][1,ind] <- -Inf
  expect_true(all(is.na(calc_DIC(samp_na))))
})
