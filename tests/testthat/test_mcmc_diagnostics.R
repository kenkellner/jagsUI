context("Test MCMC diagnostics")

test_that("Chains are split properly", {
  samples <- readRDS('coda_samples.Rds')
  post_alpha <- mcmc_to_mat(samples, 'alpha')
  expect_equal(dim(post_alpha), c(30,3))
  post_split <- split_chains(post_alpha)
  expect_equal(dim(post_split), c(15,6))
  expect_equal(c(post_alpha), c(post_split))
  post_uneven <- post_alpha[-1,]
  split_uneven <- split_chains(post_uneven)
  expect_equal(dim(split_uneven), c(14,6))
  expect_equal(c(split_uneven), c(post_uneven[-15,]))
  one_row <- post_alpha[1,,drop=F]
  expect_equal(split_chains(one_row),one_row)
})

test_that("Rhat calculation is correct", {
  samples <- readRDS('coda_samples.Rds')
  post_alpha <- mcmc_to_mat(samples, 'alpha')
  expect_equal(Rhat_min(post_alpha), 1.067712, tol=1e-4)
  #if(requireNamespace("rstan", quietly=TRUE)){
  #  expect_equal(Rhat_min(post_alpha), rstan::Rhat(post_alpha))
  #}
  const_post <- matrix(3, nrow=10, ncol=3)
  expect_true(is.na(Rhat_min(const_post)))
  novar_post <- matrix(rep(1:3, each=3),nrow=3)
  expect_true(is.na(Rhat_min(novar_post)))
  #One chain
  expect_equal(Rhat_min(post_alpha[,1,drop=F]), 1.101442, tol=1e-4)
  #One sample
  expect_true(is.na(Rhat_min(post_alpha[1,,drop=FALSE])))
  #With NA
  post_alpha[1,1] <- NA
  expect_equal(Rhat_min(post_alpha), 1.0645, tol=1e-4)
})

test_that("ESS calculation is correct", {

  samples <- readRDS('coda_samples.Rds')
  post_beta <- mcmc_to_mat(samples, 'beta')
  expect_equal(ess(post_beta), 112.5297, tol=1e-4)
  #if(requireNamespace("rstan", quietly=TRUE)){
  #  expect_equal(ess_bulk(post_beta), rstan::ess_bulk(post_beta))
  #} 
  const_post <- matrix(3, nrow=10, ncol=3)
  expect_true(is.na(ess_bulk(const_post)))
  novar_post <- matrix(rep(1:3, each=3),nrow=3)
  expect_equal(ess_bulk(novar_post), ncol(novar_post)*2)
  #One chain
  expect_equal(ess_bulk(post_beta[,1,drop=F]), 44.31364, tol=1e-4)
  #One sample
  expect_equal(ess_bulk(post_beta[1,,drop=FALSE]), ncol(post_beta))
  #With NA
  post_beta[1,1] <- NA
  expect_true(is.na(ess_bulk(post_beta)))
})
