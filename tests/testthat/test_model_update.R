context("Test updating jagsUI model output")

set_up_input <- function(){
  data(longley)
  jags_data <<- list(gnp=longley$GNP, employed=longley$Employed, 
             n=length(longley$Employed))
  model_file <<- tempfile()
  writeLines("
  model{
  #Likelihood
  for (i in 1:n){ 
  employed[i] ~ dnorm(mu[i], tau)     
  mu[i] <- alpha + beta*gnp[i]
  }
  #Priors
  alpha ~ dnorm(0, 0.00001)
  beta ~ dnorm(0, 0.00001)
  sigma ~ dunif(0,1000)
  tau <- pow(sigma,-2)
  }", con=model_file)
  params <<- c('alpha','beta','sigma','mu')
  n_chains <<- 3; n_iter <<- 1000; n_warmup <<- 500; n_adapt <<- 100
}

test_that("Updating models from jags() works", {
  skip_on_cran()
  set_up_input()

  set.seed(123)
  out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)

  out2 <- update(out, n.iter=100, verbose=F)
  expect_equal(class(out2), 'jagsUI')
  expect_equal(dim(out2$samples[[1]]), c(100,20))
  expect_equal(as.numeric(out2$summary[1:3,1]),
               c(51.853,0.035,0.740), tol=1e-3)

})

test_that("Updating models from jags.basic() works", {
  skip_on_cran()
  set_up_input()

  set.seed(123)
  out <- jags.basic(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F, save.model=T)

  out2 <- update(out, n.iter=100, verbose=F)
  expect_equal(class(out2), 'jagsUIbasic')
  expect_equal(class(out2$samples), 'mcmc.list')
  expect_equal(dim(out2$samples[[1]]), c(100,20))

})
