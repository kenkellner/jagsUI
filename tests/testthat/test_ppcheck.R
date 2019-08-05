context("Test pp.check plot generation")

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

  res[i] <- employed[i] - mu[i]   
  emp.new[i] ~ dnorm(mu[i], tau)
  res.new[i] <- emp.new[i] - mu[i]
  }
  #Priors
  alpha ~ dnorm(0, 0.00001)
  beta ~ dnorm(0, 0.00001)
  sigma ~ dunif(0,1000)
  tau <- pow(sigma,-2)

  #Derived parameters
  fit <- sum(res[])
  fit.new <- sum(res.new[])
  }", con=model_file)
  params <<- c('alpha','beta','sigma','mu','fit','fit.new')
  n_chains <<- 3; n_iter <<- 1000; n_warmup <<- 500; n_adapt <<- 100
}

test_that("pp.check errors in correct situations", {
  
  skip_on_cran()
  set_up_input()

  set.seed(123)
  out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)
  
  expect_error(pp.check('fake','fit','fit.new'))
  expect_error(pp.check(out,'fake','fit.new'))

})

test_that("pp.check generates correct plot", {

  skip_on_cran()
  set_up_input()

  set.seed(123)
  out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)

  ppcheck_test <- function() pp.check(out, 'fit', 'fit.new')
  vdiffr::expect_doppelganger("ppcheck test", ppcheck_test)

})
