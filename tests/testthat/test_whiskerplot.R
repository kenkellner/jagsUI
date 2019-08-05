context("Test whiskerplot generation")

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

test_that("whiskerplot errors in correct situations", {
  
  skip_on_cran()
  set_up_input()

  set.seed(123)
  out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)
  
  expect_error(whiskerplot('fake','fake1'))
  expect_error(whiskerplot(out,'fake1'))
  expect_error(whiskerplot(out,'alpha',quantiles=c(0.5)))
  expect_error(whiskerplot(out,'alpha',quantiles=c(0.7,0.3)))

})

test_that("whiskerplot generates correct plot", {

  skip_on_cran()
  set_up_input()

  set.seed(123)
  out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)

  whiskerplot_test <- function() whiskerplot(out, c('alpha','mu'))
  vdiffr::expect_doppelganger("whiskerplot test", whiskerplot_test)

})
