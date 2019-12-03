context("Test traceplot generation")

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

test_that("traceplot errors in correct situations", {
  
  skip_on_cran()
  set_up_input()

  set.seed(123)
  out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T)
  
  expect_error(traceplot("fake"))
  expect_error(traceplot(out, parameters=c('fake1','fake2')))
  expect_error(traceplot(out, Rhat_min=1.3))

})

test_that("Single-parameter traceplot works", {

  skip_on_cran()
  set_up_input()

  set.seed(123)
  out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T)

  traceplot_alpha <- function() traceplot(out, 'alpha')
  vdiffr::expect_doppelganger("traceplot alpha", traceplot_alpha)

})

test_that("Multi-parameter traceplot works", {

  skip_on_cran()
  set_up_input()

  set.seed(123)
  out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T)

  traceplot_multi <- function() traceplot(out, c('alpha','beta','sigma'))
  vdiffr::expect_doppelganger("traceplot multi", traceplot_multi)

})

test_that("Setting Rhat_min works", {

  skip_on_cran()
  set_up_input()

  set.seed(123)
  out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T)

  traceplot_rhat <- function() {traceplot(out, c('alpha','beta','sigma'),
                                          Rhat_min=1.0005)}
  vdiffr::expect_doppelganger("traceplot Rhat", traceplot_rhat)

})
