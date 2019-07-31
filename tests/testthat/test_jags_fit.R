context("Test model fit with jags()")

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

test_that("jags() returns correct output structure",{

  skip_on_cran()
  set_up_input()
  
  out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)
  
  expect_true(is.list(out))
  expect_equal(class(out), "jagsUI")
  expect_equal(names(out), c("sims.list","mean","sd","q2.5","q25","q50",
                             "q75","q97.5","overlap0","f","Rhat","n.eff",
                             "pD","DIC","summary","samples","modfile","model",
                             "parameters","mcmc.info","run.date","parallel",
                             "bugs.format","calc.DIC"))
  expect_equal(as.character(unlist(sapply(out, class))), 
               c(rep("list",12),rep("numeric",2),"matrix","mcmc.list",
                 "character","jags","character","list","POSIXct","POSIXt",
                 rep("logical",3)))
  expect_equal(length(out$sims.list), 5)
  expect_equal(names(out$sims.list),c(params,'deviance'))
  expect_equal(length(out$sims.list$alpha), (n_iter-n_warmup)*n_chains)
  expect_equal(dim(out$sims.list$mu), c((n_iter-n_warmup)*n_chains, 16))
  expect_equal(length(out$mean$mu), 16)
  expect_equal(length(out$mean),5)
  expect_equal(dim(out$summary), c(20,11))
  
})

test_that("jags() summary values are correct",{

  skip_on_cran()
  set_up_input()
  
  set.seed(123)
  out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)

  match_out <- readRDS('jags_out1.Rds')
  expect_equal(out$summary, match_out)

  #Check that setting seed works
  set.seed(123)
  out2 <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)
  expect_equal(out$summary, out2$summary)

  #Check that output is not fixed
  out3 <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)
  expect_true(any(out$summary!=out3$summary))

})

test_that("jags() in parallel produces identical results", {

  skip_on_cran()
  skip_on_travis()
  set_up_input()
  
  set.seed(123)
  out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F, parallel=TRUE)

  match_out <- readRDS('jags_out1.Rds')
  expect_equal(out$summary, match_out)

})
