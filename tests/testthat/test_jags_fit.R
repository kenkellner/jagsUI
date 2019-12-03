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
              n_iter, n_warmup, n.thin=1,quiet=T)
  
  expect_true(is.list(out))
  expect_equal(class(out), "jagsUI")
  expect_equal(names(out), c("sims.list","mean","sd","q2.5","q25","q50",
                             "q75","q97.5","Rhat","n.eff","overlap0","f",
                             "pD","DIC","summary","samples","model","inits",
                             "parameters", "modfile","mcmc.info","run.info"))
  expect_equal(as.character(unlist(sapply(out, class))), 
               c(rep("list",12),rep("numeric",2),"matrix","mcmc.list",
                 "jags","list",rep("character",2),"list","list"))
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
              n_iter, n_warmup, n.thin=1,quiet=T)

  match_out <- readRDS('jags_out1.Rds')
  expect_equal(out$summary, match_out)

  #Check that setting seed works
  set.seed(123)
  out2 <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T)
  expect_equal(out$summary, out2$summary)

  #Check that output is not fixed
  out3 <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T)
  expect_true(any(out$summary!=out3$summary))

})

test_that("jags() in parallel produces identical results", {

  skip_on_cran()
  set_up_input()
  n_cores <- max(2, parallel::detectCores()-1)
  
  set.seed(123)
  out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T, 
              parallel=TRUE, n.cores=n_cores)

  match_out <- readRDS('jags_out1.Rds')
  expect_equal(out$summary, match_out)
})

test_that("jags() running loudly gives identical results", {

  skip_on_cran()
  set_up_input()
  n_cores <- max(2, parallel::detectCores()-1)

  set.seed(123)
  printed <- capture_output(
    out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
    n_iter, n_warmup, n.thin=1,quiet=F))

  match_out <- readRDS('jags_out1.Rds')
  expect_equal(out$summary, match_out)
  expect_equal(printed, "Initializing model\nAdapting\nBurn-in\nSampling posterior")

  set.seed(123)
  printed <- capture_output(
    out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
    n_iter, n_warmup, n.thin=1,parallel=T,quiet=F))

  expect_equal(out$summary, match_out)

})
