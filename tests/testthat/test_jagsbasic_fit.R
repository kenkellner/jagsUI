context("Test model fit with jags.basic()")

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

test_that("jagsbasic() returns correct output structure",{

  skip_on_cran()
  set_up_input()
  
  set.seed(123)
  out <- jags.basic(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)
  
  expect_equal(class(out),"mcmc.list")
  expect_equal(length(out), n_chains)
  expect_equal(colnames(out[[1]])[1:4], c("alpha","beta","deviance","mu[1]"))
  expect_equal(dim(out[[1]]),c(n_iter-n_warmup,20)) 
  
  set.seed(123)
  out2 <- jags.basic(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F,save.model=TRUE)

  expect_equal(class(out2),"jagsUIbasic")
  expect_equal(names(out2),c("samples","model"))
  expect_equal(sapply(out2,class), c(samples='mcmc.list',model='jags')) 
  expect_equal(colnames(out2$samples[[1]])[1:4], c("alpha","beta","sigma","mu[1]"))
  expect_equal(out[,1:2],out2$samples[,1:2])
  
})

test_that("jagsbasic() returns correct values", {

  skip_on_cran()
  set_up_input()
  
  #reference
  set.seed(456)
  out_ref <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)$samples

  set.seed(456)
  out <- jags.basic(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F, save.model=TRUE)$samples

  expect_equal(out, out_ref)
})

test_that("jagsbasic() in parallel returns correct values", {

  skip_on_cran()
  skip_on_travis()
  set_up_input()
  
  #reference
  set.seed(456)
  out_ref <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)$samples

  #Check parallel
  set.seed(456)
  out_par <- jags.basic(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1, parallel=T, verbose=F, 
              save.model=TRUE)$samples

  expect_equal(out_par, out_ref, check.attributes=FALSE)

})

test_that("Updating a jagsUIbasic object returns correct values", {

  skip_on_cran()
  set_up_input()
  
  set.seed(123)
  out_ref <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)
  out_ref <- update(out_ref, n.iter=100, verbose=F)$samples
  
  set.seed(123)
  out <- jags.basic(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F, save.model=TRUE)
  out <- update(out, n.iter=100, verbose=F)

  expect_equal(class(out),"jagsUIbasic")
  expect_equal(names(out),c("samples","model"))
  expect_equal(sapply(out,class), c(samples='mcmc.list',model='jags')) 
  expect_equal(colnames(out$samples[[1]])[1:4], c("alpha","beta","sigma","mu[1]"))

  expect_equal(out$samples, out_ref)

})

test_that("Updating a jagsUIbasic object in parallel returns correct values", {

  skip_on_cran()
  skip_on_travis()
  set_up_input()
  
  set.seed(123)
  out_ref <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)
  out_ref <- update(out_ref, n.iter=100, verbose=F)$samples
  
  set.seed(123)
  out_par <- jags.basic(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F, parallel=T, save.model=TRUE)
  out_par <- update(out_par, n.iter=100, verbose=F)$samples

  #Attribute issues will be fixed when I redo the rjags code
  expect_equal(out_par, out_ref, check.attributes=FALSE)

})
