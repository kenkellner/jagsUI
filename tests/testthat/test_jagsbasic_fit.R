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
              n_iter, n_warmup, n.thin=1,quiet=T)
  
  expect_equal(class(out),"mcmc.list")
  expect_equal(length(out), n_chains)
  expect_equal(colnames(out[[1]])[1:4], c("alpha","beta","deviance","mu[1]"))
  expect_equal(dim(out[[1]]),c(n_iter-n_warmup,20)) 
  
  set.seed(123)
  out2 <- jags.basic(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T,save.model=TRUE)

  expect_equal(class(out2),"jagsUIbasic")
  expect_equal(names(out2),c("samples","model","parameters","modfile",
                             "mcmc.info","run.info"))
  expect_equal(as.character(unlist(sapply(out2,class))), 
               c('mcmc.list','jags','character','character','list','list'))
  expect_equal(colnames(out2$samples[[1]])[1:4], c("alpha","beta","sigma","mu[1]"))
  expect_equal(out[,1:2],out2$samples[,1:2])
  
})

test_that("jagsbasic() returns correct values", {

  skip_on_cran()
  set_up_input()
  
  #reference
  set.seed(456)
  out_ref <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T)$samples

  set.seed(456)
  out <- jags.basic(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T, save.model=TRUE)$samples

  expect_equal(out, out_ref)
})

test_that("Running jagsUIbasic object in parallel returns same values", {

  skip_on_cran()
  set_up_input()
  n_cores <- max(2, parallel::detectCores()-1)

  #reference
  set.seed(456)
  out_ref <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T)$samples

  #Check parallel
  set.seed(456)
  out_par <- jags.basic(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1, parallel=T, n.cores=n_cores,
              quiet=T, save.model=TRUE)$samples

  expect_equal(out_par, out_ref, check.attributes=FALSE)
})
