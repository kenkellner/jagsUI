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
              n_iter, n_warmup, n.thin=1,quiet=T)

  out2 <- update(out, n.iter=100, n.adapt=n_adapt, quiet=T)
  expect_equal(class(out2), 'jagsUI')
   
  expect_equal(names(out2), c("sims.list","mean","sd","q2.5","q25","q50",
                             "q75","q97.5","Rhat","n.eff","overlap0","f",
                             "pD","DIC","summary","samples","model",
                             "parameters", "modfile","mcmc.info","run.info"))
  expect_equal(as.character(unlist(sapply(out2, class))), 
               c(rep("list",12),rep("numeric",2),"matrix","mcmc.list",
                 "jags",rep("character",2),"list","list"))
  
  match_out <- readRDS('jags_update.Rds')
  expect_equal(out2$summary, match_out)
  
  expect_equal(dim(out2$samples[[1]]), c(100,20))

  #Test setting new params and thin
  out3 <- update(out, n.iter=100, n.adapt=n_adapt, n.thin=2,
                 parameters.to.save='alpha', quiet=T)
  expect_equal(out3$parameters, c('alpha','deviance'))
  expect_equal(colnames(out3$samples[[1]]), c('alpha','deviance'))
  expect_equal(attr(out3$samples[[1]],'mcpar')[3], 2)
})

test_that("Updating jags models in parallel works",{

  skip_on_cran()
  set_up_input()
  n_cores <- max(2, parallel::detectCores()-1)
  
  set.seed(123)
  out_par <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
                  n_iter, n_warmup, n.thin=1, quiet=T,
                  parallel=TRUE, n.cores=n_cores)
  out_par2 <- update(out_par, n.iter=100, n.adapt=n_adapt, quiet=T)
  
  match_out_par <- readRDS('jags_update.Rds')
  expect_equal(out_par2$summary, match_out_par)

})

test_that("Updating models from jags.basic() works", {
  skip_on_cran()
  set_up_input()

  set.seed(123)
  out_ref <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T)
  out_ref <- update(out_ref, n.iter=100, n.adapt=n_adapt, quiet=T)$samples
  
  set.seed(123)
  out <- jags.basic(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T, save.model=TRUE)
  out <- update(out, n.iter=100, n.adapt=n_adapt, quiet=T)

  expect_equal(class(out),"jagsUIbasic")
  expect_equal(names(out),c("samples","model","parameters","modfile",
                             "mcmc.info","run.info"))
  expect_equal(as.character(unlist(sapply(out,class))), 
               c('mcmc.list','jags','character','character','list','list'))
  expect_equal(colnames(out$samples[[1]])[1:4], c("alpha","beta","sigma","mu[1]"))

  expect_equal(out$samples, out_ref)

})

test_that("Updating models from jags.basic() in parallel works",{

  skip_on_cran()
  set_up_input()
  n_cores <- max(2, parallel::detectCores()-1)

  set.seed(123)
  out_ref <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T)
  out_ref <- update(out_ref, n.iter=100, n.adapt=n_adapt, quiet=T)$samples
  
  set.seed(123)
  out_par <- jags.basic(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,quiet=T, 
              parallel=T, n.cores=n_cores, save.model=TRUE)
  out_par <- update(out_par, n.iter=100, n.adapt=n_adapt, quiet=T)$samples

  expect_equal(out_par, out_ref, check.attributes=FALSE)
})
