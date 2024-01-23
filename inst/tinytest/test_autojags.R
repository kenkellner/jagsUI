set.seed(123)

data(longley)
data <- list(gnp=longley$GNP, employed=longley$Employed, n=nrow(longley))

modfile <- tempfile()
writeLines("
model{
  for (i in 1:n){ 
    employed[i] ~ dnorm(mu[i], tau)     
    mu[i] <- alpha + beta*gnp[i] 
  }
  alpha ~ dnorm(0, 0.00001)
  beta ~ dnorm(0, 0.00001)
  sigma ~ dunif(0,1000)
  tau <- pow(sigma,-2)
}", con=modfile)

inits <- function(){  
  list(alpha=rnorm(1,0,1),beta=rnorm(1,0,1),sigma=runif(1,0,3))  
}
params <- c('alpha','beta','sigma', 'mu')     

nul <- capture.output(
            out <- autojags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.burnin=50, 
            iter.increment=10, n.thin = 2, verbose=FALSE))
ref <- readRDS("autojags_ref.Rds")

# Remove time/date based elements
out$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
expect_equal(out[-c(17,18,21)], ref[-c(17,18,21)])


# codaOnly---------------------------------------------------------------------
nul<- capture.output(
            out <- autojags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.burnin=50, 
            iter.increment=10, n.thin = 2, verbose=FALSE, codaOnly=c("mu")))
ref <- readRDS("autojags_ref_codaonly.Rds")

# Remove time/date based elements
out$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
expect_equal(out[-c(17,18,21)], ref[-c(17,18,21)])

# Check recovery after process_output errors-----------------------------------
# Setting DIC to -999 forces process_output to error for testing
expect_message(nul<- capture.output(
            out <- autojags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.burnin=50, 
            iter.increment=10, n.thin = 2, verbose=FALSE, codaOnly=c("mu"), DIC=-999)))
expect_inherits(out, "jagsUIbasic")
expect_equal(coda::varnames(out$samples), 
             c("alpha","beta", "sigma", paste0("mu[",1:16,"]"),"deviance"))
expect_equal(names(out), c("samples", "model"))

# Save all iterations----------------------------------------------------------
set.seed(123)
params <- c('alpha','beta','sigma', 'mu')     

nul <- capture.output(
            out <- autojags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.burnin=50, 
            iter.increment=10, n.thin = 1, verbose=TRUE, save.all.iter=TRUE))

# Runs three updates of 10 iterations each
expect_true(grepl("Note: ALL iterations", nul[6]))
expect_true(grepl("Update 3", nul[10]))
expect_true(nul[11] == "")
# All are combined to yield 30 total iterations in each chain
expect_equal(coda::niter(out$samples), 30)
ref <- readRDS("autojags_ref_alliter.Rds")
out$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
expect_equal(out[-c(17,18,21)], ref[-c(17,18,21)])

# Parallel----------------------------------------------------------
at_home <- identical( Sys.getenv("AT_HOME"), "TRUE" )
if(at_home){
  set.seed(123)
  nul <- capture.output(
            out <- autojags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.burnin=50, 
            iter.increment=10, n.thin = 2, verbose=TRUE, parallel=TRUE, save.all.iter=FALSE))
  # Runs two updates of 40 iterations each
  expect_true(grepl("Update 13", nul[18]))
  expect_equal(nul[19], "")

  ref <- readRDS("autojags_ref.Rds")

  out$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
  expect_equal(out[-c(17,18,20:22)], ref[-c(17,18,20:22)])

  # Save all iter
  set.seed(123)
  params <- c('alpha','beta','sigma', 'mu')     

  nul <- capture.output(
            out <- autojags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.burnin=50, 
            iter.increment=10, n.thin = 1, verbose=TRUE, parallel=TRUE, save.all.iter=TRUE))
  expect_true(grepl("Update 3", nul[10]))
  expect_equal(nul[11], "")

  expect_equal(coda::niter(out$samples), 30)
  ref <- readRDS("autojags_ref_alliter.Rds")
  expect_equal(out[-c(17,18,20:22)], ref[-c(17,18,20:22)])
}

# test.Rhat--------------------------------------------------------------------
samples <- readRDS('coda_samples.Rds')

rhats <- sapply(1:coda::nvar(samples), function(i) coda::gelman.diag(samples[,i],
                                                      autoburnin=FALSE)$psrf[1])

# None above 1.1
expect_false(any(rhats > 1.1))
expect_false(jagsUI:::test.Rhat(samples, 1.1, params.omit=NULL, verbose=FALSE))

# Sigma is above 1.05
expect_true(any(rhats > 1.05))
expect_true(jagsUI:::test.Rhat(samples, 1.05, params.omit=NULL, verbose=FALSE))

nul <- capture.output(jagsUI:::test.Rhat(samples, 1.05, params.omit=NULL, verbose=TRUE))
expect_true(grepl("sigma", nul[1]))

# Exclude sigma
expect_false(jagsUI:::test.Rhat(samples, 1.05, params.omit="sigma", verbose=FALSE))

# Test input errors------------------------------------------------------------
expect_error(out <- autojags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.burnin=50, 
            iter.increment=10, n.thin = 2, verbose=FALSE, seed=123))

expect_error(out <- autojags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 1, n.adapt = 100, n.burnin=50, 
            iter.increment=10, n.thin = 2, verbose=FALSE))

expect_warning(out <- autojags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.burnin=50, 
            iter.increment=10, n.thin = 2, verbose=FALSE, max.iter=40))

nul <- capture.output(out <- autojags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.burnin=0, 
            iter.increment=10, n.thin = 2, verbose=TRUE, max.iter=5))
expect_equal(nul[8], "Maximum iterations reached.")
