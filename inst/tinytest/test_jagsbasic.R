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

out <- jags.basic(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
            n.burnin = 50, n.thin = 2, verbose=FALSE)

ref <- readRDS("jagsbasic_reference_fit.Rds")

expect_identical(out, ref)

# Saved model and reordered parameter names------------------------------------
params <- c('beta', 'alpha', 'sigma', 'mu')     
out <- jags.basic(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
            n.burnin = 50, n.thin = 2, verbose=FALSE, save.model=TRUE)
ref <- readRDS("jagsbasic_ref_saved.Rds")

expect_identical(names(out), names(ref))
out$model <- ref$model
expect_identical(out, ref)

# Update-----------------------------------------------------------------------
out2 <- update(out, n.iter=100, n.thin = 2, verbose=FALSE)
expect_equal(nrow(out2$samples[[1]]), 50)

ref <- readRDS('jagsbasic_ref_update.Rds')
expect_identical(names(out2), names(ref))
out2$model <- ref$model
expect_equal(out2, ref)

# Error if seed is set---------------------------------------------------------
expect_error(jags.basic(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
            n.burnin = 50, n.thin = 2, verbose=FALSE, save.model=TRUE, seed=123))

# Parallel---------------------------------------------------------------------
at_home <- identical( Sys.getenv("AT_HOME"), "TRUE" )
if(parallel::detectCores() > 1 & at_home){
  set.seed(123)
  out <- jags.basic(data = data, inits = inits, parameters.to.save = params,
              model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
              n.burnin = 50, n.thin = 2, verbose=FALSE, parallel=TRUE)

  ref <- readRDS("jagsbasic_reference_fit.Rds")
  expect_identical(out[-c(17,18,20:22)], ref[-c(17,18,20:22)])
}

# Verbose---------------------------------------------------------------------
co <- capture.output(jags.basic(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
            n.burnin = 50, n.thin = 2, verbose=TRUE, save.model=TRUE)
)
test <- any(sapply(co, function(x) grepl("MCMC took", x)))
expect_true(test)
