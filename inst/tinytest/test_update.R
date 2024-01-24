at_home <- identical( Sys.getenv("AT_HOME"), "TRUE" )
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

out <- jags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
            n.burnin = 50, n.thin = 2, verbose=FALSE)

out2 <- update(out, n.iter=100, n.thin=2, verbose=FALSE)
ref <- readRDS("update_ref.Rds")
expect_equal(out2$mcmc.info$n.iter, 200)
expect_equal(out2$mcmc.info$n.samples, 150)
expect_equal(nrow(out2$samples[[1]]), 50)

# Remove time/date based elements
if(at_home){
  out2$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
  expect_equal(out2[-c(17,19,21)], ref[-c(17,19,21)])
}

# codaOnly---------------------------------------------------------------------
out2 <- update(out, n.iter=100, n.thin=2, verbose=FALSE, codaOnly='mu')

if(at_home){
  ref <- readRDS("update_ref_codaonly.Rds")
  out2$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
  expect_equal(out2[-c(17,19,21)], ref[-c(17,19,21)])
}

# Different saved parameters---------------------------------------------------
out2 <- update(out, n.iter=100, n.thin=2, verbose=FALSE, 
               parameters.to.save=c('beta', 'alpha'))

if(at_home){
  ref <- readRDS("update_ref_diffsaved.Rds")
  out2$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
  expect_equal(out2[-c(17,19,21)], ref[-c(17,19,21)])
}

# DIC = FALSE------------------------------------------------------------------
out2 <- update(out, n.iter=100, n.thin=2, verbose=FALSE, 
               parameters.to.save=c('alpha'), DIC=FALSE)
expect_false(out2$calc.DIC)

if(at_home){
  ref <- readRDS("update_ref_noDIC.Rds")
  out2$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
  expect_equal(out2[-c(15,17,19)], ref[-c(15,17,19)])
}

# Check recovery after process_output errors-----------------------------------
# Setting DIC to -999 forces process_output to error for testing
expect_message(out2 <- update(out, n.iter=100, n.thin=2, verbose=FALSE, 
               parameters.to.save=c('alpha'), DIC=-999))
expect_inherits(out2, "jagsUIbasic")
expect_equal(coda::varnames(out2$samples), c("alpha","deviance"))
expect_equal(names(out2), c("samples", "model"))

# Parallel---------------------------------------------------------------------
if(parallel::detectCores() > 1 & at_home){
  set.seed(123)
  out <- jags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
            n.burnin = 50, n.thin = 2, verbose=FALSE, parallel=TRUE)

  out2 <- update(out, n.iter=100, n.thin=2, verbose=FALSE)
  ref <- readRDS("update_ref.Rds")
  ref$parallel <- TRUE
  out2$mcmc.info$n.cores <- NULL
  ref$mcmc.info$sufficient.adapt <- out2$mcmc.info$sufficient.adapt
  ref$mcmc.info$n.adapt <- out2$mcmc.info$n.adapt
  out2$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
  expect_equal(out2[-c(17,19,21)], ref[-c(17,19,21)])
}
