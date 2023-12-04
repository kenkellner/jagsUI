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
out2$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
expect_identical(out2[-c(17,19,21)], ref[-c(17,19,21)])

# codaOnly---------------------------------------------------------------------
out2 <- update(out, n.iter=100, n.thin=2, verbose=FALSE, codaOnly='mu')
ref <- readRDS("update_ref_codaonly.Rds")

out2$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
expect_identical(out2[-c(17,19,21)], ref[-c(17,19,21)])

# Different saved parameters---------------------------------------------------
out2 <- update(out, n.iter=100, n.thin=2, verbose=FALSE, 
               parameters.to.save=c('beta', 'alpha'))
ref <- readRDS("update_ref_diffsaved.Rds")

out2$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
expect_identical(out2[-c(17,19,21)], ref[-c(17,19,21)])

# DIC = FALSE------------------------------------------------------------------
out2 <- update(out, n.iter=100, n.thin=2, verbose=FALSE, 
               parameters.to.save=c('alpha'), DIC=FALSE)
ref <- readRDS("update_ref_noDIC.Rds")

expect_false(out2$calc.DIC)

out2$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
expect_identical(out2[-c(15,17,19)], ref[-c(15,17,19)])