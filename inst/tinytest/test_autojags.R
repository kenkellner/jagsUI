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
expect_identical(out[-c(17,18,21)], ref[-c(17,18,21)])


# codaOnly---------------------------------------------------------------------
nul<- capture.output(
            out <- autojags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.burnin=50, 
            iter.increment=10, n.thin = 2, verbose=FALSE, codaOnly=c("mu")))
ref <- readRDS("autojags_ref_codaonly.Rds")

# Remove time/date based elements
out$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
expect_identical(out[-c(17,18,21)], ref[-c(17,18,21)])
