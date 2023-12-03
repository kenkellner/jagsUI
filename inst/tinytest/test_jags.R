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
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 1000,
            n.burnin = 500, n.thin = 2, verbose=FALSE)

ref <- readRDS("longley_reference_fit.Rds")

# Remove time/date based elements
out$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
expect_identical(out[-c(17,18,21)], ref[-c(17,18,21)])

# Plots
pdf(NULL)
tp <- traceplot(out, ask=FALSE)
dev.off()
expect_true(is.null(tp))

pdf(NULL)
dp <- densityplot(out, ask=FALSE)
dev.off()
expect_true(is.null(dp))

pdf(NULL)
wp <- whiskerplot(out, "mu")
dev.off()
expect_true(is.null(wp))

pdf(NULL)
pp <- pp.check(out, "alpha", "beta")
dev.off()
expect_equal(pp, 0)
