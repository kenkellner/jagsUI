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

# Used below
mu2_est <- out$mean$mu[2]

ref <- readRDS("longley_reference_fit.Rds")

# Remove time/date based elements
out$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
expect_equal(out[-c(17,18,21)], ref[-c(17,18,21)])

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

# Other methods
expect_equal(out$summary, summary(out))

# Double check stats calculations
expect_equal(out$mean$alpha, mean(as.matrix(out$samples[,"alpha"])))
expect_equal(out$sd$alpha, sd(as.matrix(out$samples[,"alpha"])))
expect_equal(out$Rhat$alpha, coda::gelman.diag(out$samples[,"alpha"])$psrf[1])

coda_sum <- summary(out$samples)
expect_equal(out$summary[,"mean"], coda_sum$statistics[,"Mean"])
expect_equal(out$summary[,"sd"], coda_sum$statistics[,"SD"])
expect_equal(out$summary[,"50%"], coda_sum$quantiles[,"50%"])

# codaOnly---------------------------------------------------------------------
out <- jags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
            n.burnin = 50, n.thin = 1, codaOnly=c("mu", "sigma"), verbose=FALSE)
ref <- readRDS("reference_codaOnly.Rds")

out$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
expect_equal(out[-c(17,18,21)], ref[-c(17,18,21)])

# DIC = FALSE------------------------------------------------------------------
out <- jags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
            n.burnin = 50, n.thin = 1, DIC=FALSE, verbose=FALSE)
expect_false(out$calc.DIC)

ref <- readRDS("reference_noDIC.Rds")
out$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
expect_equal(out[-c(15,16,19)], ref[-c(15,16,19)])

# Reordered parameter names----------------------------------------------------
pars_new <- c("mu", "sigma", "alpha", "beta")
out <- jags(data = data, inits = inits, parameters.to.save = pars_new,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
            n.burnin = 50, n.thin = 1, verbose=FALSE)
ref <- readRDS("reference_parsorder.Rds")

out$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
expect_equal(out[-c(17,18,21)], ref[-c(17,18,21)])

# Reordered parameter names and no DIC-----------------------------------------
pars_new <- c("mu", "sigma", "alpha", "beta")
out <- jags(data = data, inits = inits, parameters.to.save = pars_new,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
            n.burnin = 50, n.thin = 1, DIC = FALSE, verbose=FALSE)
ref <- readRDS("reference_parsorder_noDIC.Rds")

out$mcmc.info$elapsed.mins <- ref$mcmc.inf$elapsed.mins
expect_equal(out[-c(15,16,19)], ref[-c(15,16,19)])

# Run in parallel--------------------------------------------------------------
at_home <- identical( Sys.getenv("AT_HOME"), "TRUE" )
if(parallel::detectCores() > 1 & at_home){
  set.seed(123)
  params <- c('alpha','beta','sigma', 'mu')     
  out <- jags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 1000,
            n.burnin = 500, n.thin = 2, verbose=FALSE, parallel=TRUE)
  ref <- readRDS("longley_reference_fit.Rds")
  expect_equal(out[-c(17,18,20:22)], ref[-c(17,18,20:22)])

  # With n.adapt = NULL
  out <- jags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = NULL, n.iter = 100,
            n.burnin = 50, n.thin = 2, verbose=FALSE, parallel=TRUE)
  expect_equal(out$mcmc.info$n.adapt, rep(100,3))
}

# Single parameter saved-------------------------------------------------------
pars_new <- c("alpha")
out <- jags(data = data, inits = inits, parameters.to.save = pars_new,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
            n.burnin = 50, n.thin = 1, DIC = FALSE, verbose=FALSE)
expect_equal(nrow(out$summary), 1)
expect_equal(ncol(out$samples[[1]]), 1)

# Another example from Github issues
modfile2 <- tempfile()
writeLines("
model{
  for (i in 1:n) {
    y[i] ~ dinterval(t[i], c[i, ])
    t[i] ~ dweib(v, lambda)
  }
  v      ~ dunif(0, 10)
  lambda ~ dunif(0, 10)
}
", con=modfile2)

dataList <- list(n = 7L, c = structure(c(5, 7.5, 8, 9, 8, 8, 8, 15, 15, 15,
15, 10, 10, 10), dim = c(7L, 2L)), y = c(1, 1, 1, 1, 1, 1, 1))
out <- jags(data = dataList, parameters.to.save = c("v", "lambda"),
            model.file =modfile2 ,n.chains = 3,n.adapt = 100, n.iter = 100+100,
            n.burnin = 100, n.thin = 5, verbose=FALSE)
expect_equal(rownames(out$summary), c("v", "lambda","deviance"))


# No non-codaOnly parameters---------------------------------------------------

out <- jags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
            n.burnin = 50, n.thin = 1, verbose=FALSE, DIC=FALSE, 
            codaOnly = params)
expect_equal(nrow(out$summary), 0)

# Saved data and inits---------------------------------------------------------
set.seed(123)
run_inits <- jagsUI:::check_inits(inits, 3)

set.seed(123)
out <- jags(data = data, inits = inits, 
            parameters.to.save = c("alpha","beta"),
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
            n.burnin = 50, n.thin = 1, verbose=FALSE, store.data=TRUE)
expect_equal(out$data, data)
expect_equal(out$inits, run_inits)

# Check recovery after process_output errors-----------------------------------
# Setting DIC to -999 forces process_output to error for testing
expect_message(out <- jags(data = data, inits = inits, 
                parameters.to.save = c("alpha","beta"),
                model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 100,
                n.burnin = 50, n.thin = 1, verbose=FALSE, DIC=-999))
expect_inherits(out, "jagsUIbasic")
expect_equal(coda::varnames(out$samples), c("alpha","beta", "deviance"))
expect_equal(names(out), c("samples", "model"))

# Single chain and single iteration--------------------------------------------
out <- jags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 1, n.adapt = 100, n.iter = 100,
            n.burnin = 99, n.thin = 1, DIC = FALSE, verbose=FALSE)
expect_true(all(is.na(out$summary[,"sd"])))
expect_true(all(is.na(out$summary[,"Rhat"])))
expect_true(all(is.na(out$summary[,"n.eff"])))
expect_true(all(out$summary["alpha",3:7] == out$summary["alpha",3]))

# test jags.View---------------------------------------------------------------
at_home <- identical( Sys.getenv("AT_HOME"), "TRUE" )
if(at_home){
  ref <- readRDS("longley_reference_fit.Rds")
  test <- jags.View(ref)
  expect_equal(ncol(test), 10)
  expect_equal(colnames(test)[7:10], c("overlap0", "f", "Rhat", "n.eff"))
  test2 <- jags.View(out)
  expect_equal(ncol(test2), 8)
  expect_equal(colnames(test)[7:8], c("overlap0", "f"))
}

# Error when user tries to set seed--------------------------------------------
expect_error(jags(data = data, inits = inits, parameters.to.save = params,
            model.file = modfile, n.chains = 1, n.adapt = 100, n.iter = 100,
            n.burnin = 50, n.thin = 1, DIC = FALSE, verbose=FALSE, seed=123))

# Single parameter slice-------------------------------------------------------
set.seed(123)
pars_new <- c("mu[2]")
out <- jags(data = data, inits = inits, parameters.to.save = pars_new,
            model.file = modfile, n.chains = 3, n.adapt = 100, n.iter = 1000,
            n.burnin = 500, n.thin = 2, DIC = FALSE, verbose=FALSE)
expect_equal(nrow(out$summary), 1)
expect_equal(ncol(out$samples[[1]]), 1)
expect_equal(out$mean$mu, mu2_est)

# Ragged arrays----------------------------------------------------------------
set.seed(123)

# Should trigger creation of a bunch of missing values in mu[,2] in output
modfile <- tempfile()
writeLines("
model{
  for (i in 1:n){ 
    employed[i] ~ dnorm(mu[i,1], tau)     
    mu[i,1] <- alpha + beta*gnp[i] 
  }

  mu[1,2] <- 1

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
            n.burnin = 50, n.thin = 1, verbose=FALSE)

expect_equal(dim(out$mean$mu), c(16, 2))
expect_true(all(is.na(out$mean$mu[2:16,2])))
expect_equal(dim(out$sims.list$mu), c(150, 16, 2))
expect_equal(nrow(out$summary), 21)
expect_equal(rownames(out$summary)[20], "mu[1,2]")

# When no stochastic nodes (and deviance is not calculated)--------------------
data <- list(a = 1, b = Inf)

modfile <- tempfile()
writeLines("
model{
  x <- a * b
} ", con=modfile)

expect_warning(out <- jags(data=data, parameters.to.save="x", model.file=modfile,
                      n.chains=3, n.iter=10, n.burnin=5, n.adapt=10, DIC=TRUE, verbose=FALSE))

expect_equal(coda::varnames(out$samples), c("x"))
expect_true(is.na(out$Rhat$x))
expect_true(is.null(out$pD))
expect_true(is.null(out$DIC))
