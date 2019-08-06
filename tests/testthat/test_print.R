context("Test jagsUI print methods")

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
  params <<- c('alpha','beta','sigma')
  n_chains <<- 3; n_iter <<- 1000; n_warmup <<- 500; n_adapt <<- 100
}

test_that("Print method returns correct output", {

  skip_on_cran()
  set_up_input()

  out <- jags(jags_data, NULL, params, model_file, n_chains, n_adapt,
              n_iter, n_warmup, n.thin=1,verbose=F)

  test <- round(out$summary[,-c(4,6,10,11)],digits=3)
  colnames(test)[3:5] <- c('2.5%','50%','97.5%')
  test[,'n.eff'] <- round(test[,'n.eff'])
  test <- utils::capture.output(test)

  pr <- utils::capture.output(expect_message(print(out)))
  expect_equal(pr, test)

  #keep tests
  test <- round(out$summary[,-c(4,6)],digits=3)
  colnames(test)[3:5] <- c('2.5%','50%','97.5%')
  test[,'n.eff'] <- round(test[,'n.eff'])
  test <- as.data.frame(test)
  test$overlap0 <- as.logical(test$overlap0)
  test <- utils::capture.output(test)

  pr <- utils::capture.output(expect_message(print(out, test=TRUE)))
  expect_equal(pr, test)
  
  #bugs format
  test <- round(out$summary[,-c(10,11)],digits=3)
  test <- utils::capture.output(test)

  pr <- utils::capture.output(expect_message(print(out, bugs=TRUE)))
  expect_equal(pr, test)
})

test_that("Print method warns when adaption was insufficient", {

  skip_on_cran()
  set_up_input()
  n_adapt <- 5

  out <- jags(jags_data, NULL, params, model_file, 
                             n_chains, n_adapt, n_iter, n_warmup, 
                             n.thin=1,verbose=F)

  capture_output(expect_warning(print(out)))
  
})
