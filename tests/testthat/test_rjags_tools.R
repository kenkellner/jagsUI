context("Test rjags tools")

test_that('load_modules loads and unloads correctly',{
  load_modules(NULL)
  expect_equal(rjags::list.modules(),c('basemod','bugs'))
  load_modules('glm')
  expect_equal(rjags::list.modules(),c('basemod','bugs','glm'))
  load_modules(NULL)
  expect_equal(rjags::list.modules(),c('basemod','bugs'))
})

test_that('get_chunks bins iterations correctly',{
  expect_equal(get_chunks(100),rep(10,10))
  expect_equal(get_chunks(105),c(15,rep(10,9)))
  expect_equal(get_chunks(40),40)
})

test_that('time_left calculates correctly',{
  t_start <- as.POSIXct('2019-01-01 00:00:00 EDT')
  iter_done <- 10
  iter_total <- 100
  t_end <- t_start + 1
  expect_equal(time_left(t_start, iter_done, iter_total, t_end),
               '9 sec left')
  t_end <- t_start + 10
  expect_equal(time_left(t_start, iter_done, iter_total, t_end),
               '1.5 min left')
  t_end <- t_start + 1000
  expect_equal(time_left(t_start, iter_done, iter_total, t_end),
               '2.5 hr left')
})

test_that('init_model creates new jags model',{
  skip_on_cran()
  data(longley)
  jags_data <- list(gnp=longley$GNP, employed=longley$Employed, 
             n=length(longley$Employed))
  model_file <- tempfile()
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
  params <- c('alpha','beta','sigma')
  
  mod <- init_model(model_file, jags_data, n_chains=3)
  expect_equal(class(mod),'jags')
  expect_equal(mod$nchain(),3)
  expect_equal(mod$state()[[1]],list(alpha=0,beta=0,sigma=500))
  expect_equal(mod$iter(),0)
  
  inits <- list(
    list(alpha=0.1,beta=0.2,sigma=10),
    list(alpha=0.2,beta=0.3,sigma=5),
    list(alpha=0,beta=0,sigma=500)
  )
  mod <- init_model(model_file, jags_data, inits, n_chains=3)
  expect_equal(mod$state(), inits)

  console_out <- capture.output(out <- init_model(model_file, jags_data, 
                                                  NULL, 1))
  expect_equal(console_out,character(0))
})

test_that('recompile_model works correctly',{
  skip_on_cran()
  mod <- readRDS('mod_after_init.Rds')
  expect_equal(class(mod),'jags')
  expect_error(mod$iter())
  expect_true(recompile_model(mod))
  expect_equal(mod$iter(),0)
})

test_that('adapt_model works correctly',{
  skip_on_cran()
  set.seed(123)
  mod <- readRDS('mod_after_init.Rds')
  expect_error(mod$iter())
  out <- adapt_model(mod, 100)
  expect_true(out)
  expect_equal(mod$iter(),100)
  set.seed(123)
  mod <- readRDS('mod_after_init.Rds')
  out <- adapt_model(mod, 5)
  expect_false(out)
  expect_equal(mod$iter(),5)
})

test_that('update_model works correctly',{
  skip_on_cran()
  mod <- readRDS('mod_after_adapt.Rds')
  expect_error(mod$iter())
  recompile_model(mod)
  update_model(mod, n_iter=100)
  expect_equal(mod$iter(),100)
  out <- update_model(mod, n_iter=0)
  expect_false(out)
  expect_equal(mod$iter(),100)
})
