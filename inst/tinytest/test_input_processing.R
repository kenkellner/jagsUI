process_input <- jagsUI:::process_input
check_inits <- jagsUI:::check_inits

# Overall structure------------------------------------------------------------
data1 <- list(a=1, b=c(1,2), c=matrix(rnorm(4), 2,2), 
              d=array(rnorm(8), c(2,2,2)), e=c(NA, 1))
test <- process_input(data1, params="a", NULL, 2, 1, 100, 50, 2, 
                      NULL, DIC=TRUE, quiet=TRUE, parallel=FALSE)
expect_inherits(test, "list")
expect_equal(names(test), c("data", "params", "inits", "mcmc.info"))

# Data processing--------------------------------------------------------------
# Stuff that gets passed through unchanged
data1 <- list(a=1, b=c(1,2), c=matrix(rnorm(4), 2,2), 
              d=array(rnorm(8), c(2,2,2)), e=c(NA, 1))
test <- process_input(data1, params="a", NULL, 2, 1, 100, 50, 2, 
                      NULL, DIC=TRUE, quiet=TRUE, parallel=FALSE)
expect_equal(test$data, data1)

# Data frame handling
data2 <- list(a=data.frame(v1=c(1,2)), b=data.frame(v1=c(0,1), v2=c(2,3)))
co <- capture.output(
test <- process_input(data2, params="a", NULL, 2, 1, 100, 50, 2, 
                      NULL, DIC=TRUE, quiet=FALSE, parallel=FALSE)
)
ref_msg <- c("", "Processing function input....... ", "", 
             "Converted data.frame a to matrix","", 
             "Converted data.frame b to matrix", "", "Done. ", " ")
expect_equal(co, ref_msg)
expect_equivalent(test$data, list(a=matrix(c(1,2), ncol=1),
                                  b=matrix(c(0:3), ncol=2)))

# non-numeric data frame errors
data2$v3 <- data.frame(v1=c("a","b"))
expect_error(process_input(data2, params="a", NULL, 2, 1, 100, 50, 2, 
                      NULL, DIC=TRUE, quiet=TRUE, parallel=FALSE))

# Factor in data
data3 <- list(a=1, b=factor(c("1","2")))
expect_error(process_input(data3, params="a", NULL, 2, 1, 100, 50, 2, 
                      NULL, DIC=TRUE, quiet=TRUE, parallel=FALSE))

# Character in data
data4 <- list(a=1, b=c("a","b"))
expect_error(process_input(data4, params="a", NULL, 2, 1, 100, 50, 2, 
                      NULL, DIC=TRUE, quiet=TRUE, parallel=FALSE))

# Vector with attributes is allowed
vec <- c(1,2)
attr(vec, "test") <- "test"
expect_false(is.vector(vec))
data5 <- list(vec=vec)
test <- process_input(data5, params="a", NULL, 2, 1, 100, 50, 2, 
                      NULL, DIC=TRUE, quiet=TRUE, parallel=FALSE)
expect_equal(data5, test$data)

# One-column matrix is not converted to vector
data6 <- list(a=matrix(c(1,2), ncol=1))
test <- process_input(data6, params="a", NULL, 2, 1, 100, 50, 2, 
                      NULL, DIC=TRUE, quiet=TRUE, parallel=FALSE)
expect_equal(test$data, data6)

# Non-list as input errors
t1 <- 1; t2 <- 2
data7 <- c("t1", "t2")
expect_error(process_input(data7, params="a", NULL, 2, 1, 100, 50, 2, 
                      NULL, DIC=TRUE, quiet=TRUE, parallel=FALSE))

# List without names as input errors
data8 <- list(1, 2)
expect_error(process_input(data8, params="a", NULL, 2, 1, 100, 50, 2, 
                      NULL, DIC=TRUE, quiet=TRUE, parallel=FALSE))


# Parameter vector processing--------------------------------------------------
dat <- list(a=1, b=2)
pars1 <- c("a", "b")
pars2 <- c("deviance","a", "b")

# DIC = FALSE
test <- process_input(dat, params=pars1, NULL, 2, 1, 100, 50, 2, 
                      NULL, DIC=FALSE, quiet=TRUE, parallel=FALSE)
expect_equal(pars1, test$params)

# DIC = TRUE
test <- process_input(dat, params=pars1, NULL, 2, 1, 100, 50, 2, 
                      NULL, DIC=TRUE, quiet=TRUE, parallel=FALSE)
expect_equal(c(pars1, "deviance"), test$params)

# Deviance already in vector
test <- process_input(dat, params=pars2, NULL, 2, 1, 100, 50, 2, 
                      NULL, DIC=TRUE, quiet=TRUE, parallel=FALSE)
expect_equal(pars2, test$params)

# Incorrect format
expect_error(process_input(dat, params=c(1,2), NULL, 2, 1, 100, 50, 2, 
                      NULL, DIC=FALSE, quiet=TRUE, parallel=FALSE))


# MCMC info processing---------------------------------------------------------
dat <- list(a=1, b=2)
pars1 <- c("a", "b")
# n.iter/n.burnin mismatch
expect_error(process_input(dat, params=pars1, NULL, 2, 1, n_iter=100, n_burnin=100, 2, 
                      NULL, DIC=FALSE, quiet=TRUE, parallel=FALSE))
expect_error(process_input(dat, params=pars1, NULL, 2, 1, n_iter=100, n_burnin=150, 2, 
                      NULL, DIC=FALSE, quiet=TRUE, parallel=FALSE))

# n.cores
# when parallel=FALSE
test <- process_input(dat, params=pars1, NULL, 2, 1, n_iter=100, n_burnin=50, 2, 
                      n_cores=NULL, DIC=FALSE, quiet=TRUE, parallel=FALSE)
expect_true(is.null(test$mcmc.info$n.cores))

# when parallel=TRUE defaults to min of nchains and ncores
test <- process_input(dat, params=pars1, NULL, 2, 1, n_iter=100, n_burnin=50, 2, 
                      n_cores=NULL, DIC=FALSE, quiet=TRUE, parallel=TRUE)
expect_equal(test$mcmc.info$n.cores, 2)

avail_cores <- parallel::detectCores()
if(avail_cores > 1){
  try_cores <- avail_cores + 1
  n_chain <- try_cores
  expect_warning(nul <- capture.output(
  test <- process_input(dat, params=pars1, NULL, n_chains=n_chain, 1, 
                        n_iter=100, n_burnin=50, 2, 
                      n_cores=try_cores, DIC=FALSE, quiet=TRUE, parallel=TRUE)
  ))
  expect_equal(test$mcmc.info$n.cores, avail_cores)
}

# Initial value processing-----------------------------------------------------
inits1 <- NULL
inits2 <- list(list(a=1, b=2), list(a=3, b=4))
inits3 <- list(a=1, b=2)
inits4 <- function() list(a=1, b=2)
inits5 <- function() list()
inits6 <- function() 1
inits7 <- 1

# No inits provided
set.seed(123)
test <- check_inits(inits1, n_chains=2)
ref <- list(list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 28758),
    list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 78830))
expect_equal(test, ref)

# A list of lists
set.seed(123)
test <- check_inits(inits2, n_chains=2)
ref <- list(list(a = 1, b = 2, .RNG.name = "base::Mersenne-Twister",
    .RNG.seed = 28758), list(a = 3, b = 4, .RNG.name = "base::Mersenne-Twister",
    .RNG.seed = 78830))
expect_equal(test, ref)
# Wrong number of list elements for number of chains
expect_error(check_inits(inits2, n_chains=3))

# A single list
expect_error(check_inits(inits3, n_chains=1))

# A function
set.seed(123)
test <- check_inits(inits4, n_chains=2)
ref <- list(list(a = 1, b = 2, .RNG.name = "base::Mersenne-Twister",
    .RNG.seed = 28758), list(a = 1, b = 2, .RNG.name = "base::Mersenne-Twister",
    .RNG.seed = 78830))
expect_equal(test, ref)

# An empty list
set.seed(123)
test <- check_inits(inits5, n_chains=2)
ref <- list(list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 28758),
    list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 78830))
expect_equal(test, ref)

# Function but doesn't return list
expect_error(check_inits(inits6, n_chains=2))

# A number
expect_error(check_inits(inits7, n_chains=2))

# Check exact match when inits is a function with random numbers
set.seed(123)
inits_fun <- function() list(a = rnorm(1), b=rnorm(1))
#inits_ref <- jagsUI:::gen.inits(inits_fun, seed=NULL, 2)
inits_ref <- list(list(a = -0.560475646552213, b = -0.23017748948328, 
                       .RNG.name = "base::Mersenne-Twister",
    .RNG.seed = 55143), list(a = 1.55870831414912, b = 0.070508391424576,
    .RNG.name = "base::Mersenne-Twister", .RNG.seed = 45662))

set.seed(123)
test <- check_inits(inits_fun, 2)
expect_equal(inits_ref, test)
