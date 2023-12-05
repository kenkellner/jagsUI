gen.inits <- jagsUI:::gen.inits
process.input <- jagsUI:::process.input


# Data processing--------------------------------------------------------------
# Stuff that gets passed through unchanged
data1 <- list(a=1, b=c(1,2), c=matrix(rnorm(4), 2,2), 
              d=array(rnorm(8), c(2,2,2)), e=c(NA, 1))
test <- process.input(data1, "a", NULL, 2, 100, 50, 2, NULL, verbose=FALSE)
expect_identical(test$data, data1)

data2 <- list(a=data.frame(v1=c(1,2)), b=data.frame(v1=c(0,1), v2=c(2,3)))
co <- capture.output(test <- process.input(data2, "a", NULL, 2, 100, 50, 2, NULL))
ref_msg <- c("", "Processing function input....... ", "", "Converting data frame 'a' to matrix.","", "Converting data frame 'b' to matrix.", "", "Done. ", " ")
expect_equal(co, ref_msg)
expect_equivalent(test$data, list(a=matrix(c(1,2), ncol=1),
                                  b=matrix(c(0:3), ncol=2)))

# Factor in data
data3 <- list(a=1, b=factor(c("1","2")))
expect_error(process.input(data3, "a", NULL, 2, 100, 50, 2, NULL, verbose=FALSE))

# Character in data
data4 <- list(a=1, b=c("a","b"))
expect_error(process.input(data4, "a", NULL, 2, 100, 50, 2, NULL, verbose=FALSE))

# Vector with attributes is allowed
#vec <- c(1,2)
#attr(vec, "test") <- "test"
#expect_false(is.vector(vec))
#data5 <- list(vec=vec)
#test <- process.input(data5, "a", NULL, 2, 100, 50, 2, NULL, verbose=FALSE)
#expect_equal(data5, test$data5)

# One-column matrix is not converted to vector
data6 <- list(a=matrix(c(1,2), ncol=1))
test <- process.input(data6, "a", NULL, 2, 100, 50, 2, NULL, verbose=FALSE)
expect_equal(test$data, data6)

# Non-list as input errors
#t1 <- 1; t2 <- 2
#data7 <- c("t1", "t2")
#expect_error(process.input(data7, "a", NULL, 2, 100, 50, 2, NULL, verbose=FALSE))


# Parameter vector processing--------------------------------------------------
dat <- list(a=1, b=2)
pars1 <- c("a", "b")
pars2 <- c("deviance","a", "b")

# DIC = FALSE
test <- process.input(dat, pars1, NULL, 2, 100, 50, 2, NULL, verbose=FALSE)
expect_equal(pars1, test$params)

# DIC = TRUE
test <- process.input(dat, pars1, NULL, 2, 100, 50, 2, NULL, verbose=FALSE, DIC=TRUE)
expect_equal(c(pars1, "deviance"), test$params)

# Deviance already in vector
test <- process.input(dat, pars2, NULL, 2, 100, 50, 2, NULL, verbose=FALSE, DIC=TRUE)
expect_equal(pars2, test$params)

# Incorrect format
expect_error(process.input(dat, c(1,2), NULL, 2, 100, 50, 2, NULL, verbose=FALSE))


# MCMC info processing---------------------------------------------------------
dat <- list(a=1, b=2)
pars1 <- c("a", "b")
# n.iter/n.burnin mismatch
expect_error(process.input(dat, pars1, NULL, 2, n.iter=100, n.burnin=100, 
                      2, NULL, verbose=FALSE, DIC=TRUE))
expect_error(process.input(dat, pars1, NULL, 2, n.iter=100, n.burnin=150, 
                      2, NULL, verbose=FALSE, DIC=TRUE))

# n.cores
# when parallel=FALSE
test <- process.input(dat, pars1, NULL, 2, n.iter=100, n.burnin=50, 
                      2, NULL, verbose=FALSE, DIC=TRUE)
expect_true(is.null(test$n.cores))

# when parallel=TRUE defaults to min of nchains and ncores
test <- process.input(dat, pars1, NULL, 2, n.iter=100, n.burnin=50, 
                      2, NULL, verbose=FALSE, DIC=TRUE, parallel=TRUE)
expect_equal(test$n.cores, 2)

avail_cores <- parallel::detectCores()
if(avail_cores > 1){
  try_cores <- avail_cores + 1
  n_chain <- try_cores
  expect_warning(nul <- capture.output(
    test <- process.input(dat, pars1, NULL, n.chains=n_chain, n.iter=100, n.burnin=50, 
    n.thin=2, verbose=TRUE, DIC=TRUE, parallel=TRUE, n.cores=try_cores)))
  expect_equal(test$n.cores, avail_cores)
}

# Initial value processing-----------------------------------------------------
inits1 <- NULL
inits2 <- list(list(a=1, b=2), list(a=3, b=4))
inits3 <- list(a=1, b=2)
inits4 <- function() list(a=1, b=2)
inits5 <- function() list()
inits6 <- 1

# No inits provided
set.seed(123)
test <- gen.inits(inits1, seed=NULL, n.chains=2)
ref <- list(list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 28758),
    list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 78830))
expect_identical(test, ref)

# A list of lists
set.seed(123)
test <- gen.inits(inits2, seed=NULL, n.chains=2)
ref <- list(list(a = 1, b = 2, .RNG.name = "base::Mersenne-Twister",
    .RNG.seed = 28758), list(a = 3, b = 4, .RNG.name = "base::Mersenne-Twister",
    .RNG.seed = 78830))
expect_identical(test, ref)
# Wrong number of list elements for number of chains
expect_error(gen.inits(inits2, seed=NULL, n.chains=3))

# A single list
expect_error(gen.inits(inits3, seed=NULL, n.chains=1))

# A function
set.seed(123)
test <- gen.inits(inits4, seed=NULL, n.chains=2)
ref <- list(list(a = 1, b = 2, .RNG.name = "base::Mersenne-Twister",
    .RNG.seed = 28758), list(a = 1, b = 2, .RNG.name = "base::Mersenne-Twister",
    .RNG.seed = 78830))
expect_identical(test, ref)

# An empty list
set.seed(123)
test <- gen.inits(inits5, seed=NULL, n.chains=2)
ref <- list(list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 28758),
    list(.RNG.name = "base::Mersenne-Twister", .RNG.seed = 78830))
expect_identical(test, ref)

# A number
expect_error(gen.inits(inits6, seed=NULL, n.chains=2))
