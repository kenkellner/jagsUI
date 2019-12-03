context("Test generation of initial values")

test_that('inits are generated properly',{

  expect_error(get_inits('test',1))

  func_inits_bad <- function() c(a=1,b=2)
  expect_error(get_inits(func_inits_bad,3))
  
  list_inits <- replicate(2, list(a=1,b=2), simplify=F)
  expect_error(get_inits(list_inits,3))
  list_inits <- replicate(3, c(a=1,b=2), simplify=F)
  expect_error(get_inits(list_inits, 3))

  set.seed(123)
  list_inits <- replicate(3, list(a=1,b=2), simplify=F)
  inits <- get_inits(list_inits, 3)
  expect_equal(inits[[1]], list(a = 1, b = 2, 
                  .RNG.name = "base::Mersenne-Twister", .RNG.seed = 51663L))

  set.seed(123)
  func_inits <- function() list(a=1,b=2)
  inits <- get_inits(func_inits, 3)
  expect_equal(inits[[1]], list(a = 1, b = 2, 
                  .RNG.name = "base::Mersenne-Twister", .RNG.seed = 51663L))
  set.seed(123)
  inits <- get_inits(NULL, 3)
  expect_equal(inits[[1]], list(.RNG.name = "base::Mersenne-Twister", 
                                .RNG.seed = 51663L))

  list_inits <- replicate(3, list(a=1,b=2, .RNG.seed=123), simplify=F)
  inits <- get_inits(list_inits, 3)
  expect_equal(inits[[1]], list(a=1,b=2, .RNG.seed=123), simplify=F)
})

test_that('add_RNG adds RNG to inits when required',{ 
  set.seed(123)
  inits <- list(list(a=1,b=2))
  expect_equal(add_RNG(inits), list(list(a=1,b=2,
                                .RNG.name = "base::Mersenne-Twister", 
                                .RNG.seed = 51663L)))
  inits <- list(list(a=1,b=2,.RNG.seed = 123L))
  expect_equal(add_RNG(inits), list(list(a=1,b=2,.RNG.seed=123L)))
})
