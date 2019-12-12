context("Test output processing")

test_that('fill_array works properly',{
  dat <- 1:10
  indices <- matrix(c(rep(1:5,2),rep(1:2,each=5)),ncol=2)
  out_mat <- matrix(dat,ncol=2)
  expect_equal(fill_array(dat,indices),out_mat)
  dat <- 1:20
  indices <- matrix(c(rep(1:5,4),
                      rep(rep(1:2,each=5),2),
                      rep(1:2,each=10)),ncol=3)
  out_arr <- array(dat,dim=c(5,2,2))
  expect_equal(fill_array(dat,indices),out_arr)
})

test_that('get_posterior_array output structure is correct',{
  samples <- readRDS('coda_samples.Rds')
  n_chains <- length(samples)
  n_samples <- nrow(samples[[1]])
  out <- get_posterior_array('alpha',samples)
  expect_equal(class(out),'numeric')
  expect_equal(length(out),n_samples*n_chains)
  out <- get_posterior_array('mu',samples)
  expect_true(inherits(out,'matrix'))
  expect_equal(dim(out),c(n_samples*n_chains,16))
  out <- get_posterior_array('kappa',samples)
  expect_equal(class(out),'array')
  expect_equal(dim(out),c(n_samples*n_chains,2,2,2))
  #Check error handling
  result <- expect_message(get_posterior_array('fake', samples))
  expect_true(is.na(result))
})

test_that('sims_list generates correct sims.list',{
  samples <- readRDS('coda_samples.Rds')
  out <- sims_list(samples)
  expect_equal(class(out),'list')
  expect_equal(names(out),param_names(samples,T))
  expect_equal(sapply(out, function(x) class(x)[1]),
               c(alpha = "numeric", beta = "numeric", sigma = "numeric",
                 mu = "matrix", kappa = "array", deviance = "numeric"))
  expect_equal(sapply(out,dim), list(alpha = NULL, beta = NULL, sigma = NULL, 
                                    mu = c(90L, 16L), 
                                    kappa = c(90L, 2L, 2L, 2L), 
                                    deviance = NULL))
  #With some parameters excluded
  out_sub <- sims_list(samples, exclude=c('alpha','mu','kappa')) 
  expect_true(inherits(out_sub,'list'))
  expect_equal(names(out_sub), c('beta','sigma','deviance'))
  expect_equal(sapply(out_sub,class), c(beta='numeric',sigma='numeric',
                                    deviance='numeric'))
  expect_equal(sapply(out_sub, length), c(beta=90,sigma=90,deviance=90))
})

test_that('get_stat_array generates correct array',{
  samples <- readRDS('coda_samples.Rds')
  sum <- calc_stats(samples)
  mean_alpha <- get_stat_array('alpha','mean',sum)
  expect_equal(mean_alpha, 51.90933, tol=1e-4)
  f_mu <- get_stat_array('mu','f',sum)
  expect_equal(f_mu, structure(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                          1, 1, 1, 1, 1), .Dim = 16L))
  sd_kappa <- get_stat_array('kappa', 'sd', sum)
  expect_equal(sd_kappa, structure(c(3.24041434459656, 2.99177153512039, 
                                     3.2912326732425, 3.35708890821258, 
                                     3.01174961734256, 3.34886787628231, 
                                     2.97520825307743, 3.16364214294695), 
                                   .Dim = c(2L, 2L, 2L)))
  #Check error handling
  result <- expect_message(get_stat_array('fake','mean',sum))
  expect_true(is.na(result))
})

test_that('all_stat_arrays makes a list of stat arrays for all params', {
  samples <- readRDS('coda_samples.Rds')
  sumstat <- calc_stats(samples)
  sal <- all_stat_arrays(sumstat)
  expect_is(sal, 'list')
  expect_equal(names(sal),colnames(sumstat))
  expect_true(all(sapply(sal,class)=='list'))
  expect_equal(sal$mean$alpha, 51.90933, tol=1e-4)
  expect_equal(dim(sal$sd$kappa), c(2,2,2))
  #With some parameters excluded
  sum_sub <- calc_stats(samples, exclude=c('alpha','mu','kappa'))
  sal_sub <- all_stat_arrays(sum_sub)
  expect_equal(names(sal_sub),colnames(sumstat))
  expect_equal(names(sal_sub$mean), rownames(sum_sub))
  expect_equal(sal_sub$mean, list(beta=0.034614,sigma=0.73937,
                                  deviance=34.05111),tol=1e-4)
})

test_that('check_stat throws errors correctly',{
  mod_summary <- readRDS('calc_stats_output.Rds')
  expect_error(check_stat('fake',mod_summary),'Invalid stat "fake"')
  expect_error(check_stat('mean',mod_summary),NA)
})

test_that('process_output generates correct list of output',{
  samples <- readRDS('coda_samples.Rds')
  out <- process_output(samples)
  expect_is(out,'list')
  expect_equal(length(out),15)
  expect_equal(names(out),c("sims.list", "mean", "sd", "q2.5", "q25", "q50", 
                            "q75", "q97.5","Rhat","n.eff","overlap0", "f",
                            "pD", "DIC", "summary"))
  expect_is(out$sims.list,'list')
  expect_equal(out$sims.list, sims_list(samples))
  expect_equal(length(out$sims.list),length(param_names(samples,simplify=T)))
  expect_equal(out$summary,calc_stats(samples))
  #Check error handling
  result <- expect_message(process_output("junk"))
  expect_true(all(is.na(result)))
})
