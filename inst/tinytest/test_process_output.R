at_home <- identical( Sys.getenv("AT_HOME"), "TRUE" )

process_output <- jagsUI:::process_output
calc_stats <- jagsUI:::calc_stats
fill_array <- jagsUI:::fill_array
get_posterior_array <- jagsUI:::get_posterior_array
sims_list <- jagsUI:::sims_list
get_stat_array <- jagsUI:::get_stat_array
all_stat_arrays <- jagsUI:::all_stat_arrays
overlap_0 <- jagsUI:::overlap_0
calc_f <- jagsUI:::calc_f
calc_Rhat <- jagsUI:::calc_Rhat
calc_neff <- jagsUI:::calc_neff
calc_param_stats <- jagsUI:::calc_param_stats
calc_DIC <- jagsUI:::calc_DIC
which_params <- jagsUI:::which_params
param_names <- jagsUI:::param_names

# test that process_output generates correct list of output--------------------
samples <- readRDS('coda_samples.Rds')
out <- process_output(samples, DIC=TRUE, quiet=TRUE)
expect_inherits(out,'list')
expect_equal(length(out),15)
expect_equal(names(out),c("sims.list", "mean", "sd", "q2.5", "q25", "q50", 
                            "q75", "q97.5","overlap0", "f","Rhat","n.eff",
                            "pD", "DIC", "summary"))
expect_true(all(sapply(out$overlap, is.logical)))
expect_inherits(out$sims.list,'list')
expect_equal(out$sims.list, sims_list(samples))
expect_equal(length(out$sims.list),length(param_names(samples,simplify=T)))
cs <- calc_stats(samples)
colnames(cs)[3:7] <- c("2.5%","25%","50%","75%","97.5%")
cs <- cs[,c(1:7,10,11,8,9)]
expect_equal(out$summary, cs)
#Check error handling
expect_message(result <- process_output("junk", DIC=TRUE, quiet=TRUE))
expect_true(all(is.na(result)))

#DIC=FALSE
out2 <- process_output(samples[,-coda::nvar(samples)], DIC=FALSE, quiet=TRUE)
expect_true(is.null(out2$DIC))
expect_true(is.null(out2$pD))
out2 <- process_output(samples[,-coda::nvar(samples)], DIC=TRUE, quiet=TRUE)
expect_true(is.null(out2$DIC))
expect_true(is.null(out2$pD))

#Exclude parameters
out3 <- process_output(samples, coda_only=c("alpha","kappa", "mu"), DIC=TRUE, quiet=TRUE)
expect_equal(names(out3$sims.list), names(out$sims.list))
expect_equal(names(out3$mean), names(out$mean))
expect_false(any(is.na(unlist(out3$mean))))
expect_equal(rownames(out3$summary), c("beta", "sigma", "deviance"))

# Check progress messages
co <- capture.output(out <- process_output(samples, DIC=TRUE, quiet=FALSE))
expect_equal(co, c("Calculating statistics....... ", "", "Done. "))
                

# Unexpected error happens during process_output-------------------------------

# Here one of the arguments is missing
expect_message(out_fail <- process_output(samples, quiet=TRUE))
expect_true(is.null(out_fail)) # result is NULL

#test that process_output matches old jagsUI process.output--------------------
if(at_home){
  old_all <- readRDS("old_jagsUI_output.Rds")
  new_po <- process_output(old_all$samples, DIC=TRUE, quiet=TRUE)
  old_po <- readRDS("old_process_output.Rds")  
  expect_equal(new_po$summary, old_all$summary)
  new_po$summary <- NULL
  expect_equal(new_po, old_po)
}

# test that fill_array works properly------------------------------------------
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


# test that get_posterior_array output structure is correct--------------------
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
expect_message(result <- get_posterior_array('fake', samples))
expect_true(is.na(result))


# test that sims_list generates correct sims.list------------------------------
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


# test that get_stat_array generates correct array-----------------------------
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
expect_message(result <- get_stat_array('fake','mean',sum))
expect_true(is.na(result))

#Exclude
ex <- calc_stats(samples, coda_only="mu")
mu_rows <- ex[grepl("mu", rownames(ex)),]
expect_equal(nrow(mu_rows), 16)
expect_equal(ex[,"mean"], sum[,"mean"])
expect_true(all(is.na(mu_rows[,"sd"])))


# test that all_stat_arrays makes a list of stat arrays for all params---------
samples <- readRDS('coda_samples.Rds')
sumstat <- calc_stats(samples, coda_only=NULL)
sal <- all_stat_arrays(sumstat, coda_only=NULL)
expect_inherits(sal, 'list')
expect_equal(names(sal),colnames(sumstat))
expect_true(all(sapply(sal,class)=='list'))
expect_equal(sal$mean$alpha, 51.90933, tol=1e-4)
expect_equal(dim(sal$sd$kappa), c(2,2,2))

#With some parameters excluded
sum_sub <- calc_stats(samples, coda_only=c('alpha','mu','kappa'))
sal_sub <- all_stat_arrays(sum_sub, coda_only=c("alpha","mu","kappa"))
expect_equal(names(sal_sub),colnames(sumstat))
expect_false(any(is.na(unlist(sal_sub$mean))))
expect_equal(dim(sal_sub$mean$kappa), c(2,2,2))
expect_true(is.na(sal_sub$sd$kappa))


# test that overlap0 calculation is correct------------------------------------
expect_equal(overlap_0(-2.5, 3.5), 1)
expect_equal(overlap_0(1.5, 3.5), 0)
expect_equal(overlap_0(-3,0), 1)
expect_equal(overlap_0(0, 2.5), 1)


# test that calculation of f statistic is correct------------------------------
set.seed(123)
test <- c(runif(10,-3,-1),runif(20,1,3))
test <- matrix(test, nrow=10)
expect_equal(calc_f(test, mean(test)), 2/3 )
set.seed(123)
test <- c(runif(10,-10,-8),runif(20,1,2))
test <- matrix(test, nrow=10)
expect_equal(calc_f(test, mean(test)), 1/3 )

test[1,1] <- NA
expect_equal(round(calc_f(test, mean(test,na.rm=T)),4), 0.3103)


# Test that calculation of Rhat is correct-------------------------------------
samples <- readRDS('coda_samples.Rds')
alpha <- samples[,"alpha"]
expect_equal(calc_Rhat(alpha), 1.003831, tol=1e-4)
expect_error(calc_Rhat(samples))
expect_equal(calc_Rhat(alpha[1]), NA)
alpha[[1]][1] <- Inf
expect_equal(calc_Rhat(alpha), NA)


# test that all stats for one parameter calculated correctly-------------------
samples <- readRDS('coda_samples.Rds')
ps <- calc_param_stats(samples[,'alpha'], FALSE)
expect_equal(length(ps), 11)
expect_equivalent(ps["mean"], mean(unlist(samples[,"alpha"])))
expect_equivalent(ps["sd"], sd(unlist(samples[,"alpha"])))
expect_equivalent(ps["q2.5"], quantile(unlist(samples[,"alpha"]), 0.025))
expect_equivalent(ps["Rhat"], coda::gelman.diag(samples[,"alpha"])$psrf[1])
expect_equivalent(ps, c(51.90933,0.89475,50.31759,51.17975,51.9649,52.46763,
                   53.6077087,0,1,1.0038308,90), tol=1e-4)
#Test if inf value is present
alpha_inf <- samples[,"alpha"]
alpha_inf[[1]][1] <- Inf
expect_equivalent(calc_param_stats(alpha_inf, FALSE), rep(NA,11))
#Test if NA is present
alpha_na <- samples[,"alpha"]
alpha_na[[1]][1] <- NA
expect_equivalent(calc_param_stats(alpha_na, FALSE), 
             c(51.9180331,0.89598,50.309243,51.198956,51.98700,52.47200,
               53.60957,0,1,NA,90),tol=1e-4)
#Test if all NA
alpha_na[[1]][] <- NA
alpha_na[[2]][] <- NA
alpha_na[[3]][] <- NA
expect_equivalent(calc_param_stats(alpha_na, FALSE), rep(NA, 11))
#Test if one row
alpha_one <- samples[,"alpha"]
alpha_one[[1]] <- coda::mcmc(alpha_one[[1]][1])
alpha_one[[2]] <- coda::mcmc(alpha_one[[2]][1])
alpha_one[[3]] <- coda::mcmc(alpha_one[[3]][1])
expect_equivalent(calc_param_stats(alpha_one, FALSE),
             c(51.870939,0.8998954,51.15826,51.36934,51.6038732,52.239005,
               52.8106251, 0, 1, NA, NA), tol=1e-4)
#Test if error
expect_message(out <- calc_param_stats(alpha_one))
expect_true(all(is.na(out)))
expect_true(length(out) == 11)


# test that stats for all parameters are calculated by calc_stats--------------
if(at_home){
  samples <- readRDS('coda_samples.Rds')
  st <- calc_stats(samples)
  expect_equal(dim(st), c(length(param_names(samples)), 11))
  expect_equal(rownames(st), param_names(samples))
  expect_equal(colnames(st), c('mean','sd','q2.5','q25','q50','q75','q97.5',
                             'overlap0','f','Rhat','n.eff'))
  ref_output <- readRDS('calc_stats_output.Rds')
  expect_equal(st, ref_output)
}


# test that calculating stats for a subset of parameters works-----------------
if(at_home){
  samples <- readRDS('coda_samples.Rds') 
  ref_output <- readRDS('calc_stats_output.Rds')
  ref_output[c(1,4:19),2:11] <- NA
  st_sub <- calc_stats(samples, coda_only=c('alpha','mu'))
  expect_equal(ref_output, st_sub)
}


# test that calculation of pD/DIC works----------------------------------------
samples <- readRDS('coda_samples.Rds')
expect_equal(calc_DIC(samples, DIC=TRUE), c(pD=6.660906,DIC=40.712014), tol=1e-4)
dev_ind <- which_params('deviance', param_names(samples))
no_dev <- samples[,-coda::nvar(samples)]
expect_true(is.null(calc_DIC(no_dev, DIC=TRUE)))
expect_true(is.null(calc_DIC(no_dev, DIC=FALSE)))
expect_true(is.null(calc_DIC(samples, DIC=FALSE)))
samp_na <- samples
ind <- which_params('deviance',param_names(samples))
samp_na[[1]][1,ind] <- NA 
expect_true(is.null(calc_DIC(samp_na, DIC=TRUE)))
samp_inf <- samples
samp_inf[[1]][1,ind] <- Inf
expect_true(all(is.na(calc_DIC(samp_na, DIC=TRUE))))
samp_inf[[1]][1,ind] <- -Inf
expect_true(is.null(calc_DIC(samp_na, DIC=TRUE)))
