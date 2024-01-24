# test that param_names returns correct names---------------------------------
param_names <- jagsUI:::param_names
samples <- readRDS('coda_samples.Rds')
expect_equal(param_names(samples),
    c("alpha", "beta", "sigma", "mu[1]", "mu[2]", "mu[3]", "mu[4]",
    "mu[5]", "mu[6]", "mu[7]", "mu[8]", "mu[9]", "mu[10]", "mu[11]",
    "mu[12]", "mu[13]", "mu[14]", "mu[15]", "mu[16]", "kappa[1,1,1]",
    "kappa[2,1,1]", "kappa[1,2,1]", "kappa[2,2,1]", "kappa[1,1,2]",
    "kappa[2,1,2]", "kappa[1,2,2]", "kappa[2,2,2]", "deviance"))
expect_equal(param_names(samples,simplify=T),
             c('alpha','beta','sigma','mu','kappa','deviance'))


# test that strip_params removes brackets and indices--------------------------
strip_params <- jagsUI:::strip_params
params_raw <- c('alpha','beta[1]','beta[2]','gamma[1,2]','kappa[1,2,3]')
expect_equal(strip_params(params_raw),
               c('alpha','beta','beta','gamma','kappa'))
expect_equal(strip_params(params_raw,unique=T),
               c('alpha','beta','gamma','kappa'))


# test that match_param identifies correct set of params-----------------------
match_params <- jagsUI:::match_params
params_raw <- c('alpha','beta[1]','beta[2]','gamma[1,1]','gamma[3,1]')
expect_equal(match_params('alpha', params_raw),'alpha')
expect_equal(match_params('beta', params_raw), c('beta[1]','beta[2]'))
expect_equal(match_params('gamma[1,1]', params_raw), 'gamma[1,1]')
expect_true(is.null(match_params('fake',params_raw)))
expect_equal(match_params(c('alpha','beta'),params_raw),
               c('alpha','beta[1]','beta[2]'))
expect_equal(match_params(c('alpha','fake','beta'),params_raw),
               c('alpha','beta[1]','beta[2]'))

# test that order_samples works correctly--------------------------------------
order_samples <- jagsUI:::order_samples
samples <- readRDS('coda_samples.Rds')
new_order <- c('beta','mu','alpha')
out <- order_samples(samples, new_order)
expect_equal(class(out), 'mcmc.list')
expect_equal(length(out),length(samples))
expect_equal(lapply(out,class),lapply(samples,class))
expect_equal(param_names(out),c('beta',paste0('mu[',1:16,']'),'alpha', "deviance"))
expect_equal(dim(out[[1]]), c(30,19))
expect_equal(as.numeric(out[[1]][1,1:2]), 
               c(0.03690717, 59.78175), tol=1e-4)
expect_equal(order_samples(samples, 'beta'),
             order_samples(samples, c('beta','fake')))
expect_message(order_samples('fake','beta'))
expect_message(test <- order_samples('fake','beta'))
expect_equal(test, 'fake')
one_param <- samples[, 'alpha',drop=FALSE]
expect_equal(order_samples(one_param,'alpha'),one_param)
expect_equal(dim(order_samples(one_param, 'beta')[[1]]),c(30,0))
new_order <- c('deviance', 'beta','mu','alpha')
out <- order_samples(samples, new_order)
expect_equal(param_names(out),c('deviance', 'beta',paste0('mu[',1:16,']'),'alpha'))

# test that which_params gets param col indices--------------------------------
which_params <- jagsUI:::which_params
params_raw <- c('alpha','beta[1]','beta[2]','gamma[1,1]','gamma[3,1]')
expect_equal(which_params('alpha',params_raw),1)
expect_equal(which_params('beta',params_raw),c(2,3))
expect_equal(which_params('gamma',params_raw),c(4,5))
expect_null(which_params('kappa',params_raw))


# test that mcmc_to_mat converts properly--------------------------------------
mcmc_to_mat <- jagsUI:::mcmc_to_mat
samples <- readRDS('coda_samples.Rds')
mat <- mcmc_to_mat(samples[, 'alpha'])
expect_true(inherits(mat, 'matrix'))
expect_equal(dim(mat),c(nrow(samples[[1]]),length(samples)))
expect_equal(mat[,1],as.numeric(samples[[1]][,'alpha'])) 
one_sample <- readRDS('one_sample.Rds')
mat <- mcmc_to_mat(one_sample[, 'alpha'])
expect_equal(dim(mat), c(1,3))

# test that get_inds extracts indices-----------------------------------------
get_inds <- jagsUI:::get_inds
params_raw <- c('beta[1]','beta[2]')  
expect_equal(get_inds('beta',params_raw),matrix(c(1,2)))
params_raw <- c('gamma[1,1]','gamma[2,1]','gamma[1,3]')
expect_equal(get_inds('gamma',params_raw),
               matrix(c(1,1,2,1,1,3),ncol=2,byrow=T))
params_raw <- c('kappa[1,1,1]','kappa[2,1,1]','kappa[1,3,1]')
expect_equal(get_inds('kappa',params_raw),
               matrix(c(1,1,1,2,1,1,1,3,1),ncol=3,byrow=T))
params_raw <- 'alpha'
expect_warning(test <- get_inds('alpha',params_raw)[1,1])
expect_true(is.na(test))

# test that bind.mcmc works correctly------------------------------------------
cs1 <- readRDS('coda_samples.Rds')
cs2 <- readRDS('coda_samples.Rds')

iter_increment <- coda::niter(cs1) * coda::thin(cs1)
test <- jagsUI:::bind.mcmc(cs1, cs2, stats::start(cs1), iter_increment)
expect_equal(coda::niter(test), 60)
expect_equal(coda::varnames(test), coda::varnames(cs1))
expect_equal(stats::start(cs1), stats::start(test))
expect_equal(stats::end(test), stats::end(cs1) + iter_increment)

comb <- rbind(
  rbind(as.matrix(cs1[[1]]), as.matrix(cs2[[1]])),
  rbind(as.matrix(cs1[[2]]), as.matrix(cs2[[2]])),
  rbind(as.matrix(cs1[[3]]), as.matrix(cs2[[3]]))
)
expect_equal(comb, as.matrix(test))
