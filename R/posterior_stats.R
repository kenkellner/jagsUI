#Functions for calculating statistics from posterior samples

#Determine if 95% credible interval of parameter overlaps 0
overlap_0 <- function(lower, upper){
  as.numeric(!(lower <= 0) == (upper < 0))
}

#Calculate proportion of posterior with same sign as mean
calc_f <- function(mcmc_mat, mn){
  if(mn >= 0) return(mean(mcmc_mat>=0,na.rm=TRUE))
  mean(mcmc_mat<0, na.rm=TRUE)
}

#Calculate series of statistics for one parameter
#Takes a matrix as input, n_iter * n_chains
calc_param_stats <- function(mcmc_mat){
  
  fallback <- rep(NA,11)
  if(any(is.infinite(mcmc_mat)) | all(is.na(mcmc_mat))){
    return(fallback)
  }
  
  #Handle any unexpected errors during calculation
  tryCatch({
    mn <- mean(mcmc_mat, na.rm=T)
    quants <- quantile(mcmc_mat, c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=TRUE)
    c(mn, stats::sd(mcmc_mat,na.rm=TRUE), quants, Rhat_min(mcmc_mat), 
    ess_bulk(mcmc_mat), overlap_0(quants[1], quants[5]),
    calc_f(mcmc_mat, mn))
  }, error = function(e) {
    cat(paste0('Caught error when calculating stats:\n',e,'\n'))
    fallback
  })
}

#Calculate statistics for all parameters in posterior and organize into matrix
#Takes mcmc.list as input
calc_stats <- function(samples){
  out <- sapply(param_names(samples), 
                function(x) calc_param_stats(mcmc_to_mat(samples, x)))
  rownames(out) <- c('mean','sd','q2.5','q25','q50','q75','q97.5','Rhat',
                  'n.eff','overlap0','f')
  t(out)
}
