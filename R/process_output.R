#------------------------------------------------------------------------------
#Process output master function
#To generate backwards-compatible jagsUI output
process_output <- function(mcmc_list, coda_only=NULL, DIC, quiet=FALSE){
  if(!quiet){cat('Calculating statistics.......','\n')} 

  tryCatch({
    if(DIC == -999) stop("Throwing error for testing purposes", call.=FALSE)
    # Get the sims.list
    sims <- list(sims.list = sims_list(mcmc_list))
    # Calculate all stats
    stats <- calc_stats(mcmc_list, coda_only)
    # Convert them into stat arrays
    stats_list <- all_stat_arrays(stats, coda_only)
    # Get final summary table
    sum_list <- list(summary = stat_summary_table(stats, coda_only))
    # DIC stuff
    dic_list <- calc_DIC(mcmc_list, DIC)
    
    # Bind it all together
    if(!quiet){cat('\nDone.','\n')}
    c(sims, stats_list, dic_list, sum_list)
  }, error = function(e) {
    message(paste0("Processing output failed with this error:\n",e,"\n"))
    NULL
  })
}


#------------------------------------------------------------------------------
#Fill an array from vector using matching array indices
fill_array <- function(data_vector, indices){
  out <- array(NA, dim=apply(indices,2,max))
  out[indices] <- data_vector
  out
}


#------------------------------------------------------------------------------
#Extract the posterior of a parameter and organize it into an array
get_posterior_array <- function(parameter, samples){
  
  tryCatch({
    #Subset output columns matching parameter
    col_inds <- which_params(parameter, param_names(samples))
    posterior_raw <- do.call(rbind, samples[,col_inds,drop=FALSE])
  
    #If parameter is scalar, return it now
    if( ncol(posterior_raw) == 1 ){ return(as.vector(posterior_raw)) }

    #If parameter is array, get indices
    ind_raw <- get_inds(parameter, colnames(posterior_raw))
    ndraws <- nrow(posterior_raw)
    ind_array <- cbind(1:ndraws, ind_raw[rep(1:nrow(ind_raw), each=ndraws),])

    #Create, fill, return output object
    fill_array(as.vector(posterior_raw), ind_array)
  }, error = function(e) {
    message(paste0("Caught error when creating sims.list array for '",
                   parameter,"':\n",e,"\n"))
    NA
  })
}


#------------------------------------------------------------------------------
#Get sims list
sims_list <- function(samples){
  params <- param_names(samples)
  sapply(strip_params(params, unique=TRUE), get_posterior_array, 
                      samples, simplify=FALSE) 
}


#------------------------------------------------------------------------------
#Extract stats for a parameter and organize into appropriately-sized array
get_stat_array <- function(parameter, stat, model_summary){
  
  tryCatch({
    #Subset vector of stats for parameter
    row_ind <- which_params(parameter, rownames(model_summary))
    stat_vector <- model_summary[row_ind, stat]

    #If parameter is scalar, return it now
    if( length(stat_vector) == 1 ) return(stat_vector)

    #If parameter is array, get indices
    ind_array <- get_inds(parameter, names(stat_vector))

    #Create, fill, return output object
    fill_array(stat_vector, ind_array)
  }, error = function(e) {
    message(paste0("Caught error when creating stat array for '",
                   parameter,"':\n",e,"\n"))
    NA
  })
}


#------------------------------------------------------------------------------
#Compile all stats for all parameters into list of lists
all_stat_arrays <- function(summary_stats, coda_only){
  
  stat_array_list <- function(stat, summary_stats){
    params <- strip_params(rownames(summary_stats), unique=TRUE)
    sapply(params, function(x){
      # If the parameter is in coda_only and the stat is not the mean, return NA
      if(x %in% coda_only & stat != "mean") return(NA)
      # Otherwise return the stat array for that parameter and stat
      get_stat_array(x, stat, summary_stats)
    }, simplify=FALSE)
  }
  # Do this for all stats
  out <- sapply(colnames(summary_stats), stat_array_list, summary_stats,
                simplify=FALSE)
  
  # Convert overlap0 to logical to match old jagsUI code
  out$overlap0 <- lapply(out$overlap0, function(x) x == 1)
  out
}


#------------------------------------------------------------------------------
# Convert stats into summary table in original jagsUI format
# For backwards compatibility
stat_summary_table <- function(stats, coda_only){
  # Move overlap 0 and f to the end of the table
  stats <- stats[,c("mean", "sd", "q2.5", "q25", "q50", "q75", "q97.5",
                    "Rhat", "n.eff", "overlap0", "f"), drop=FALSE]
  # Rename the quantile columns
  colnames(stats)[3:7] <- c("2.5%", "25%", "50%", "75%", "97.5%")
  # Remove rows marked as coda_only
  keep_rows <- ! strip_params(rownames(stats)) %in% coda_only
  stats[keep_rows,,drop=FALSE]
}


#------------------------------------------------------------------------------
#Determine if 95% credible interval of parameter overlaps 0
overlap_0 <- function(lower, upper){
  as.numeric(!(lower <= 0) == (upper < 0))
}

#Calculate proportion of posterior with same sign as mean
calc_f <- function(values, mn){
  if(mn >= 0) return(mean(values>=0,na.rm=TRUE))
  mean(values<0, na.rm=TRUE)
}

calc_Rhat <- function(mcmc_list){
  stopifnot(has_one_parameter(mcmc_list))
  if(length(mcmc_list) == 1) return(NA)
  out <- try(coda::gelman.diag(mcmc_list, 
            autoburnin=FALSE, multivariate=FALSE)$psrf[1])
  if(inherits(out, "try-error") || !is.finite(out)) out <- NA
  out
}

mcmc_to_mat <- function(mcmc_list){
  stopifnot(has_one_parameter(mcmc_list))
  matrix(unlist(mcmc_list), 
         nrow=coda::niter(mcmc_list), ncol=coda::nchain(mcmc_list))
}

# Based on R2WinBUGS code
calc_neff <- function(mcmc_list){
  niter <- coda::niter(mcmc_list)
  nchain <- coda::nchain(mcmc_list)
  mcmc_mat <- mcmc_to_mat(mcmc_list)
  
  xdot <- apply(mcmc_mat, 2, mean, na.rm=TRUE)
  s2 <- apply(mcmc_mat, 2, stats::var, na.rm=TRUE)
  W <- mean(s2)

  #Non-degenerate case
  if(is.na(W)){
	  n_eff <- NA
  } else if ((W > 1.e-8) && (nchain > 1)) {
    B <- niter * stats::var(xdot)
    sig2hat <- ((niter-1)*W + B)/ niter
    n_eff <- round(nchain * niter * min(sig2hat/B,1),0)
  } else {
    #Degenerate case
    n_eff <- 1
  }
  n_eff
}

#Calculate series of statistics for one parameter
#Takes an mcmc.list as input
calc_param_stats <- function(mcmc_list, coda_only){
  stopifnot(has_one_parameter(mcmc_list))
  values <- unlist(mcmc_list)
  stat_names <- c('mean','sd','q2.5','q25','q50','q75','q97.5',
                  'overlap0','f','Rhat','n.eff')

  fallback <- sapply(stat_names, function(x) NA)
  if(any(is.infinite(values)) | all(is.na(values))){
    return(fallback)
  }

  #Handle any unexpected errors during calculation
  tryCatch({
    # If the parameter is in codaOnly, return only the mean
    mn <- mean(values, na.rm=TRUE)
    if(coda_only){
      fallback['mean'] <- mn
      return(fallback)
    }
    # Otherwise calculate all stats
    quants <- stats::quantile(values, c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=TRUE)
    out <- c(mn, 
             stats::sd(values,na.rm=TRUE), 
             quants, 
             overlap0 = overlap_0(quants[1], quants[5]),
             calc_f(values, mn),
             calc_Rhat(mcmc_list), 
             calc_neff(mcmc_list))
    names(out) <- stat_names
    out
  }, error = function(e) {
    message(paste0('Caught error when calculating stats:\n',e,'\n'))
    fallback
  })
}


#------------------------------------------------------------------------------
#Calculate statistics for all parameters in posterior and organize into matrix
#Takes mcmc.list as input
calc_stats <- function(mcmc_list, coda_only=NULL){
  params <- param_names(mcmc_list)
  coda_only <- strip_params(params) %in% coda_only 

  out <- sapply(1:length(params), function(i){
    calc_param_stats(mcmc_list[,i], coda_only[i])
  })
  colnames(out) <- params
  t(out)
}


#------------------------------------------------------------------------------
#Calculate pD and DIC from deviance if it exists in output samples
calc_DIC <- function(samples, DIC){ 
  if(!DIC | !("deviance" %in% param_names(samples))){
    return(NULL)
  }

  dev <- mcmc_to_mat(samples[,'deviance'])  
  #if(any(is.na(dev)) || any(is.infinite(dev))) return(c(pD=NA, DIC=NA))
  if(any(is.na(dev)) || any(is.infinite(dev))) return(NULL)

  pd <- apply(dev,2,FUN=function(x) stats::var(x)/2)
  dic <- apply(dev,2,mean) + pd
  
  c(pD=mean(pd),DIC=mean(dic))
}
