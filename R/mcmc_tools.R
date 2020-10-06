#Functions for manipulating and extracting info from mcmc.list-class objects
#from package rjags/coda

# This is a subset of the functions in mcmc_tools in devel version 1.5.1.9024

###------------------------------------------------------------------------------
#Remove brackets and indices from parameter names in mcmc.list
strip_params <- function(params_raw, unique=FALSE){
  params_strip <- sapply(strsplit(params_raw,'[', fixed=T),'[',1)
  if(unique) return( unique(params_strip) )
  params_strip
}
#------------------------------------------------------------------------------

###------------------------------------------------------------------------------
#Identify which columns in mcmc.list object correspond to a given
#parameter name (useful for non-scalar parameters)
which_params <- function(param, params_raw){
  params_strip <- strip_params(params_raw)
  if( ! param %in% params_strip ){
    return(NULL)
  }
  which(params_strip == param)
}
#------------------------------------------------------------------------------

###------------------------------------------------------------------------------
#Get names of parameters from an mcmc.list
#If simplify=T, also drop brackets/indices
param_names <- function(mcmc_list, simplify=FALSE){
  raw <- colnames(mcmc_list[[1]])
  if(!simplify) return(raw)
  strip_params(raw, unique=T)
}
#------------------------------------------------------------------------------

###------------------------------------------------------------------------------
#Match parameter name to scalar or array versions of parameter name
match_params <- function(params, params_raw){
  unlist(lapply(params, function(x){
    if(x %in% params_raw) return(x)
    if(!x %in% strip_params(params_raw)) return(NULL)
    params_raw[which_params(x, params_raw)]
    }))
}
#------------------------------------------------------------------------------

###------------------------------------------------------------------------------
#Subset cols of mcmc.list (simple version of [.mcmc.list method)
select_cols <- function(mcmc_list, col_inds){
  out <- lapply(1:length(mcmc_list), FUN=function(x){
            mcmc_element <- mcmc_list[[x]][,col_inds,drop=FALSE]
            attr(mcmc_element,'mcpar') <- attr(mcmc_list[[x]], 'mcpar')
            class(mcmc_element) <- 'mcmc'
            mcmc_element
            })
  class(out) <- 'mcmc.list'
  out
}
#------------------------------------------------------------------------------

###------------------------------------------------------------------------------
#Convert one parameter in mcmc.list to matrix, n_iter * n_chains
mcmc_to_mat <- function(samples, param){
  psamples <- select_cols(samples, param)
  n_chain <- length(samples)
  n_iter <- nrow(samples[[1]])
  matrix(unlist(psamples), nrow=n_iter, ncol=n_chain)
}
#------------------------------------------------------------------------------
