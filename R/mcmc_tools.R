#Functions for manipulating and extracting info from mcmc.list-class objects
#from package rjags/coda

#------------------------------------------------------------------------------
#Extract index values inside brackets from a non-scalar parameter
#param is the "base" name of the parameter and params_raw is a vector of 
#strings that contain brackets
get_inds <- function(param, params_raw){
  inds_raw <- sub(paste(param,'[',sep=''),'', params_raw,fixed=T)
  inds_raw <- sub(']','', inds_raw, fixed=T)
  inds_raw <- strsplit(inds_raw,',',fixed=T)
  inds <- as.integer(unlist(inds_raw))
  matrix(inds, byrow=T, ncol=length(inds_raw[[1]]))
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Remove brackets and indices from parameter names in mcmc.list
strip_params <- function(params_raw, unique=FALSE){
  params_strip <- sapply(strsplit(params_raw,'[', fixed=T),'[',1)
  if(unique) return( unique(params_strip) )
  params_strip
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
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

#------------------------------------------------------------------------------
#Get names of parameters from an mcmc.list
#If simplify=T, also drop brackets/indices
param_names <- function(mcmc_list, simplify=FALSE){
  raw <- colnames(mcmc_list[[1]])
  if(!simplify) return(raw)
  strip_params(raw, unique=T)
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Match parameter name to scalar or array versions of parameter name
match_params <- function(params, params_raw){
  unlist(lapply(params, function(x){
    if(x %in% params_raw) return(x)
    if(!x %in% strip_params(params_raw)) return(NULL)
    params_raw[which_params(x, params_raw)]
    }))
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Reorder output samples from coda to match input parameter order
order_samples <- function(samples, params){
  tryCatch({
    matched <- match_params(params, param_names(samples))
    select_cols(samples, matched)
  }, error = function(e){ 
    message(paste0("Caught error re-ordering samples:\n",e,"\n"))
    samples
  })
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
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

#------------------------------------------------------------------------------
#Remove parameters from list of params
remove_params <- function(samples, exclude=NULL){
  all_params <- param_names(samples)
  if(is.null(exclude)) return(all_params)
  params_strip <- strip_params(all_params)
  ind <- unlist(sapply(exclude, which_params, all_params))
  all_params[-ind]
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Convert one parameter in mcmc.list to matrix, n_iter * n_chains
mcmc_to_mat <- function(samples, param){
  psamples <- select_cols(samples, param)
  n_chain <- length(samples)
  n_iter <- nrow(samples[[1]])
  matrix(unlist(psamples), nrow=n_iter, ncol=n_chain)
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Combine two mcmc.lists into one; should have same parameters saved
#TODO: manage start/stop iterations (doesn't work properly with update)
comb_mcmc_list <- function(x,y){
  x_mcpar <- attr(x[[1]],"mcpar")
  y_mcpar <- attr(y[[1]],"mcpar")

  out <- lapply(Map(rbind,x,y), function(z){
                  attr(z,"mcpar") <- c(x_mcpar[1],y_mcpar[2],x_mcpar[3])
                  class(z) <- "mcmc"
                  z 
              })

  class(out) <- "mcmc.list"
  out
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Check that a parameter exists in the output
check_parameter <- function(parameter, samples){
  if(! parameter %in% param_names(samples, simplify=T)) 
    stop(paste0('Parameter ',parameter,' not found in output'))
} 
#------------------------------------------------------------------------------
