#------------------------------------------------------------------------------
#Get names of parameters from an mcmc.list
#If simplify=T, also drop brackets/indices
param_names <- function(mcmc_list, simplify=FALSE){
  out <- coda::varnames(mcmc_list)
  if(simplify) out <- strip_params(out, unique=TRUE)
  out
}


#------------------------------------------------------------------------------
strip_params <- function(params_raw, unique=FALSE){
  params_strip <- sapply(strsplit(params_raw,'[', fixed=TRUE),'[',1)
  if(unique) return( unique(params_strip) )
  params_strip
}


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
#Reorder output samples from coda to match input parameter order
order_samples <- function(samples, params){
  tryCatch({
    matched <- match_params(params, param_names(samples))
    if("deviance" %in% param_names(samples) & ! "deviance" %in% matched){
      matched <- c(matched, "deviance")
    }
    samples[,matched,drop=FALSE]
  }, error = function(e){ 
    message(paste0("Caught error re-ordering samples:\n",e,"\n"))
    samples
  })
}


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
mcmc_to_mat <- function(mcmc_list){
  stopifnot(coda::nvar(mcmc_list) == 1)
  matrix(unlist(mcmc_list), 
         nrow=coda::niter(mcmc_list), ncol=coda::nchain(mcmc_list))
}

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
# Check if mcmc.list has only one parameter (one column)
has_one_parameter <- function(mcmc_list){
  coda::nvar(mcmc_list) == 1
}


#------------------------------------------------------------------------------
# Bind two mcmc.lists together
bind.mcmc <- function(mcmc.list1,mcmc.list2,start,n.new.iter){
  
  nchains <- length(mcmc.list1)
  
  samples <- list()
  
  for (i in 1:nchains){
    
    d <- rbind(mcmc.list1[[i]],mcmc.list2[[i]])
    
    samples[[i]] <- coda::mcmc(data=d,start=start,
                               end=(stats::end(mcmc.list1[[i]])+n.new.iter),
                               thin=coda::thin(mcmc.list1[i]))
    
  }
  
  return(coda::as.mcmc.list(samples))
  
  
}
