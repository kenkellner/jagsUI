#------------------------------------------------------------------------------
#Process output master function
#To generate backwards-compatible jagsUI output
#WIP
process_output <- function(samples, exclude_params=NULL){
  tryCatch({ 
    sum_list <- list(summary=calc_stats(samples, exclude_params))
    stat_list <- all_stat_arrays(sum_list$summary)
    sims.list <- list(sims.list=sims_list(samples, exclude_params))
    dic_list <- calc_DIC(samples)

    c(sims.list, stat_list, dic_list, sum_list)
  }, error = function(e) {
    message(paste0("Processing output failed with this error:\n",e,"\n"))
    list(sims.list=NA, pD=NA, DIC=NA, summary=NA)
  })
}

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Fill an array from vector using matching array indices
fill_array <- function(data_vector, indices){
  out <- array(NA, dim=apply(indices,2,max))
  out[indices] <- data_vector
  out
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Extract the posterior of a parameter and organize it into an array
get_posterior_array <- function(parameter, samples){
  
  tryCatch({
    #Subset output columns matching parameter
    col_inds <- which_params(parameter, param_names(samples))
    posterior_raw <- do.call(rbind, select_cols(samples, col_inds))
  
    #If parameter is scalar, return it now
    if( ncol(posterior_raw) == 1 ){ return(as.vector(posterior_raw)) }

    #If parameter is array, get indices
    ind_raw <- get_inds(parameter, colnames(posterior_raw))
    nsamples <- nrow(posterior_raw)
    ind_array <- cbind(1:nsamples, ind_raw[rep(1:nrow(ind_raw), each=nsamples),])

    #Create, fill, return output object
    fill_array(as.vector(posterior_raw), ind_array)
  }, error = function(e) {
    message(paste0("Caught error when creating sims.list array for '",
                   parameter,"':\n",e,"\n"))
    NA
  })
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Get sims list
sims_list <- function(samples, exclude=NULL){
  params <- remove_params(samples, exclude)
  sapply(strip_params(params, unique=TRUE), get_posterior_array, 
                      samples, simplify=FALSE) 
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Extract stats for a parameter and organize into appropriately-sized array
get_stat_array <- function(parameter, stat, model_summary){
  
  tryCatch({
    #Subset vector of stats for parameter
    row_ind <- which_params(parameter, rownames(model_summary))
    stat_vector <- model_summary[row_ind, stat]

    #If parameter is scalar, return it now
    if( length(stat_vector) == 1 ){ return(stat_vector) }

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

#------------------------------------------------------------------------------
#Compile all stats for all parameters into list of lists
all_stat_arrays <- function(summary_stats){

  stat_array_list <- function(stat, summary_stats){
    params <- strip_params(rownames(summary_stats),unique=TRUE)
    sapply(params, get_stat_array, stat, summary_stats, simplify=FALSE)
  }
  sapply(colnames(summary_stats), stat_array_list, summary_stats,
                   simplify=FALSE)

}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Check if stat is in model summary
check_stat <- function(stat, model_summary){
  if( ! stat %in% colnames(model_summary) ){
    stop(paste0('Invalid stat "',stat,'"'))
  }
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Check that an object is the right class
check_class <- function(output){
  if(class(output) != "jagsUI"){ stop("Requires jagsUI object") }
}
#------------------------------------------------------------------------------
