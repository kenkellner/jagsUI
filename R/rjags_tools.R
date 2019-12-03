#Functions for interfacing with rjags

#------------------------------------------------------------------------------
#Load requested JAGS modules
#basemod and bugs are always loaded, and glm is by default
load_modules <- function(modules){ 
  current_mods <- rjags::list.modules() 
  if(length(current_mods > 0)){
    sapply(current_mods,rjags::unload.module, quiet=T)
  }
  mods_to_load <- c('basemod','bugs',modules)
  sapply(mods_to_load,rjags::load.module, quiet=T)
  invisible()
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Load requested JAGS factories
load_factories <- function(factories){ 
  
  if(is.null(factories)) return(invisible())
  
  set_factory <- function(factory){
    spl <- strsplit(factory,'\\s')[[1]]
    if(length(spl)!=3){ 
      stop(paste0("Incorrect input format for factory '",factory,"', see help"))
    }
    avail <- as.character(rjags::list.factories(spl[2])[,1])
    if(spl[1] %in% avail){
      null <- rjags::set.factory(spl[1],spl[2],spl[3])
    } else {
      stop(paste('Factory',spl[1],'is not available. Check loaded modules.'))
    }  
  }

  sapply(factories, set_factory)
  invisible()
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Divide a series of iterations into approx equal-sized chunks
get_chunks <- function(size){
  if(size<50) return(size)
  chunk_size <- floor(size/10)
  chunks <- rep(chunk_size, 10)
  chunks[1] <- chunks[1] + size %% 10
  chunks
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Estimate time remaining in JAGS run
time_left <- function(time_start, iter_done, iter_total, time_end=Sys.time()){
  t_elapsed <- as.numeric(difftime(time_end,time_start,units='s'))
  t_total <- t_elapsed / (iter_done/iter_total)
  t_left <- t_total - t_elapsed
  if(t_left <= 60 ) {
    return( paste(round(t_left),'sec left') ) 
  } else if (t_left <= (60*60)){
    return( paste(round(t_left/60, 1),'min left') )
  }
  paste(round(t_left/(60*60), 1),'hr left')
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Initialize model
init_model <- function(model_file, data, inits=NULL, n_chains){  
  utils::capture.output(
    out <- rjags::jags.model(file=model_file, data=data, inits=inits, 
                     n.chains=n_chains, n.adapt=0)
    )
  out
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Recompile model
#Force this even when it's not necessary, so that 
#parallel and non-parallel output match
#(models always have to be recompiled in parallel)
recompile_model <- function(model_object){
  utils::capture.output( model_object$recompile() )
  TRUE
}

#Old version that only recompiles when necessary
#recompile_model <- function(model_object){  
  #if(identical(model_object$ptr(), methods::new('externalptr'))){
    #utils::capture.output(model_object$recompile())
    #return(TRUE)
  #}
  #FALSE
#}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Adapt model
#TODO: maybe sophisticated iterative adapt check
adapt_model <- function(model_object, n_iter){
  recompile_model(model_object)
  rjags::adapt(object=model_object, n.iter=n_iter, end.adaptation=TRUE)
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Update model without saving iterations
#TODO: run in chunks?
update_model <- function(model_object, n_iter){
  if(n_iter==0) return(FALSE)
  #recompile_model(model_object)
  stats::update(model_object, n.iter=n_iter)
}
#------------------------------------------------------------------------------

#Sample from model posterior
#TODO: tests
sample_model <- function(model_object, params, n_iter, n_thin){
  #recompile_model(model_object)
  rjags::coda.samples(model_object, variable.names=params, n.iter=n_iter,
                      thin=n_thin, na.rm=FALSE)
}
