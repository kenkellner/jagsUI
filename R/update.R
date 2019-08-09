#------------------------------------------------------------------------------
#Update method for regular jagsUI-class objects
update.jagsUI <- function(object, parameters.to.save=NULL, n.adapt=1000, 
                          n.iter, n.thin=NULL, no.stats=NULL, 
                          quiet=FALSE, ...){ 
  
  #Process input
  inp <- get_update_input(object, parameters.to.save, n.adapt, n.iter, n.thin)
  
  #Run JAGS
  out <- run_model(inp, quiet)
 
  #Process output
  stats <- process_output(out$samples, exclude_params=no.stats)
  
  #Build jagsUI object
  out <- c(stats, out)
  class(out) <- 'jagsUI'
  out 
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Update method for jagsUIbasic-class objects
update.jagsUIbasic <- function(object, parameters.to.save=NULL, n.adapt=1000,
                               n.iter, n.thin=NULL, quiet=FALSE, ...){

  #Process input
  inp <- get_update_input(object, parameters.to.save, n.adapt, n.iter, n.thin)
  
  #Run JAGS
  out <- run_model(inp, quiet)

  out <- out[c('samples','model','parameters','modfile',
               'mcmc.info','run.info')]

  class(out) <- 'jagsUIbasic'
  out
}
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Utility function for compiling run info for updates
get_update_input <- function(object, params, n.adapt, n.iter, n.thin){
  
  #Set up input
  inp <- object[c("parameters", "modfile", "mcmc.info", "run.info", "model")]
  
  if(!is.null(params)){
    DIC <- 'deviance' %in% param_names(object$samples)
    inp$parameters <- check_params(params, DIC)
  }
  
  inp$mcmc.info$n.adapt <- n.adapt
  inp$mcmc.info$n.burnin <- 0
  inp$mcmc.info$n.iter <- n.iter
  if(!is.null(n.thin)) inp$mcmc.info$n.thin <- n.thin

  inp
}
#------------------------------------------------------------------------------
