
jags.basic <- function(data,inits=NULL,parameters.to.save,model.file,n.chains,n.adapt=NULL,n.iter,n.burnin=0,n.thin=1,
                           modules=c('glm'),factories=NULL,parallel=FALSE,n.cores=NULL,DIC=TRUE,seed=NULL,save.model=FALSE,verbose=TRUE){
  
  if(!is.null(seed)){
    stop("The seed argument is no longer supported, use set.seed() instead", call.=FALSE)
  }

  # Check input data
  inps_check <- process_input(data=data, params=parameters.to.save, inits=inits,
                              n_chains=n.chains, n_adapt=n.adapt, n_iter=n.iter, 
                              n_burnin=n.burnin, n_thin=n.thin, n_cores=n.cores,
                              DIC=DIC, quiet=!verbose, parallel=parallel)
  data <- inps_check$data
  parameters.to.save <- inps_check$params
  inits <- inps_check$inits
  mcmc.info <- inps_check$mcmc.info
  if(parallel) n.cores <- inps_check$mcmc.info$n.cores
  
  #Save start time
  start.time <- Sys.time()
  
  #Stuff to do if parallel=TRUE
  if(parallel && n.chains>1){
    
    par <- run.parallel(data,inits,parameters.to.save,model.file,n.chains,n.adapt,n.iter,n.burnin,n.thin,
                        modules=modules,factories=factories,DIC=DIC,verbose=verbose,n.cores=n.cores) 
    samples <- par$samples
    m <- par$model
    total.adapt <- par$total.adapt
    sufficient.adapt <- par$sufficient.adapt
    if(any(!sufficient.adapt)&verbose){warning("JAGS reports adaptation was incomplete. Consider increasing n.adapt")}
    
  } else {
    
    #######################
    ##Run rjags functions##
    #######################
    
    #Set modules
    set.modules(modules,DIC)
    set.factories(factories)
    
    rjags.output <- run.model(model.file,data,inits,parameters.to.save,n.chains,n.iter,n.burnin,n.thin,n.adapt,
                              verbose=verbose)
    samples <- rjags.output$samples
    m <- rjags.output$m
    total.adapt <- rjags.output$total.adapt
    sufficient.adapt <- rjags.output$sufficient.adapt
    
    ##########################
    ##End of rjags functions##
    ##########################
    
  }
  
  #Get more info about MCMC run
  time <- round(as.numeric(Sys.time()-start.time,units="mins"),digits=3)
  if(verbose){cat('MCMC took',time,'minutes.\n')}
  
  if(save.model){
  output <- list()
  samples <- order_samples(samples, parameters.to.save)
  output$samples <- samples
  output$model <- m
  output$n.cores <- n.cores
  output$random.seed <- seed
  class(output) <- 'jagsUIbasic'
  } else {output <- samples}
 
  
  return(output)
  
}
