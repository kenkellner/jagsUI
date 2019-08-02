

jagsUI <- jags <- function(data,inits=NULL,parameters.to.save,model.file,n.chains,n.adapt=NULL,n.iter,n.burnin=0,n.thin=1,
                       modules=c('glm'),factories=NULL,parallel=FALSE,n.cores=NULL,DIC=TRUE,store.data=FALSE,codaOnly=NULL,
                       bugs.format=FALSE,verbose=TRUE){
  
  #Process inputs
  inp <- process_input(data, inits, parameters.to.save, n.chains,
                              n.adapt, n.iter, n.burnin, n.thin, n.cores,
                              DIC, parallel)

  #Save start time
  start.time <- Sys.time()
  
  #Stuff to do if parallel=TRUE
  if(parallel && n.chains>1){
 
  par <- run.parallel(inp$data,inp$inits,inp$params,model.file,n.chains,n.adapt,n.iter,n.burnin,n.thin,
                      modules=modules,factories=factories,DIC=DIC,verbose=verbose,n.cores=inp$mcmc_info$n.cores) 
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
  
  rjags.output <- run.model(model.file,inp$data,inp$inits,inp$params,n.chains,n.iter,n.burnin,n.thin,n.adapt,verbose=verbose)
  samples <- rjags.output$samples
  m <- rjags.output$m
  total.adapt <- rjags.output$total.adapt
  sufficient.adapt <- rjags.output$sufficient.adapt
  
  ##########################
  ##End of rjags functions##
  ##########################
  
  }
  
  #Get more info about MCMC run
  end.time <- Sys.time() 
  time <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
  date <- start.time
  
  #Combine mcmc info into list
  n.samples <- dim(samples[[1]])[1] * n.chains
  end.values <- samples[(n.samples/n.chains),]
  mcmc.info <- list(n.chains,n.adapt=total.adapt,sufficient.adapt,n.iter,n.burnin,n.thin,n.samples,end.values,time)
  names(mcmc.info) <- c('n.chains','n.adapt','sufficient.adapt','n.iter','n.burnin','n.thin','n.samples','end.values','elapsed.mins')
  if(parallel){mcmc.info$n.cores <- n.cores}
  
  #Reorganize JAGS output to match input parameter order
  if(dim(samples[[1]])[2]>1){
    samples <- order.params(samples,inp$params,DIC,verbose=verbose)
  }
  
  #Process output
  output <- process_output(samples, exclude_params=codaOnly)

  output$samples <- samples
  output$modfile <- model.file
  #If user wants to save input data/inits
  if(store.data){
    output$inits <- inp$inits
    output$data <- inp$data
  } 
  output$model <- m
  output$parameters <- inp$params
  output$mcmc.info <- mcmc.info
  output$run.date <- date
  output$parallel <- parallel
  output$bugs.format <- bugs.format
  output$calc.DIC <- DIC
  
  #Classify final output object
  class(output) <- 'jagsUI'
  
  return(output)
  
}
