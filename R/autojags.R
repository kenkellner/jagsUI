
autojags <- function(data,inits=NULL,parameters.to.save,model.file,n.chains,n.adapt=NULL,iter.increment=1000,n.burnin=0,n.thin=1,
                     save.all.iter=FALSE,modules=c('glm'),factories=NULL,parallel=FALSE,n.cores=NULL,DIC=TRUE,store.data=FALSE,codaOnly=NULL,
                    bugs.format=FALSE,Rhat.limit=1.1,max.iter=100000,verbose=TRUE){
    
  inp <- process_input(data, inits, parameters.to.save, model.file, n.chains,
                              n.adapt, 
                              iter.increment + n.burnin, #fix this
                              n.burnin, n.thin, modules, factories, parallel,
                              n.cores, DIC)

  #Save start time
  start.time <- Sys.time()
  
  #Note if saving all iterations
  if(save.all.iter&verbose){cat('Note: ALL iterations will be included in final posterior.\n\n')}
  
  #Initial model run
  
  #Parallel
  
  if(verbose){cat('Burn-in + Update 1',' (',(n.burnin + iter.increment),')\n',sep="")}
  
  if(parallel){
    
    par <- run.parallel(inp$data,inp$inits,inp$parameters,model.file,n.chains,n.adapt,n.iter=(n.burnin + iter.increment),n.burnin,n.thin,
                        modules=modules,factories=factories,DIC=DIC,verbose=FALSE,n.cores=inp$run.info$n.cores) 
    samples <- par$samples
    mod <- par$model
    total.adapt <- par$total.adapt
    sufficient.adapt <- par$sufficient.adapt
    
  } else {
    
  #Not parallel  
    
    set.modules(modules,DIC)
    set.factories(factories)
    
    rjags.output <- run.model(model.file,inp$data,inp$inits,inp$parameters,n.chains,n.iter=(n.burnin + iter.increment),n.burnin,n.thin,n.adapt,
                              verbose=FALSE)
    samples <- rjags.output$samples
    mod <- rjags.output$m
    total.adapt <- rjags.output$total.adapt
    sufficient.adapt <- rjags.output$sufficient.adapt
    
  }
  
  #Combine mcmc info into list
  n.samples <- dim(samples[[1]])[1] * n.chains
  mcmc.info <- list(n.chains,n.adapt=total.adapt,sufficient.adapt=sufficient.adapt,n.iter=(n.burnin + iter.increment),n.burnin,n.thin,n.samples,time)
  names(mcmc.info) <- c('n.chains','n.adapt','sufficient.adapt','n.iter','n.burnin','n.thin','n.samples','elapsed.mins')
  if(parallel){mcmc.info$n.cores <- inp$run.info$n.cores}
  
  #test <- test.Rhat(samples,Rhat.limit,codaOnly,verbose=verbose)
  test <- test_Rhat(samples, Rhat.limit)
  reach.max <- FALSE
  index = 1
  
  if(mcmc.info$n.iter>=max.iter){
    reach.max <- TRUE
    if(verbose){cat('\nMaximum iterations reached.\n\n')}
  }
  
  while(test$result==TRUE && reach.max==FALSE){
        
    index <- index + 1
    if(verbose){cat('Update ',index,' (',mcmc.info$n.iter + iter.increment,')\n',sep="")}
    
    if(save.all.iter){
      if(index==2){start.iter <- start(samples)}
      if (index > 1) {
        old.samples <- samples
      }
    }
       
    if(parallel){
      
      par <- run.parallel(data=NULL,inits=NULL,parameters.to.save=inp$parameters,model.file=NULL,n.chains=n.chains
                          ,n.adapt=n.adapt,n.iter=iter.increment,n.burnin=0,n.thin=n.thin,modules=modules,
                          factories=factories,DIC=DIC,model.object=mod,update=TRUE,verbose=FALSE,n.cores=inp$run.info$n.cores) 
      
      if(save.all.iter & index > 1){
        samples <- comb_mcmc_list(old.samples, par$samples)
      } else {samples <- par$samples}
      
      mod <- par$model
      sufficient.adapt <- par$sufficient.adapt
      
      test <- test_Rhat(samples, Rhat.limit)
      #test <- test.Rhat(samples,Rhat.limit,codaOnly)
      
    } else {
      
      set.modules(modules,DIC)
      
      rjags.output <- run.model(model.file=NULL,data=NULL,inits=NULL,parameters.to.save=inp$parameters,
                                n.chains=n.chains,n.iter=iter.increment,n.burnin=0,n.thin,n.adapt=n.adapt,
                                model.object=mod,update=TRUE,verbose=FALSE)
      
      if(save.all.iter & index > 1){
        samples <- comb_mcmc_list(old.samples, rjags.output$samples)
      } else {samples <- rjags.output$samples}

      mod <- rjags.output$m
      sufficient.adapt <- rjags.output$sufficient.adapt

      test <- test_Rhat(samples, Rhat.limit)
      #test <- test.Rhat(samples,Rhat.limit,codaOnly)

     
    }
    
    if(!save.all.iter){mcmc.info$n.burnin <- mcmc.info$n.iter}   
    mcmc.info$n.iter <- mcmc.info$n.iter + iter.increment    
    mcmc.info$n.samples <- dim(samples[[1]])[1] * n.chains
    mcmc.info$sufficient.adapt <- sufficient.adapt
    
    if(mcmc.info$n.iter>=max.iter){
      reach.max <- TRUE
      if(verbose){cat('\nMaximum iterations reached.\n\n')}
    }
  }
  
  #Get more info about MCMC run
  end.time <- Sys.time() 
  mcmc.info$elapsed.mins <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
  date <- start.time
  
  #Reorganize JAGS output to match input parameter order
  samples <- order_samples(samples,inp$parameters)
  
  #Convert rjags output to jagsUI form
  output <- process_output(samples, exclude_params = codaOnly)

  output$samples <- samples
  output$modfile <- model.file
  #If user wants to save input data/inits
  if(store.data){
    output$inits <- inp$inits
    output$data <- inp$data
  } 
  output$model <- mod
  output$parameters <- inp$parameters
  output$mcmc.info <- mcmc.info
  output$run.date <- date
  output$parallel <- parallel
  output$bugs.format <- bugs.format
  output$calc.DIC <- DIC
  
  #Classify final output object
  class(output) <- 'jagsUI'
  
  return(output)
  
  
  
  
  
  
  
  
}
  
