
autojags <- function(data,inits=NULL,parameters.to.save,model.file,n.chains,n.adapt=100,iter.increment=1000,n.burnin=0,n.thin=1,
                    modules=c('glm'),parallel=FALSE,DIC=TRUE,store.data=FALSE,codaOnly=FALSE,seed=floor(runif(1,1,10000)),
                    bugs.format=FALSE,Rhat.limit=1.1,max.iter=100000){
  
  if(n.chains<2){stop('Number of chains must be >2.')}
  
  #Set random seed
  RNGkind('default')
  set.seed(seed)
  
  #Pass input data and parameter list through error check / processing
  data.check <- process.input(data,parameters.to.save,inits,n.chains,iter.increment,n.burnin,n.thin,DIC=DIC)
  data <- data.check$data
  parameters.to.save <- data.check$params
  inits <- data.check$inits
  
  #Save start time
  start.time <- Sys.time()
  
  #Initial model run
  
  #Parallel
  
  cat('Iteration 1',' (',iter.increment,')',sep="")
  
  if(parallel){
    
    par <- run.parallel(data,inits,parameters.to.save,model.file,n.chains,n.adapt,n.iter=iter.increment,n.burnin,n.thin,
                        modules,seed,DIC,verbose=FALSE) 
    samples <- par$samples
    mod <- par$model
    
  } else {
    
  #Not parallel  
    
    set.modules(modules,DIC)
    
    rjags.output <- run.model(model.file,data,inits,parameters.to.save,n.chains,n.iter=iter.increment,n.burnin,n.thin,n.adapt,
                              verbose=FALSE)
    samples <- rjags.output$samples
    mod <- rjags.output$m
    
  }
  
  #Combine mcmc info into list
  n.samples <- (iter.increment-n.burnin) / n.thin * n.chains
  mcmc.info <- list(n.chains,n.adapt,n.iter=iter.increment,n.burnin,n.thin,n.samples,time)
  names(mcmc.info) <- c('n.chains','n.adapt','n.iter','n.burnin','n.thin','n.samples','elapsed.mins')
  
  test <- test.Rhat(samples,Rhat.limit)
  reach.max <- FALSE
  index = 1
  
  while(test==TRUE && reach.max==FALSE){
    
    index <- index + 1
    cat('Iteration ',index,' (',mcmc.info$n.iter + iter.increment,')',sep="")
    
    if(parallel){
      
      par <- run.parallel(data=NULL,inits=NULL,parameters.to.save=parameters.to.save,model.file=NULL,n.chains=n.chains
                          ,n.adapt=n.adapt,n.iter=iter.increment,n.burnin=0,n.thin=n.thin,modules=modules,
                          seed=seed,DIC=DIC,model.object=mod,update=TRUE,verbose=FALSE) 
      samples <- par$samples
      mod <- par$model
      test <- test.Rhat(samples,Rhat.limit)
      
    } else {
      
      set.modules(modules,DIC)
      
      rjags.output <- run.model(model.file=NULL,data=NULL,inits=NULL,parameters.to.save=parameters.to.save,
                                n.chains=n.chains,n.iter=iter.increment,n.burnin=0,n.thin,n.adapt,
                                model.object=mod,update=TRUE,verbose=FALSE)
      samples <- rjags.output$samples
      mod <- rjags.output$m
      test <- test.Rhat(samples,Rhat.limit)
     
    }
    mcmc.info$n.burnin <- mcmc.info$n.iter
    mcmc.info$n.iter <- mcmc.info$n.iter + iter.increment
    mcmc.info$n.samples <- (iter.increment) / n.thin * n.chains
    
    if(mcmc.info$n.iter>=max.iter){
      reach.max <- TRUE
      cat('\nMaximum iterations reached.\n\n')
    }
  }
  
  #Get more info about MCMC run
  end.time <- Sys.time() 
  mcmc.info$elapsed.mins <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
  date <- start.time
  
  #Reorganize JAGS output to match input parameter order
  samples <- order.params(samples,parameters.to.save,DIC)
  
  #Convert rjags output to jagsUI form 
  output <- process.output(samples,DIC=DIC,codaOnly)
  
  #Add additional information to output list
  
  #Summary
  output$summary <- summary.matrix(output,samples,n.chains,codaOnly)
  
  output$samples <- samples
  output$modfile <- model.file
  #If user wants to save input data/inits
  if(store.data){
    output$inits <- inits
    output$data <- data
  } 
  output$model <- mod
  output$parameters <- parameters.to.save
  output$mcmc.info <- mcmc.info
  output$run.date <- date
  output$random.seed <- seed
  output$parallel <- parallel
  output$bugs.format <- bugs.format
  
  #Classify final output object
  class(output) <- 'jagsUI'
  
  return(output)
  
  
  
  
  
  
  
  
}
  