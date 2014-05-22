
jags.basic <- function(data,inits=NULL,parameters.to.save,model.file,n.chains,n.adapt=100,n.iter,n.burnin=0,n.thin=1,
                           modules=c('glm'),parallel=FALSE,DIC=TRUE,seed=floor(runif(1,1,10000)),save.model=FALSE){
  
  #Set random seed
  RNGkind('default')
  set.seed(seed)
  
  #Load/unload appropriate modules (besides dic)
  called.set <- c('basemod','bugs',modules)
  current.set <- list.modules()
  
  load.set <- called.set[!called.set%in%current.set]
  unload.set <- current.set[!current.set%in%called.set]
  
  if(length(load.set)>0){
    for (i in 1:length(load.set)){
      load.module(load.set[i],quiet=TRUE)
    }
  }
  if(length(unload.set)>0){
    for (i in 1:length(unload.set)){
      unload.module(unload.set[i],quiet=TRUE)
    }
  }
  
  #Pass input data and parameter list through error check / processing
  data.check <- process.input(data,parameters.to.save,inits,n.chains,DIC=DIC)
  data <- data.check$data
  parameters.to.save <- data.check$params
  inits <- data.check$inits
  
  #Save start time
  start.time <- Sys.time()
  
  #Stuff to do if parallel=TRUE
  if(parallel && n.chains>1){
    
    par <- run.parallel(data,inits,parameters.to.save,model.file,n.chains,n.adapt,n.iter,n.burnin,n.thin,
                        modules,seed,DIC) 
    samples <- par$samples
    m <- par$model
    
  } else {
    
    #######################
    ##Run rjags functions##
    #######################
    
    #Compile model 
    m <- jags.model(file=model.file,data=data,inits=inits,n.chains=n.chains,n.adapt=0)
    
    #Adaptive phase using adapt()
    if(n.adapt>0){
      cat('Adaptive phase,',n.adapt,'iterations x',n.chains,'chains','\n')
      cat('If no progress bar appears JAGS has decided not to adapt','\n','\n')
      x <- adapt(object=m,n.iter=n.adapt,progress.bar="text",end.adaptation=TRUE)
    } else{cat('No adaptive period specified','\n','\n')
           #If no adaptation period specified:
           #Force JAGS to not adapt (you have to allow it to adapt at least 1 iteration)
           x <- adapt(object=m,n.iter=1,end.adaptation=TRUE)
    }
    
    #Burn-in phase using update()  
    if(n.burnin>0){
      cat('\n','Burn-in phase,',n.burnin,'iterations x',n.chains,'chains','\n','\n')
      update(object=m,n.iter=n.burnin,progress.bar="text")
      cat('\n')
    } else{cat('No burn-in specified','\n','\n')}
    
    #Sample from posterior using coda.samples() 
    cat('Sampling from joint posterior,',(n.iter-n.burnin),'iterations x',n.chains,'chains','\n','\n')
    samples <- coda.samples(model=m,variable.names=parameters.to.save,n.iter=(n.iter-n.burnin),thin=n.thin,
                            progress.bar="text")
    cat('\n')
    
    ##########################
    ##End of rjags functions##
    ##########################
    
  }
  
  #Get more info about MCMC run
  end.time <- Sys.time() 
  time <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
  cat('MCMC took',time,'minutes.')
  
  #Reorganize JAGS output to match input parameter order
  params <- colnames(samples[[1]])
  params <- params[order(match(sapply(
    strsplit(params, "\\["), "[", 1),parameters.to.save))]
  
  if(DIC&&('deviance'%in%params)){
    params <- c(params[params!='deviance'],'deviance')
  } else if (DIC&&!('deviance'%in%params)){
    warning('JAGS did not monitor deviance.')
    DIC <- FALSE
  } 
  
  if(save.model){
  output <- list()
  output$samples <- samples[,params]
  output$model <- m
  class(output) <- 'jagsUIbasic'
  } else {output <- samples}
 
  
  return(output)
  
}