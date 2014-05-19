

jagsUI <- jags <- function(data,inits=NULL,parameters.to.save,model.file,n.chains,n.adapt=100,n.iter,n.burnin=0,n.thin=1,
                       modules=c('glm'),parallel=FALSE,DIC=TRUE,store.data=FALSE,codaOnly=FALSE,seed=floor(runif(1,1,10000)),bugs.format=FALSE){
  
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
 
  require(parallel)
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
  date <- start.time
  
  #Combine mcmc info into list
  n.samples <- (n.iter-n.burnin) / n.thin * n.chains
  mcmc.info <- list(n.chains,n.adapt,n.iter,n.burnin,n.thin,n.samples,time)
  names(mcmc.info) <- c('n.chains','n.adapt','n.iter','n.burnin','n.thin','n.samples','elapsed.mins')
  
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

  samples <- samples[,params]
  
  #Convert rjags output to jagsUI form 
  output <- process.output(samples,DIC=DIC,codaOnly)
  
  #Add additional information to output list
  
  #Summary
  y = data.frame(unlist(output$mean[!names(output$mean)%in%codaOnly]),unlist(output$sd[!names(output$mean)%in%codaOnly]),
                 unlist(output$q2.5[!names(output$mean)%in%codaOnly]),unlist(output$q25[!names(output$mean)%in%codaOnly]),
                 unlist(output$q50[!names(output$mean)%in%codaOnly]),unlist(output$q75[!names(output$mean)%in%codaOnly]),
                 unlist(output$q97.5[!names(output$mean)%in%codaOnly]),
                 unlist(output$Rhat[!names(output$mean)%in%codaOnly]),unlist(output$n.eff[!names(output$mean)%in%codaOnly]),
                 unlist(output$overlap0[!names(output$mean)%in%codaOnly]),unlist(output$f[!names(output$mean)%in%codaOnly])) 
  params <- colnames(samples[[1]])
  expand <- sapply(strsplit(params, "\\["), "[", 1)  
  row.names(y) = params[!expand%in%codaOnly]
  names(y) = c('mean','sd','2.5%','25%','50%','75%','97.5%','Rhat','n.eff','overlap0','f')
  if(mcmc.info[[1]]==1){
    y = y[,-c(8,9)]
  }
  output$summary <- as.matrix(y)
 
  output$samples <- samples
  output$modfile <- model.file
  #If user wants to save input data/inits
  if(store.data){
    output$inits <- inits
    output$data <- data
  } 
  output$model <- m
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