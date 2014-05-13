

jagsUI <- jags <- function(data,inits=NULL,parameters.to.save,model.file,n.chains,n.adapt=100,n.iter,n.burnin=0,n.thin=1,
                       parallel=FALSE,DIC=TRUE,store.data=FALSE,seed=floor(runif(1,1,10000)),bugs.format=FALSE){
  
  #Set random seed
  RNGkind('default')
  set.seed(seed)
  
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
                      seed,DIC) 
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
  if(DIC){
    params <- c(params[params!='deviance'],'deviance')
  }  

  samples <- samples[,params]
  
  #Convert rjags output to jagsUI form 
  output <- process.output(samples,DIC=DIC)
  
  #Add additional information to output list
  
  #Summary
  y = data.frame(unlist(output$mean),unlist(output$sd),unlist(output$q2.5),unlist(output$q25),
                 unlist(output$q50),unlist(output$q75),unlist(output$q97.5),
                 unlist(output$Rhat),unlist(output$n.eff),unlist(output$overlap0),unlist(output$f)) 
  row.names(y) = colnames(samples[[1]])
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