

simplejags <- function(data,inits=NULL,parameters.to.save,model.file,n.chains,n.adapt=0,n.iter,n.burnin=0,n.thin=1,
                       store.data=TRUE){
  
  start.time <- Sys.time()
  
  #Compile model
  
  m <- jags.model(file=model.file,data=data,inits=inits,n.chains=n.chains,n.adapt=0)
  
  #Adaptive phase
  
  if(n.adapt>0){
  cat('Adaptive phase','\n')
  cat('If no progress bar appears JAGS has decided not to adapt','\n','\n')
  x <- adapt(object=m,n.iter=n.adapt,progress.bar="text")
  } else{cat('No adaptive period specified','\n','\n')}
  
  #Burn-in phase
  
  if(n.burnin>0){
  cat('Burn-in phase','\n','\n')
  update(object=m,n.iter=n.burnin,progress.bar="text")
  cat('\n')
  } else{cat('No burn-in specified','\n','\n')}
  
  #Sample from posterior
  
  cat('Sampling from joint posterior','\n','\n')
  samples <- coda.samples(model=m,variable.names=parameters.to.save,n.iter=(n.iter-n.burnin),thin=n.thin,
                          progress.bar="text") 
  
  end.time <- Sys.time()
  
  time <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
  date <- start.time
  
  #Combine mcmc info
  n.samples <- (n.iter-n.burnin) / n.thin * n.chains
  mcmc.info <- list(n.chains,n.adapt,n.iter,n.burnin,n.thin,n.samples,time)
  names(mcmc.info) <- c('n.chains','n.adapt','n.iter','n.burnin','n.thin','n.samples','elapsed.mins')
  
  #For calculating n.eff
  n <- (n.iter - n.burnin) / n.thin
  
  #Get output stats
  
  output <- process.output(samples,n.chains=n.chains,n=n)
  
  output$samples <- samples
  output$modfile <- model.file
  if(store.data){
    output$inits <- inits
    output$data <- data
  }
  
  output$model <- m
  output$parameters <- parameters.to.save
  output$mcmc.info <- mcmc.info
  output$run.date <- date
  
  
  class(output) <- 'simplejags'
  
  return(output)
  
}