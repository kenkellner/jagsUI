

jagsUI <- jags <- function(data,inits=NULL,parameters.to.save,model.file,n.chains,n.adapt=100,n.iter,n.burnin=0,n.thin=1,
                       modules=c('glm'),parallel=FALSE,DIC=TRUE,store.data=FALSE,codaOnly=FALSE,seed=floor(runif(1,1,10000)),bugs.format=FALSE){
  
  #Set random seed
  RNGkind('default')
  set.seed(seed)
  
  #Pass input data and parameter list through error check / processing
  data.check <- process.input(data,parameters.to.save,inits,n.chains,n.iter,n.burnin,n.thin,modules,DIC=DIC)
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
  
  rjags.output <- run.model(model.file,data,inits,parameters.to.save,n.chains,n.iter,n.burnin,n.thin,n.adapt)
  samples <- rjags.output$samples
  m <- rjags.output$m
  
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
  samples <- order.params(samples,parameters.to.save,DIC)
  
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
  p <- colnames(samples[[1]])
  expand <- sapply(strsplit(p, "\\["), "[", 1)  
  row.names(y) = p[!expand%in%codaOnly]
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