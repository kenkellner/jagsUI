setClass("jagsUIbasic")

update.jagsUIbasic <- function(object, parameters.to.save=NULL, n.adapt=100, n.iter, n.thin=NULL){
  if(missing(n.iter)){stop('Specify n.iter, the number of update iterations.')}
  mod <- object$model
  n.chains <- length(object$samples)
  if(is.null(parameters.to.save)){
    params.temp <- colnames(object$samples[[1]])
    parameters <- unique(sapply(strsplit(params.temp, "\\["), "[", 1))
  } else {parameters <- parameters.to.save}
  
  if(is.null(n.thin)){n.thin <- thin(object$samples)}
  
  start.time <- Sys.time()
  
  if(names(object$model[1])=='cluster1'){
    
    #Set number of clusters/chains
    p <- detectCores()
    if(n.chains > p){
      stop('Number of chains (',n.chains,') exceeds available cores (',p,'), reduce number of chains.',sep="")
    } else {n.cluster <- n.chains}
    cl = makeCluster(n.cluster)
    clusterExport(cl = cl, ls(), envir = environment())
    clusterSetRNGStream(cl, floor(runif(1,1,10000)))
    
    cat('Beginning parallel processing with',n.cluster,'clusters. Console output will be suppressed.\n')
    
    jags.clust <- function(i){
      
      #Set initial values for cluster
      cluster.mod <- mod[[i]]
      
      #Load rjags and modules
      require(rjags)
      if('deviance'%in%parameters){
        load.module("dic",quiet=TRUE)
      }
      
      #Recompile model
      cluster.mod$recompile()
      
      #Adapt using adapt()
      if(n.adapt>0){
        x <- adapt(object=cluster.mod,n.iter=n.adapt,progress.bar="none",end.adaptation=TRUE)
      } else{
        x <- adapt(object=cluster.mod,n.iter=1,end.adaptation=TRUE)
      }
      
      #Sample from posterior using coda.samples() 
      samples <- coda.samples(model=cluster.mod,variable.names=parameters,n.iter=n.iter,thin=n.thin,
                              progress.bar="none")
      
      return(list(samp=samples[[1]],mod=cluster.mod))
      
    }
    
    #Do analysis
    par <- clusterApply(cl=cl,x=1:n.chains,fun=jags.clust)
    closeAllConnections()
    
    #Create empty lists
    samples <- model <- list()
    
    #Save samples and model objects from each cluster
    for (i in 1:n.cluster){
      samples[[i]] <- par[[i]][[1]]
      model[[i]] <- par[[i]][[2]]
    }
    samples <- as.mcmc.list(samples)
    names(model) <- sapply(1:length(model),function(i){paste('cluster',i,sep="")})
    
    cat('\nParallel processing completed.\n\n')
    
    
  } else {
    
    if('deviance'%in%parameters){
      load.module("dic",quiet=TRUE)
    }
    
    mod$recompile()
    
    if(n.adapt>0){
      cat('Adaptive phase,',n.adapt,'iterations x',object$mcmc.info$n.chains,'chains','\n')
      cat('If no progress bar appears JAGS has decided not to adapt','\n','\n')
      x <- adapt(object=mod,n.iter=n.adapt,progress.bar="text",end.adaptation=TRUE)
    } else{cat('No adaptive period specified','\n','\n')
           #If no adaptation period specified:
           #Force JAGS to not adapt (you have to allow it to adapt at least 1 iteration)
           x <- adapt(object=mod,n.iter=1,end.adaptation=TRUE)
    }
    
    cat('\nSampling from joint posterior,',n.iter,'iterations x',object$mcmc.info$n.chains,'chains','\n','\n')
    
    samples <- coda.samples(mod,parameters,n.iter,n.thin,progress.bar='text')
    cat('\n')
    
    model <- mod    
  }
  
  params <- colnames(samples[[1]])
  params <- params[order(match(sapply(strsplit(params, "\\["), "[", 1),parameters))]
  if('deviance'%in%parameters){
    params <- c(params[params!='deviance'],'deviance')}   
  
  end.time <- Sys.time() 
  time <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
  cat('MCMC took',time,'minutes.')
  
  output <- list()
  output$samples <- samples[,params]
  output$model <- model
  
  class(output) <- 'jagsUIbasic'
  
  return(output)
  
}