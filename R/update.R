
update.jagsUI <- function(object, parameters.to.save=NULL, n.adapt=100, n.iter, n.thin=NULL){
  if(class(object)!="jagsUI"){stop('Requires jagsUI object as input')}
  mod <- object$model
  if(is.null(parameters.to.save)){parameters <- object$parameters
  } else {parameters <- parameters.to.save}
  if(object$DIC&&!'deviance'%in%parameters){parameters <- c(parameters,"deviance")}
  
  if(is.null(n.thin)){n.thin <- object$mcmc.info$n.thin}
  
  start.time <- Sys.time()
  
  if(object$parallel){
    
    #Set number of clusters/chains
    p <- detectCores()
    if(object$mcmc.info$n.chains > p){
      stop('Number of chains (',object$mcmc.info$n.chains,') exceeds available cores (',p,'), reduce number of chains.',sep="")
    } else {n.cluster <- object$mcmc.info$n.chains}
    cl = makeCluster(n.cluster)
    clusterExport(cl = cl, ls(), envir = environment())
    clusterSetRNGStream(cl, object$random.seed)
    
    cat('Beginning parallel processing with',n.cluster,'clusters. Console output will be suppressed.\n')
    
    jags.clust <- function(i){
      
      #Set initial values for cluster
      cluster.mod <- mod[[i]]
      
      #Load rjags and modules
      require(rjags)
      if(object$DIC){
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
    par <- clusterApply(cl=cl,x=1:object$mcmc.info$n.chains,fun=jags.clust)
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
    
    if(object$DIC){load.module("dic",quiet=TRUE)}
    
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
  if(object$DIC){params <- c(params[params!='deviance'],'deviance')}   
  samples <- samples[,params]
  
  end.time <- Sys.time() 
  time <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
  date <- start.time
  
  #Run process output
  output <- process.output(samples,DIC=object$DIC)
    
  #Summary
  y = data.frame(unlist(output$mean),unlist(output$sd),unlist(output$q2.5),unlist(output$q25),
                 unlist(output$q50),unlist(output$q75),unlist(output$q97.5),
                 unlist(output$Rhat),unlist(output$n.eff),unlist(output$overlap0),unlist(output$f)) 
  row.names(y) = colnames(samples[[1]])
  names(y) = c('mean','sd','2.5%','25%','50%','75%','97.5%','Rhat','n.eff','overlap0','f')
  if(object$mcmc.info$n.chains==1){
    y = y[,-c(8,9)]
  }
  output$summary <- as.matrix(y)
    
  output$samples <- samples
  
  output$modfile <- object$modfile
  #If user wants to save input data/inits
  if(!is.null(object$inits)){
    output$inits <- object$inits
    output$data <- object$data
  } 
  output$parameters <- parameters
  
  output$model <- model
  output$mcmc.info <- object$mcmc.info
  output$mcmc.info$n.burnin <- object$mcmc.info$n.iter
  output$mcmc.info$n.iter <- n.iter + output$mcmc.info$n.burnin
  output$mcmc.info$n.thin <- n.thin
  output$mcmc.info$n.samples <- round(output$mcmc.info$n.iter / n.thin,0)
  output$mcmc.info$elapsed.mins <- time
  output$run.date <- date
  output$random.seed <- object$random.seed
  output$parallel <- object$parallel
  output$bugs.format <- object$bugs.format
  
  if(is.null(object$update.count)){output$update.count <- 1
  } else {output$update.count <- object$update.count + 1}
  
  #Classify final output object
  class(output) <- 'jagsUI'
  
  return(output)
  
}