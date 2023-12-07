# Top-level function to run analysis via rjags---------------------------------
run_rjags <- function(data, inits, params, modfile, mcmc_info,
                      modules, factories, DIC, parallel, quiet, 
                      model.object=NULL, update=FALSE){
  
  #Save start time
  start.time <- Sys.time()

  mc <- mcmc_info

  # Run parallel
  if(parallel & mc$n.chains > 1){
    result <- run.parallel(data, inits, params, modfile, 
                           mc$n.chains, mc$n.adapt, mc$n.iter, mc$n.burnin, mc$n.thin,
                           modules=modules, factories=factories, DIC=DIC,
                           model.object=model.object, update=update,
                           verbose=!quiet, n.cores=mc$n.cores)
    # Move this down if not also handled in runmodel()
    if(any(!result$sufficient.adapt)){
      warning("JAGS reports adaptation was incomplete. Consider increasing n.adapt", call.=FALSE)
    }
  } else {
    # Run non-parallel
    set.modules(modules, DIC)
    set.factories(factories)

    result <- run.model(modfile, data, inits, params,
                        mc$n.chains, mc$n.iter, mc$n.burnin, mc$n.thin, mc$n.adapt,
                        verbose=!quiet, model.object=model.object, update=update)
  }
  
  result$run.date <- start.time
  result$elapsed.mins <- round(as.numeric(Sys.time()-start.time,units="mins"),digits=3)
  
  result
}

# Setup and run rjags----------------------------------------------------------
run.model <- function(model.file=NULL, data=NULL, inits=NULL, parameters.to.save,
                      n.chains=NULL, n.iter, n.burnin, n.thin, n.adapt,
                      verbose=TRUE, model.object=NULL, update=FALSE,
                      parallel=FALSE, na.rm=TRUE){
  
  if(verbose){
    pb <- "text"
  } else {
    pb <- "none"
  }

  if(update){
    #Recompile model
    m <- model.object
    if(verbose | parallel==TRUE){
      m$recompile()
    } else {
      null <- utils::capture.output(m$recompile())
    }

  } else {
    #Compile model
    if(verbose | parallel==TRUE){
      m <- rjags::jags.model(file=model.file,data=data,
                             inits=inits,n.chains=n.chains,n.adapt=0)
    } else {
      null <- utils::capture.output(
      m <- rjags::jags.model(file=model.file,data=data,inits=inits,
                             n.chains=n.chains,n.adapt=0,quiet=TRUE)
      )
    }
  }

  #Adaptive phase using adapt()
  total.adapt <- 0

  if(!is.null(n.adapt)){
    if(n.adapt>0){
      if(verbose){
        cat('Adaptive phase,',n.adapt,'iterations x',n.chains,'chains','\n')
        cat('If no progress bar appears JAGS has decided not to adapt','\n','\n')
        sufficient.adapt <- rjags::adapt(object=m, n.iter=n.adapt, 
                                         progress.bar=pb, end.adaptation=TRUE)
      } else {
        null <- utils::capture.output(
        sufficient.adapt <- rjags::adapt(object=m, n.iter=n.adapt, 
                                         progress.bar=pb, end.adaptation=TRUE)
        )
      }
      total.adapt <- total.adapt + n.adapt
    } else{
      if(verbose){cat('No adaptive period specified','\n','\n')}
      #If no adaptation period specified:
      #Force JAGS to not adapt (you have to allow it to adapt at least 1 iteration)
      if(!update){
        if(verbose){
          sufficient.adapt <- rjags::adapt(object=m, n.iter=1, end.adaptation=TRUE)
        } else {
          null <- utils::capture.output(
            sufficient.adapt <- rjags::adapt(object=m, n.iter=1, end.adaptation=TRUE)
          )}
      }
      total.adapt <- 0
    }
  } else {

    maxloops <- 100
    n.adapt.iter <- 100

    for (i in 1:maxloops){
      if(verbose) cat('Adaptive phase.....','\n')
      sufficient.adapt <- rjags::adapt(object=m, n.iter=n.adapt.iter, progress.bar='none')
      total.adapt <- total.adapt + n.adapt.iter
      if(i==maxloops){
        if(verbose){
          warning(paste("Reached max of",maxloops*n.adapt.iter,"adaption iterations; set n.adapt to > 10000"))
        }
        null <- rjags::adapt(object=m,n.iter=1,end.adaptation = TRUE)
        break
      }
      if(sufficient.adapt){
        null <- rjags::adapt(object=m,n.iter=1,end.adaptation = TRUE)
        if(verbose) cat('Adaptive phase complete','\n','\n')
        break
      }
    }
  }

  if(!sufficient.adapt & total.adapt!=0){
    warning("JAGS reports adaptation was incomplete. Consider increasing n.adapt")
  }

  #Burn-in phase using update()
  if(n.burnin>0){
    if(verbose){
      cat('\n','Burn-in phase,',n.burnin,'iterations x',n.chains,'chains','\n','\n')
      update(object=m,n.iter=n.burnin,progress.bar=pb)
      cat('\n')
    } else {
      null <- utils::capture.output(
      update(object=m,n.iter=n.burnin,progress.bar=pb)
    )}
  } else if(verbose){
    cat('No burn-in specified','\n','\n')
  }

  #Sample from posterior using coda.samples()
  if(verbose){
    cat('Sampling from joint posterior,',(n.iter-n.burnin),
        'iterations x',n.chains,'chains','\n','\n')
    samples <- rjags::coda.samples(model=m, variable.names=parameters.to.save,
                                   n.iter=(n.iter-n.burnin), thin=n.thin,
                                   na.rm=na.rm, progress.bar=pb)
    cat('\n')
  } else {
    null <- utils::capture.output(
    samples <- rjags::coda.samples(model=m, variable.names=parameters.to.save,
                                   n.iter=(n.iter-n.burnin), thin=n.thin,
                                   na.rm=na.rm, progress.bar=pb)
    )
  }

  return(list(m=m,samples=samples,
              total.adapt=total.adapt,
              sufficient.adapt=sufficient.adapt))
}

# Setup parallel and run rjags-------------------------------------------------
run.parallel <- function(data=NULL, inits=NULL, parameters.to.save, model.file=NULL,
                         n.chains, n.adapt, n.iter, n.burnin, n.thin,
                         modules, factories, DIC,
                         model.object=NULL, update=FALSE, 
                         verbose=TRUE, n.cores=NULL) {

  #Save current library paths
  current.libpaths <- .libPaths()

  #Set up clusters
  cl <- parallel::makeCluster(n.cores)
  on.exit(parallel::stopCluster(cl))
  parallel::clusterExport(cl = cl, ls(), envir = environment())
  parallel::clusterEvalQ(cl,.libPaths(current.libpaths))

  if(verbose){
    cat('Beginning parallel processing using',n.cores,
        'cores. Console output will be suppressed.\n')
  }

  #Function called in each core
  jags.clust <- function(i){

    #Load modules
    set.modules(modules,DIC)
    set.factories(factories)

    if(update){
      #Recompile model
      cluster.mod <- model.object[[i]]
      #Run model
      rjags.output <- run.model(model.file=NULL,data=NULL,inits=NULL,parameters.to.save,
                                n.chains=1,n.iter,n.burnin=0,n.thin,n.adapt,
                                verbose=FALSE,model.object=cluster.mod,update=TRUE,
                                parallel=TRUE, na.rm=FALSE)
    } else {
      #Set initial values for cluster
      cluster.inits <- inits[[i]]
      #Run model
      rjags.output <- run.model(model.file,data,inits=cluster.inits,parameters.to.save,
                                n.chains=1,n.iter,n.burnin,n.thin,n.adapt,verbose=FALSE,
                                parallel=TRUE, na.rm=FALSE)
    }

    return(list(samp=rjags.output$samples[[1]],
                mod=rjags.output$m,
                total.adapt=rjags.output$total.adapt,
                sufficient.adapt=rjags.output$sufficient.adapt))
    }

  #Do parallel analysis
  par <- parallel::clusterApply(cl=cl, x=1:n.chains, fun=jags.clust)

  #Create empty lists
  out <- samples <- model <- list()
  total.adapt <- sufficient.adapt <- vector(length=n.chains)

  #Save samples from each cluster into mcmc.list
  total.adapt <- sapply(par, function(x) x[[3]])
  starts <- sapply(par, function(x) stats::start(x$samp))
  ends <- sapply(par, function(x) stats::end(x$samp))
  nsamp <- sapply(par, function(x) coda::niter(x$samp))
  thins <- sapply(par, function(x) coda::thin(x$samp))
  stopifnot(all(nsamp == nsamp[1]))
  stopifnot(all(thins == n.thin))
  for (i in 1:n.chains){
    samples[[i]] <- coda::mcmc(par[[i]]$samp,start=max(starts),thin=n.thin)
    model[[i]] <- par[[i]]$m
    sufficient.adapt[i] <- par[[i]]$sufficient.adapt
  }
  out$samples <- coda::as.mcmc.list(samples)
  
  # Remove columns with all NA
  try({
    all_na <- apply(as.matrix(out$samples),2, function(x) all(is.na(x)))
    out$samples <- out$samples[,!all_na,drop=FALSE]
  })
  
  # Save other info in output object
  out$model <- model
  out$total.adapt <- total.adapt
  out$sufficient.adapt <- sufficient.adapt
  names(out$model) <- sapply(1:length(out$model),function(i){paste('cluster',i,sep="")})

  if(verbose){
    cat('\nParallel processing completed.\n\n')
  }

  return(out)
}


# Set factories----------------------------------------------------------------
set.factories <- function(factories){
  
  if(!is.null(factories)){
    for (i in 1:length(factories)){
    
      split <- strsplit(factories[i],'\\s')[[1]]
    
      #Check if requested factory is available
      faclist <- as.character(rjags::list.factories(split[2])[,1])
      if(split[1]%in%faclist){
        
        null <- rjags::set.factory(split[1],split[2],split[3])
      
      } else{stop(paste('Requested factory',split[1],'is not available. Check that appropriate modules are loaded.'))}
    
    }
  }
  
}

# Set modules------------------------------------------------------------------
set.modules <- function(modules,DIC){

  #Load/unload appropriate modules (besides dic)
  called.set <- c('basemod','bugs',modules)
  current.set <- rjags::list.modules()

  load.set <- called.set[!called.set%in%current.set]
  unload.set <- current.set[!current.set%in%called.set]

  if(length(load.set)>0){
    for (i in 1:length(load.set)){
      rjags::load.module(load.set[i],quiet=TRUE)
    }
  }
  if(length(unload.set)>0){
    for (i in 1:length(unload.set)){
      rjags::unload.module(unload.set[i],quiet=TRUE)
    }
  }
  if(DIC){
    rjags::load.module("dic",quiet=TRUE)
  }
}
