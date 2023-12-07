
autojags <- function(data,inits=NULL,parameters.to.save,model.file,n.chains,n.adapt=NULL,iter.increment=1000,n.burnin=0,n.thin=1,
                     save.all.iter=FALSE,modules=c('glm'),factories=NULL,parallel=FALSE,n.cores=NULL,DIC=TRUE,store.data=FALSE,codaOnly=FALSE,seed=NULL,
                    bugs.format=FALSE,Rhat.limit=1.1,max.iter=100000,verbose=TRUE){
    
  if(!is.null(seed)){
    stop("The seed argument is no longer supported, use set.seed() instead", call.=FALSE)
  }
  if(n.chains<2) stop('Number of chains must be >1 to calculate Rhat.')
  if(max.iter < n.burnin){
    old_warn <- options()$warn
    options(warn=1)
    warning('Maximum iterations includes burn-in and should be larger than burn-in.')
    options(warn=old_warn)
  }

  # Check input data
  inps_check <- process_input(data=data, params=parameters.to.save, inits=inits,
                              n_chains=n.chains, n_adapt=n.adapt, 
                              n_iter=(n.burnin + iter.increment), 
                              n_burnin=n.burnin, n_thin=n.thin, n_cores=n.cores,
                              DIC=DIC, quiet=!verbose, parallel=parallel)
  data <- inps_check$data
  parameters.to.save <- inps_check$params
  inits <- inps_check$inits
  mcmc.info <- inps_check$mcmc.info
  mcmc.info$end.values <- NULL # this is not saved in autojags for some reason
  if(parallel) n.cores <- inps_check$mcmc.info$n.cores
  
  #Save start time
  start.time <- Sys.time()
  
  #Note if saving all iterations
  if(save.all.iter&verbose){cat('Note: ALL iterations will be included in final posterior.\n\n')}
  
  #Initial model run
  
  #Parallel
  
  if(verbose){cat('Burn-in + Update 1',' (',(n.burnin + iter.increment),')',sep="")}
  
  if(parallel){
    
    par <- run.parallel(data,inits,parameters.to.save,model.file,n.chains,n.adapt,n.iter=(n.burnin + iter.increment),n.burnin,n.thin,
                        modules=modules,factories=factories,DIC=DIC,verbose=FALSE,n.cores=n.cores) 
    samples <- par$samples
    mod <- par$model
    total.adapt <- par$total.adapt
    sufficient.adapt <- par$sufficient.adapt
    
  } else {
    
  #Not parallel  
    
    set.modules(modules,DIC)
    set.factories(factories)
    
    rjags.output <- run.model(model.file,data,inits,parameters.to.save,n.chains,n.iter=(n.burnin + iter.increment),n.burnin,n.thin,n.adapt,
                              verbose=FALSE)
    samples <- rjags.output$samples
    mod <- rjags.output$m
    total.adapt <- rjags.output$total.adapt
    sufficient.adapt <- rjags.output$sufficient.adapt
    
  }
  
  #Combine mcmc info into list
  mcmc.info$elapsed.mins <- round(as.numeric(Sys.time()-start.time,units="mins"),digits=3)
  mcmc.info$n.samples <- coda::niter(samples) * n.chains
  mcmc.info$n.adapt <- total.adapt
  mcmc.info$sufficient.adapt <- sufficient.adapt
  mcmc.info$n.iter <- n.burnin + iter.increment
  
  test <- test.Rhat(samples,Rhat.limit,codaOnly,verbose=verbose)
  reach.max <- FALSE
  index = 1
  
  if(mcmc.info$n.iter>=max.iter){
    reach.max <- TRUE
    if(verbose){cat('\nMaximum iterations reached.\n\n')}
  }
  
  while(test==TRUE && reach.max==FALSE){
        
    index <- index + 1
    if(verbose){cat('Update ',index,' (',mcmc.info$n.iter + iter.increment,')',sep="")}
    
    if(save.all.iter){
      if(index==2){start.iter <- stats::start(samples)}
      if (index > 1) {
        old.samples <- samples
      }
    }
       
    if(parallel){
      
      par <- run.parallel(data=NULL,inits=NULL,parameters.to.save=parameters.to.save,model.file=NULL,n.chains=n.chains
                          ,n.adapt=n.adapt,n.iter=iter.increment,n.burnin=0,n.thin=n.thin,modules=modules,
                          factories=factories,DIC=DIC,model.object=mod,update=TRUE,verbose=FALSE,n.cores=n.cores) 
      
      if(save.all.iter & index > 1){
        samples <- bind.mcmc(old.samples,par$samples,start=start.iter,n.new.iter=iter.increment)
      } else {samples <- par$samples}
      
      mod <- par$model
      sufficient.adapt <- par$sufficient.adapt
      
      test <- test.Rhat(samples,Rhat.limit,codaOnly)
      
    } else {
      
      set.modules(modules,DIC)
      
      rjags.output <- run.model(model.file=NULL,data=NULL,inits=NULL,parameters.to.save=parameters.to.save,
                                n.chains=n.chains,n.iter=iter.increment,n.burnin=0,n.thin,n.adapt=n.adapt,
                                model.object=mod,update=TRUE,verbose=FALSE)
      
      if(save.all.iter & index > 1){
        samples <- bind.mcmc(old.samples,rjags.output$samples,start=start.iter,n.new.iter=iter.increment)
      } else {samples <- rjags.output$samples}

      mod <- rjags.output$m
      sufficient.adapt <- rjags.output$sufficient.adapt

      test <- test.Rhat(samples,Rhat.limit,codaOnly)

     
    }
    
    if(!save.all.iter){mcmc.info$n.burnin <- mcmc.info$n.iter}   
    mcmc.info$n.iter <- mcmc.info$n.iter + iter.increment    
    mcmc.info$n.samples <- coda::niter(samples) * n.chains
    mcmc.info$sufficient.adapt <- sufficient.adapt
    
    if(mcmc.info$n.iter>=max.iter){
      reach.max <- TRUE
      if(verbose){cat('\nMaximum iterations reached.\n\n')}
    }
  }
  
  #Get more info about MCMC run
  mcmc.info$elapsed.mins <- round(as.numeric(Sys.time()-start.time,units="mins"),digits=3)
  
  #Reorganize JAGS output to match input parameter order
  samples <- order_samples(samples, parameters.to.save)
  
  #Convert rjags output to jagsUI form 
  output <- process_output(samples, coda_only = codaOnly, DIC, quiet = !verbose)
  if(is.null(output)){
    output <- list()
    output$samples <- samples
    output$model <- mod
    output$n.cores <- n.cores
    class(output) <- 'jagsUIbasic'
    return(output)
  }

  #Add additional information to output list
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
  output$run.date <- start.time
  output$parallel <- parallel
  output$bugs.format <- bugs.format
  output$calc.DIC <- DIC
  
  #Classify final output object
  class(output) <- 'jagsUI'
  
  return(output)
  
  
  
  
  
  
  
  
}
 

test.Rhat <- function(samples,cutoff,params.omit,verbose=TRUE){
  
  params <- colnames(samples[[1]])
  expand <- sapply(strsplit(params, "\\["), "[", 1)
  
  gd <- function(hold){
    r <- try(coda::gelman.diag(hold, autoburnin=FALSE)$psrf[1], silent=TRUE)
    if(inherits(r, "try-error") || !is.finite(r)) {
      r <- NA
    }
    return(r)
  }
  
  failure <- FALSE
  index <- 1
  while (failure==FALSE && index <= length(params)){
    
    if(!expand[index]%in%params.omit){
      test <- gd(samples[,index])
    } else {test <- 1}
    
    if(is.na(test)){test <- 1}
   
    if(test>cutoff){failure=TRUE
    } else {index <- index + 1}
  }
  
  if(failure==TRUE&verbose){
    cat('.......Convergence check failed for parameter \'',params[index],'\'\n',sep="")
  }
  if(failure==FALSE&verbose){
    cat('.......All parameters converged.','\n\n')
  }
  
  return(failure)
  
}
