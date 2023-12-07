
autojags <- function(data, inits=NULL, parameters.to.save, model.file,
                     n.chains, n.adapt=NULL, iter.increment=1000, n.burnin=0, n.thin=1,
                     save.all.iter=FALSE, modules=c('glm'), factories=NULL,
                     parallel=FALSE, n.cores=NULL, DIC=TRUE, store.data=FALSE,
                     codaOnly=FALSE, seed=NULL, bugs.format=FALSE, Rhat.limit=1.1,
                     max.iter=100000, verbose=TRUE){
    
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
  inps <- process_input(data=data, params=parameters.to.save, inits=inits,
                              n_chains=n.chains, n_adapt=n.adapt, 
                              n_iter=(n.burnin + iter.increment), 
                              n_burnin=n.burnin, n_thin=n.thin, n_cores=n.cores,
                              DIC=DIC, quiet=!verbose, parallel=parallel)
  mcmc.info <- inps$mcmc.info
  mcmc.info$end.values <- NULL # this is not saved in autojags for some reason
  
  #Note if saving all iterations
  if(save.all.iter&verbose){
    cat('Note: ALL iterations will be included in final posterior.\n\n')
  }

  #Save start time
  start.time <- Sys.time()
  
  if(verbose){
    cat('Burn-in + Update 1',' (',(n.burnin + iter.increment),')',sep="")
  }
  
  #Initial model run
  mcmc.info$n.iter <- n.burnin + iter.increment
  rjags_out <- run_rjags(inps$data, inps$inits, inps$params, model.file,
                             mcmc.info, modules, factories, DIC, parallel, quiet=TRUE)
  # Save output
  samples <- rjags_out$samples
  mod <- rjags_out$m

  #Update mcmc info
  mcmc.info$elapsed.mins <- rjags_out$elapsed.mins
  mcmc.info$n.samples <- coda::niter(samples) * n.chains
  mcmc.info$n.adapt <- rjags_out$total.adapt
  mcmc.info$sufficient.adapt <- rjags_out$sufficient.adapt
  mcmc.info$n.iter <- n.burnin + iter.increment
  
  # Tests to see if function should stop
  large_Rhats <- test.Rhat(samples, Rhat.limit, codaOnly, verbose=verbose)
  reach.max <- FALSE
  index <- 1
  
  if(mcmc.info$n.iter>=max.iter){
    reach.max <- TRUE
    if(verbose){cat('\nMaximum iterations reached.\n\n')}
  }
  
  # Continue incremental running
  while(large_Rhats & !reach.max){
        
    index <- index + 1
    if(verbose){
      cat('Update ',index,' (',mcmc.info$n.iter + iter.increment,')',sep="")
    }
    
    # MCMC info for just this update
    mcmc_info_update <- mcmc.info
    mcmc_info_update$n.adapt <- n.adapt
    mcmc_info_update$n.iter <- iter.increment
    mcmc_info_update$n.burnin <- 0
    rjags_out <- run_rjags(data=NULL, inits=NULL, inps$params, modfile=NULL,
                           mcmc_info_update, modules, factories, DIC, parallel, 
                           quiet=TRUE, model.object = mod, update=TRUE)
    
    # Save the model object
    mod <- rjags_out$m

    # Save samples and combine with previous samples if required
    if(save.all.iter){
      samples <- bind.mcmc(samples,rjags_out$samples, start=stats::start(samples),
                           n.new.iter=iter.increment)
    } else {
      samples <- rjags_out$samples
    }

    # Update the total iteration count etc.
    if(!save.all.iter) mcmc.info$n.burnin <- mcmc.info$n.iter
    mcmc.info$n.iter <- mcmc.info$n.iter + iter.increment
    mcmc.info$n.samples <- coda::niter(samples) * n.chains
    mcmc.info$sufficient.adapt <- rjags_out$sufficient.adapt

    # Test to see if JAGS should continue updating model
    large_Rhats <- test.Rhat(samples, Rhat.limit, codaOnly)
    if(mcmc.info$n.iter>=max.iter){
      reach.max <- TRUE
      if(verbose) cat('\nMaximum iterations reached.\n\n')
    }
  }
  
  #Save final runtime
  mcmc.info$elapsed.mins <- round(as.numeric(Sys.time()-start.time,units="mins"),digits=3)
  
  #Reorganize JAGS output to match input parameter order
  samples <- order_samples(samples, inps$params)
  
  #Convert rjags output to jagsUI form 
  output <- process_output(samples, coda_only = codaOnly, DIC, quiet = !verbose)
  if(is.null(output)){
    output <- list(samples = samples, model = mod)
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
  output$parameters <- inps$params
  output$mcmc.info <- mcmc.info
  output$run.date <- start.time
  output$parallel <- parallel
  output$bugs.format <- bugs.format
  output$calc.DIC <- DIC
  
  #Classify final output object
  class(output) <- 'jagsUI'
  
  return(output)
}


# Function to test if all Rhats are below some cutoff value--------------------
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
