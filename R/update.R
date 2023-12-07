# update method for jagsUI class-----------------------------------------------
update.jagsUI <- function(object, parameters.to.save=NULL, 
                          n.adapt=NULL, n.iter, n.thin=NULL, 
                          modules=c('glm'), factories=NULL,
                          DIC=NULL,codaOnly=FALSE, verbose=TRUE, ...){
  
  #Get list of parameters to save
  if(is.null(parameters.to.save)) parameters.to.save <- object$parameters
  
  #Set up DIC monitoring
  if(is.null(DIC)) DIC <- object$calc.DIC 
  if(DIC & (!'deviance' %in% parameters.to.save)){
    parameters.to.save <- c(parameters.to.save, 'deviance')
  } else if(!DIC & 'deviance' %in% parameters.to.save){
    parameters.to.save <- parameters.to.save[parameters.to.save != 'deviance']
  }
  
  # Update mcmc info
  mcmc.info <- object$mcmc.info
  mcmc.info$n.iter <- n.iter
  mcmc.info$n.burnin <- 0
  mcmc.info$n.adapt <- n.adapt
  if(!is.null(n.thin)) mcmc.info$n.thin <- n.thin
  
  # Run JAGS via rjags
  rjags_out <- run_rjags(data=NULL, inits=NULL, parameters.to.save, modfile=NULL,
                         mcmc.info, modules, factories, DIC, object$parallel, !verbose,
                         model.object = object$model, update=TRUE)
    
  #Reorganize JAGS output to match input parameter order
  samples <- order_samples(rjags_out$samples, parameters.to.save)    
  #Run process output
  output <- process_output(samples, coda_only = codaOnly, DIC, quiet = !verbose)
  # Fallback if output processing fails
  if(is.null(output)){
    output <- list(samples = samples, model = rjags_out$m)
    output$n.cores <- object$mcmc.info$n.cores
    class(output) <- 'jagsUIbasic'
    return(output)
  }
 
  #Save other information to output object
  output$samples <- samples  
  output$modfile <- object$modfile 
  #If user wants to save input data/inits
  if(!is.null(object$inits)){
    output$inits <- object$inits
    output$data <- object$data
  } 
  output$parameters <- parameters.to.save  
  output$model <- rjags_out$m
  output$mcmc.info <- object$mcmc.info
  output$mcmc.info$n.burnin <- object$mcmc.info$n.iter
  output$mcmc.info$n.iter <- n.iter + output$mcmc.info$n.burnin
  output$mcmc.info$n.thin <- mcmc.info$n.thin
  output$mcmc.info$n.samples <- coda::niter(samples) * output$mcmc.info$n.chains
  output$mcmc.info$elapsed.mins <- rjags_out$elapsed.mins
  output$run.date <- rjags_out$run.date
  output$parallel <- object$parallel
  output$bugs.format <- object$bugs.format
  output$calc.DIC <- DIC
  
  #Keep a record of how many times model has been updated
  if(is.null(object$update.count)){
    output$update.count <- 1
  } else {
    output$update.count <- object$update.count + 1
  }
  
  #Classify final output object
  class(output) <- 'jagsUI'
  
  return(output) 
}

# update method for jagsUIbasic class------------------------------------------
update.jagsUIbasic <- function(object, parameters.to.save=NULL, 
                               n.adapt=NULL, n.iter, n.thin=NULL, 
                               modules=c('glm'), factories=NULL, 
                               DIC=NULL, verbose=TRUE, ...){
  
  # Set up parameters
  if(is.null(parameters.to.save)){
    params_long <- colnames(object$samples[[1]])
    parameters.to.save <- unique(sapply(strsplit(params_long, "\\["), "[", 1))
  }
  
  #Set up DIC monitoring
  if(is.null(DIC)){
    DIC <- 'deviance' %in% parameters.to.save
  } else {
    if(DIC & (!'deviance' %in% parameters.to.save)){
      parameters.to.save <- c(parameters.to.save, 'deviance')
    } else if(!DIC & 'deviance' %in% parameters.to.save){
      parameters.to.save <- parameters.to.save[parameters.to.save != 'deviance']
    }
  }

  # Set up MCMC info
  mcmc.info <- list(n.chains = length(object$samples), n.adapt = n.adapt, 
                    n.iter = n.iter, n.burnin = 0,
                    n.thin = ifelse(is.null(n.thin), coda::thin(object$samples), n.thin),
                    n.cores = object$n.cores)

  parallel <- names(object$model[1]) == "cluster1"

  # Run JAGS via rjags
  rjags_out <- run_rjags(data=NULL, inits=NULL, parameters.to.save, modfile=NULL,
                         mcmc.info, modules, factories, DIC, parallel, !verbose,
                         model.object = object$model, update=TRUE)

  # Report time
  if(verbose) cat('MCMC took', rjags_out$elapsed.min, 'minutes.\n')
  
  # Create output object
  output <- list(samples = order_samples(rjags_out$samples, parameters.to.save),
                 model = rjags_out$m, 
                 n.cores = object$n.cores)
  class(output) <- 'jagsUIbasic'
  
  return(output) 
}
