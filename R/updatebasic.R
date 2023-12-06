
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
                    n.thin = ifelse(is.null(n.thin), thin(object$samples), n.thin),
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
