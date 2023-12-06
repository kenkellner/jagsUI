
jagsUI <- jags <- function(data, inits=NULL, parameters.to.save, model.file,
                           n.chains, n.adapt=NULL, n.iter, n.burnin=0, n.thin=1,
                           modules=c('glm'), factories=NULL, parallel=FALSE,
                           n.cores=NULL, DIC=TRUE, store.data=FALSE, 
                           codaOnly=FALSE,seed=NULL, bugs.format=FALSE,verbose=TRUE){
  
  # Check input data
  inps <- process_input(data, parameters.to.save, inits,
                        n.chains, n.adapt, n.iter, n.burnin, n.thin, n.cores,
                        DIC, !verbose, parallel, seed)
   
  # Run JAGS via rjags
  rjags_out <- run_rjags(inps$data, inps$inits, inps$params, model.file,
                         inps$mcmc.info, modules, factories, DIC, parallel, !verbose)

  #Update mcmc.info list
  mcmc.info <- inps$mcmc.info
  iter_final <- coda::niter(rjags_out$samples)
  mcmc.info$elapsed.mins <- rjags_out$elapsed.mins
  mcmc.info$n.samples <- iter_final * n.chains
  mcmc.info$end.values <- rjags_out$samples[iter_final,]
  mcmc.info$n.adapt <- rjags_out$total.adapt
  mcmc.info$sufficient.adapt <- rjags_out$sufficient.adapt

  # Reorganize JAGS output to match input parameter order
  samples <- order_samples(rjags_out$samples, inps$params)
  # Process output and calculate statistics
  output <- process_output(samples, coda_only = codaOnly, DIC, quiet = !verbose)
  # Fallback if processing output fails
  if(is.null(output)){
    output <- list(samples = samples, model = rjags_out$m) 
    output$n.cores <- mcmc.info$n.cores
    class(output) <- 'jagsUIbasic'
    return(output)
  }
  
  #Add additional information to output list
  output$samples <- samples
  output$modfile <- model.file
  #If user wants to save input data/inits
  if(store.data){
    output$inits <- inps$inits
    output$data <- inps$data
  } 
  output$model <- rjags_out$m
  output$parameters <- inps$params
  output$mcmc.info <- mcmc.info
  output$run.date <- rjags_out$run.date
  output$parallel <- parallel
  output$bugs.format <- bugs.format
  output$calc.DIC <- DIC
  
  #Classify final output object
  class(output) <- 'jagsUI'
  
  return(output)
  
}
