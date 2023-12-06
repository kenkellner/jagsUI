
jags.basic <- function(data, inits=NULL, parameters.to.save, model.file,
                       n.chains, n.adapt=NULL, n.iter, n.burnin=0, n.thin=1,
                       modules=c('glm'), factories=NULL, parallel=FALSE, 
                       n.cores=NULL, DIC=TRUE, seed=NULL, save.model=FALSE, verbose=TRUE){
  
  # Check input data
  inps <- process_input(data, parameters.to.save, inits,
                        n.chains, n.adapt, n.iter, n.burnin, n.thin, n.cores,
                        DIC, !verbose, parallel, seed)
  
  # Run JAGS via rjags
  rjags_out <- run_rjags(inps$data, inps$inits, inps$params, model.file, 
                         inps$mcmc.info, modules, factories, DIC, parallel, !verbose)
  
  # Report time
  if(verbose) cat('MCMC took', rjags_out$elapsed.mins, 'minutes.\n')
 
  # Create output object
  if(save.model){
    samples <- order_samples(rjags_out$samples, inps$params)
    output <- list(samples = samples, model = rjags_out$m)
    output$n.cores <- inps$mcmc.info$n.cores
    class(output) <- 'jagsUIbasic'
  } else{
    output <- rjags_out$samples
  }
 
  return(output)
}
