
jags.basic <- function(data, inits=NULL, parameters.to.save ,model.file,
                       n.chains, n.adapt=1000, n.iter, n.burnin=0, n.thin=1,
                       modules=c('glm'), factories=NULL, parallel=FALSE,
                       n.cores=NULL, DIC=TRUE, save.model=FALSE, quiet=FALSE){
  
  #Process inputs
  inp <- process_input(data, inits, parameters.to.save, model.file, n.chains,
                              n.adapt, n.iter, n.burnin, n.thin, modules,
                              factories, parallel, n.cores, DIC)

  #Run JAGS
  out <- run_model(inp, quiet) 
  
  #Cleanup and output
  if(save.model){
    out <- out[c('samples','model','parameters','modfile',
                 'mcmc.info','run.info')]
    class(out) <- 'jagsUIbasic'
    return(out)
  }
  
  order_samples(out$samples, sort(inp$parameters))
}
