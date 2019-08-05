
jags.basic <- function(data, inits=NULL, parameters.to.save ,model.file,
                       n.chains, n.adapt=NULL, n.iter, n.burnin=0, n.thin=1,
                       modules=c('glm'), factories=NULL, parallel=FALSE,
                       n.cores=NULL, DIC=TRUE, save.model=FALSE, verbose=TRUE){
  
  #Check parameters
  params <- check_params(parameters.to.save, DIC)
  
  #Run regular jags() but all parameters are codaOnly
  jags_out <- jags(data, inits, params, model.file, n.chains, 
                   n.adapt, n.iter, n.burnin, n.thin, modules, factories, 
                   parallel, n.cores, DIC, codaOnly = params, 
                   verbose=verbose)
  
  #Cleanup and output
  if(save.model){
    jags_out$n.cores <- jags_out$mcmc.info$n.cores
    to_remove <- c('sims.list','pD','DIC','summary','modfile','parameters',
                   'mcmc.info','run.date','parallel','bugs.format','calc.DIC')
    jags_out[to_remove] <- NULL
    class(jags_out) <- 'jagsUIbasic'
    return(jags_out)
  }
  
  jags_out$samples <- order_samples(jags_out$samples, sort(params))
  jags_out$samples  
}
