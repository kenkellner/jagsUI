
autojags <- function(data, inits=NULL, parameters.to.save, model.file,
                     n.chains, n.adapt=1000, iter.increment=1000, n.burnin=0,
                     n.thin=1, save.all.iter=FALSE, modules=c('glm'),
                     factories=NULL, parallel=FALSE, n.cores=NULL, DIC=TRUE,
                     no.stats=NULL, Rhat.limit=1.1, max.iter=100000, quiet=FALSE){
  
  #Save overall start time
  start.time <- Sys.time()
  
  #Initial model run
  params <- check_params(parameters.to.save, DIC)
  cur_iter <- iter.increment + n.burnin
  if(!quiet) cat('Burn-in + Update 1',' (',cur_iter,')\n',sep="")
  out <- jags(data, inits, parameters.to.save, model.file, n.chains,
              n.adapt, cur_iter, n.burnin, n.thin, modules, factories,
              parallel, n.cores, DIC, no.stats=params, quiet=TRUE)
  
  Rhat_fail <- test_Rhat(out$samples, Rhat.limit)
  reach_max <- cur_iter >= max.iter
  index <- 0
  new_burnin = n.burnin

  while(Rhat_fail$result & !reach_max){
    
    index <- index + 1
    new_burnin <- cur_iter
    cur_iter <- cur_iter + iter.increment
   
    if(!quiet) cat('Update ',index,' (',cur_iter,')\n',sep="")

    if(save.all.iter) old_samples <- out$samples
    
    out <- stats::update(out, n.adapt=n.adapt, n.iter=iter.increment,
                         no.stats = params, quiet=TRUE)

    if(save.all.iter) out$samples <- comb_mcmc_list(old_samples, out$samples)
    
    #Tests
    Rhat_fail <- test_Rhat(out$samples, Rhat.limit)
    reach_max <- cur_iter >= max.iter
  }

  if(!quiet & reach_max) cat('\nMaximum iterations reached.\n\n')
  
  #Update MCMC info with final results
  out$run.info$start.time <- start.time
  out$run.info$end.time <- Sys.time()
  out$mcmc.info$n.iter <- cur_iter
  out$mcmc.info$n.burnin <- new_burnin
  if(save.all.iter){
    out$mcmc.info$n.burnin <- n.burnin
    out$mcmc.info$n.samples <- nrow(out$samples[[1]]) * out$mcmc.info$n.chains
  }
  
  #Process output
  stats <- process_output(out$samples, exclude_params=no.stats)
  
  #Build jagsUI object
  out[c("sims.list","pD","DIC","summary")] <- NULL
  out <- c(stats, out)
  class(out) <- 'jagsUI'
  out
} 
