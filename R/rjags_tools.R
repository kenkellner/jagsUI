run_rjags <- function(data, inits, params, modfile, mcmc_info,
                      modules, factories, DIC, parallel, quiet){
  
  #Save start time
  start.time <- Sys.time()

  mc <- mcmc_info

  # Run parallel
  if(parallel & mc$n.chains > 1){
    result <- run.parallel(data, inits, params, modfile, 
                           mc$n.chains, mc$n.adapt, mc$n.iter, mc$n.burnin, mc$n.thin,
                           modules=modules, factories=factories, DIC=DIC,
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
                        verbose=!quiet)
  }
  
  result$run.date <- start.time
  result$elapsed.mins <- round(as.numeric(Sys.time()-start.time,units="mins"),digits=3)
  
  result
}
