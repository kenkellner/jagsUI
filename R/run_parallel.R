run_parallel <- function(inp, quiet, logfile=""){
  
  ri <- inp$run.info
  mc <- inp$mcmc.info

  #Output object
  out <- list()

  #Set up cluster
  #Use FORK if on linux
  ctype <- 'PSOCK'
  if(.Platform$OS.type=='unix') ctype <- 'FORK'

  if(!quiet){ 
    cat('Initializing cluster\n')
    cl <- parallel::makeCluster(ri$n.cores, outfile=logfile,
                                mc.silent=T, type=ctype)
  } else {
    cl <- parallel::makeCluster(ri$n.cores, mc.silent=T, type=ctype)
  }
  on.exit(parallel::stopCluster(cl))

  #Exporting environment only necessary on Windows
  if(ctype == 'PSOCK'){
    parallel::clusterExport(cl = cl, ls(), envir = environment())
    parallel::clusterEvalQ(cl, .libPaths(.libPaths()))
  }

  #Function to run 1 chain in each core
  jags_par <- function(x){
    inp$mcmc.info$n.chains <- 1
    inp$model <- inp$model[[x]]
    inp$inits <- inp$inits[[x]]
    call_rjags(inp, quiet, this_chain=x)
  }

  #Distribute chains among cores
  out_raw <- parallel::clusterApply(cl=cl, x=1:mc$n.chains, fun=jags_par)

  #Format output
  out$samples <- lapply(lapply(out_raw,`[[`,1),`[[`,1)
  class(out$samples) <- 'mcmc.list'
  out$model <- lapply(out_raw,`[[`,2)
  out$sufficient.adapt <- unlist(lapply(out_raw,`[[`,3))
  
  out

}
