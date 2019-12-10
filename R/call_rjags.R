call_rjags <- function(inp, quiet=FALSE, this_chain=NULL){
  
  ri <- inp$run.info
  mc <- inp$mcmc.info

  if(quiet | ri$parallel) options("jags.pb"="none")
    
  #Initialize output object
  out <- list()
  
  #Load modules and factories
  load_modules(ri$modules)
  load_factories(ri$factories)

  #Get chain ID for printing progress (if necessary)
  ch_id <- NULL
  if(!is.null(this_chain)){
    ch_id <- paste0('[chain ',this_chain,'] ')
  }

  #Initialize model object-----------------------------------------------------
  if(is.null(inp$model)){
    if(!quiet) cat(paste0(ch_id,'Initializing model\n'))
    inp$model <- init_model(model_file=inp$modfile, 
                            data=inp$data, inits=inp$inits, 
                            n_chains=mc$n.chains)
  }
  #----------------------------------------------------------------------------
  
  #Adaptive phase--------------------------------------------------------------
  if(!quiet) cat(paste0(ch_id,'Adapting\n'))  
  adapt_check <- adapt_model(inp$model, n_iter=mc$n.adapt)
  if(any(!adapt_check)){
    warning(paste0(ch_id,"JAGS reports adaptation was incomplete"))
  }
  #----------------------------------------------------------------------------
  
  #Burn-in phase---------------------------------------------------------------
  st_time <- Sys.time()
  if ( mc$n.burnin > 0 ){

    if ( !ri$parallel | mc$n.burnin < 10 | quiet ){
      #Update in one chunk if single-core or few iterations
      if(!quiet) cat(paste0(ch_id,'Burn-in\n'))
      update_model(inp$model, n_iter=mc$n.burnin)
    
    } else {
      #Run in ~10% chunks to show progress
      chunks <- get_chunks(mc$n.burnin)

      for (i in 1:length(chunks)){
        update_model(inp$model, n_iter=chunks[i])
        tl <- time_left(st_time, sum(chunks[1:i]), mc$n.iter)
        pct <- round(sum(chunks[1:i])/mc$n.burnin*100)

        if(!quiet) cat(paste0(ch_id,'Burn-in ',pct,'%, ',tl,'\n'))
      }
    }
  }
  #----------------------------------------------------------------------------

  #Sample from posterior-------------------------------------------------------
  n.samples <- mc$n.iter - mc$n.burnin

  #Common function for generating samples
  get_samples <- function(n_samples){
    sample_model(inp$model, inp$parameters, n_samples, mc$n.thin, na.rm=mc$na.rm)
  }
  
  #Get samples
  if ( !ri$parallel | n.samples < 10 | quiet ){
    #Get all samples at once if single-core or few iterations
    if (!quiet) cat(paste0(ch_id,'Sampling posterior\n'))
    out$samples <- get_samples(n.samples)

  } else {   
    #Run in ~10% chunks to show progress
    chunks <- get_chunks(n.samples)
    sample_chunks <- vector(length=length(chunks),"list")
    for (i in 1:length(chunks)){
      sample_chunks[[i]] <- get_samples(chunks[i])
      tl <- time_left(st_time, mc$n.burnin + sum(chunks[1:i]), 
                      mc$n.iter)
      pct <- round(sum(chunks[1:i])/n.samples*100)
      if(!quiet) cat(paste0(ch_id,'Sampling ', pct,'%, ',tl,'\n'))
    }

    out$samples <- Reduce(comb_mcmc_list,sample_chunks) #Combine chunks
  }
  #----------------------------------------------------------------------------
  
  #Save model object
  out$model <- inp$model
  
  #Save adaptation status
  out$sufficient.adapt <- all(adapt_check)

  out

}
