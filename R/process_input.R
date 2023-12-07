# Process input----------------------------------------------------------------
process_input <- function(data, params, inits, n_chains, n_adapt, n_iter, n_burnin,
                          n_thin, n_cores, DIC, quiet, parallel, seed=NULL){
  
  if(!quiet){cat('\nProcessing function input.......','\n')}

  if(!is.null(seed)){
    stop("The seed argument is no longer supported, use set.seed() instead", call.=FALSE)
  }

  out <- list(data = check_data(data, quiet),
              params = check_params(params, DIC),
              inits = check_inits(inits, n_chains),
              mcmc.info = check_mcmc_info(n_chains, n_adapt, n_iter, n_burnin, 
                                          n_thin, n_cores, parallel)
  )
  if(!quiet){cat('\nDone.','\n','\n')}
  out
}


# Check data list--------------------------------------------------------------
check_data <- function(inp_data, quiet){
  # Check data is a list
  if(!is.list(inp_data)){
    stop("Input data must be a named list", call.=FALSE)
  }
  # Check list is named
  nms <- names(inp_data)
  if(is.null(nms) || any(nms == "")){
    stop("All elements of the input data list must be named", call.=FALSE)
  }
  
  # Check individual data elements
  out <- lapply(1:length(inp_data), function(i){
    check_data_element(inp_data[[i]], nms[i], quiet)
  })
  names(out) <- nms
  out
}


# Check individual data elements-----------------------------------------------
check_data_element <- function(x, name, quiet){
  # Stop if element is a factor
  if(is.factor(x)){
    stop("Element", name, "is a factor. Convert it to numeric.", call.=FALSE)
  }
  # Try to convert data frame to matrix/vector
  if(is.data.frame(x)){
    # Old versions attempted to convert a 1-column data frame to vector
    # but did it incorrectly (always converted to matrix).
    # This behavior was preserved here.
    x <- as.matrix(x)
    if(!is.numeric(x)){
      stop("Could not convert data.frame ", name, " to numeric matrix", call.=FALSE)
    }
    if(!quiet) cat("\nConverted data.frame", name, "to matrix\n")
  }
  # Final check if element is numeric
  if(!is.numeric(x)){
    stop("Element ", name, " is not numeric", call.=FALSE)
  }
  x
}


# Check parameter vector-------------------------------------------------------
check_params <- function(params, DIC){
  if(!(is.vector(params) & is.character(params))){
    stop("parameters.to.save must be a character vector", call.=FALSE)
  }
  if(DIC & (! "deviance" %in% params)){
    params <- c(params, "deviance")
  }
  params
}


# Check mcmc settings----------------------------------------------------------
#names(mcmc.info) <- c('n.chains','n.adapt','sufficient.adapt','n.iter','n.burnin','n.thin','n.samples','end.values','elapsed.mins')

check_mcmc_info <- function(n_chains, n_adapt, n_iter, n_burnin, 
                            n_thin, n_cores, parallel){

  if(n_iter <= n_burnin){
    stop("Number of iterations must be larger than burn-in", call.=FALSE)
  }
  # Removed warnings about small numbers of iterations and uneven iterations
  
  n_cores = check_cores(n_cores, n_chains, parallel)

  # Create list structure and save available elements
  out <- list(n.chains = n_chains, n.adapt = n_adapt, sufficient.adapt = NA,
       n.iter = n_iter, n.burnin = n_burnin, n.thin = n_thin,
       n.samples = NA, end.values = NA, elapsed.mins = NA)
  if(parallel) out$n.cores <- n_cores
  out
}


# Check number of cores--------------------------------------------------------
check_cores <- function(n_cores, n_chains, parallel){
  if(!parallel) return(NULL)
  max_cores <- parallel::detectCores()
  # Send this warning right away
  old_warn <- options()$warn
  options(warn=1)
  if(!is.null(n_cores)){
    if(n_cores > max_cores){
      n_cores <- max_cores
      warning("n.cores > max available cores. n.cores set to ", max_cores, 
              call.=FALSE)
    }
  } else {
    if(!is.na(max_cores)){
      n_cores <- min(max_cores, n_chains)
    } else {
      warning("Couldn't detect number of cores. Setting n.cores = n.chains", 
            call.=FALSE)
      n_cores <- n_chains
    }  
  }
  options(warn=old_warn)
  n_cores
}


# Check initial values---------------------------------------------------------
check_inits <- function(inits, n_chains){
  if(is.list(inits)){
    if(length(inits) != n_chains){
      stop("inits list must have length equal to the number of chains",
           call.=FALSE)
    }
  } else if(is.function(inits)){
    inits <- lapply(1:n_chains, function(x) inits())
    if(!is.list(inits[[1]])){
      stop("inits function must return list", call.=FALSE)
    }
  } else if(is.null(inits)){
    inits <- vector("list", n_chains)
  } else {
    stop("inits must be a list or a function that returns a list", call.=FALSE)
  }

  # Setup seeds in each chain, for reproducibility
  # Check if they already exist
  has_RNG <- all(c(".RNG.name",".RNG.seed") %in% names(inits[[1]]))
  # If not add them
  if(!has_RNG){
    # Generate random seeds for each chain
    chain_seeds <- floor(stats::runif(n_chains, 1, 100000))
    for (i in 1:n_chains){
      inits[[i]]$.RNG.name="base::Mersenne-Twister"
      inits[[i]]$.RNG.seed=chain_seeds[i]
    }
  }
  inits
}
