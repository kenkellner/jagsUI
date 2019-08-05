## MASTER FUNCTION TO PROCESS INPUT--------------------------------------------

process_input <- function(data, inits, params, n.chains,
                          n.adapt, n.iter, n.burnin, n.thin, n.cores, DIC,
                          parallel){
  
  mcmc_info <- mget(c("n.chains", "n.adapt", "n.iter", "n.burnin", "n.thin",
                      "parallel", "n.cores"))

  list(data = check_data(data), inits = get_inits(inits, n.chains), 
       params = check_params(params, DIC),
       mcmc_info = check_mcmc_info(mcmc_info))

}

## PARAM PROCESSING------------------------------------------------------------

#Check if parameters to save are correct
check_params <- function(inp, DIC){
  #Error if parameters to save is not character vector
  if(!is.character(inp)) stop("Parameters to save must be character vector")

  #Add deviance to list of parameters to save if it isn't present
  #TODO: check if this is actually necessary?
  if(DIC & !("deviance" %in% inp)) inp <- c(inp, "deviance")
  if(!DIC & ("deviance" %in% inp)) inp <- inp[inp != "deviance"]
  inp
}

## DATA PROCESSING-------------------------------------------------------------

#Get final data to send to JAGS
#Check data list validity
check_data <- function(inp){
  if(!is.list(inp)){
    stop(paste0('Input data is class ',class(inp),
                ', it should be a named list'))
  }
  if(length(inp)==0) stop("Data list is empty")
  
  nms <- names(inp)
  if(is.null(nms) | any(nms == "")){
    stop("All elements of data list must be named")
  }

  not_num <- names(inp)[which(!sapply(inp, is.numeric))]
  if(length(not_num != 0)){
    stop(paste0("Data list element(s) [", paste(not_num, collapse=", "),
                "] are not numeric"))
  }

  inp
}

## MCMC SETTINGS PROCESSING----------------------------------------------------

check_parallel <- function(inp){
  
  avail_cores <- parallel::detectCores()   
  if(is.null(avail_cores)) stop("Unable to detect number of available cores")

  if(is.null(inp$n.cores)){
    inp$n.cores <- min(inp$n.chains, avail_cores)
  } else if(inp$n.cores > avail_cores){
    stop(paste0('More cores requested (',inp$n.cores,') than available (',
                    avail_cores,')'))
  }
 
  inp
}

check_mcmc_info <- function(inp){
  if(inp$n.iter <= inp$n.burnin){
    stop("Number of iterations must be larger than burn-in")
  }

  if(inp$parallel) inp <- check_parallel(inp)

  inp
}
