run_parallel <- function(inp, quiet){
  
  ri <- inp$run.info
  mc <- inp$mcmc.info

  #Output object
  out <- list()

  #Set up cluster
  #Use FORK if on linux
  ctype <- 'PSOCK'
  if(.Platform$OS.type=='unix') ctype <- 'FORK'

  #Set up logfile
  write_html <- FALSE
  show_html <- .Platform$OS.type=='windows'
  logfile <- NULL
  if(.Platform$OS.type=='unix' && isatty(stdout())){
    logfile <- ""
  } else if(!quiet && !identical(browser <- getOption("browser"), "false")){
    write_html <- TRUE
    logfile <- tempfile()
    template <- file.path(system.file('misc', package = 'jagsUI'), 
                          'progress_template.html')
    src <- paste(readLines(template), collapse = '\n')
    src <- sub("%filename%", basename(logfile), src, fixed = TRUE)
    
    prog_file <- paste0(logfile, "_JAGS_Progress.html")
    cat(src, file = prog_file)
  }

  if(!quiet){ 
    cat('Initializing cluster\n')
    if(write_html){
      cat(paste0("Writing progress updates to file ", prog_file, "\n"))
      if(show_html){
        cat("Attempting to open the file in your web browser...\n")
        utils::browseURL(paste0("file://", prog_file))
      }
    }
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
  if(write_html) cat("Done\n", file = logfile, append = TRUE)

  #Format output
  out$samples <- lapply(lapply(out_raw,`[[`,1),`[[`,1)
  class(out$samples) <- 'mcmc.list'
  out$model <- lapply(out_raw,`[[`,2)
  out$sufficient.adapt <- unlist(lapply(out_raw,`[[`,3))
  
  out

}
