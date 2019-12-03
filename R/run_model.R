run_model <- function(inp, quiet){

  inp$run.info$start.time <- Sys.time()

  if(inp$run.info$parallel){
    out <- run_parallel(inp, quiet)
  } else {
    out <- call_rjags(inp, quiet)
  }

  inp$run.info$end.time <- Sys.time()
  inp$mcmc.info$n.samples <- nrow(out$samples[[1]]) * length(out$samples)
  inp$mcmc.info$sufficient.adapt <- all(out$sufficient.adapt)

  #Reorganize
  out$samples <- order_samples(out$samples, inp$parameters)
  out$sufficient.adapt <- NULL
  inp$data <- NULL
  inp$model <- NULL

  c(out, inp)

}
