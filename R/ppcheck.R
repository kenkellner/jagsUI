
pp.check <- function(x, observed, simulated, 
                     xlab='Observed data', ylab='Simulated data', 
                     main='Posterior Predictive Check', ...){

  check_class(x)
  if(is.null(match_params(observed, param_names(x$samples)))){
    stop("Observed parameter not found in output")
  }
  if(is.null(match_params(simulated, param_names(x$samples)))){
    stop("Simulated parameter not found in output")
  }

  obs <- c(mcmc_to_mat(x$samples, observed))
  sim <- c(mcmc_to_mat(x$samples, simulated))

  bpval <- mean(sim > obs)
  plotrange <- range(obs, sim)

  graphics::plot(x = obs, y = sim, xlab=xlab, ylab=ylab, main=main,
       xlim=plotrange,ylim=plotrange, ...)
  graphics::abline(0,1, lwd=2)
  where <- if(bpval < 0.5) 'topleft' else 'bottomright'
  graphics::legend(where, paste("P =", round(bpval,2)), bty='n', cex=1.5)

  return(bpval)
}

