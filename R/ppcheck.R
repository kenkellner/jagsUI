
pp.check <- function(x, observed, simulated, 
                     xlab='Observed data', ylab='Simulated data', 
                     main='Posterior Predictive Check', ...){

  check_class(x)
  check_parameter(observed, x$samples)
  check_parameter(simulated, x$samples)

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

