
pp.check <- function(x, observed, simulated, xlab=NULL, ylab=NULL, main=NULL, ...){

  check_class(x)

  obs <- eval(parse(text=paste('x$sims.list$',observed,sep="")))
  if(is.null(obs))
    stop("MCMC chain ", deparse(substitute(observed)), 
         " not found.", call.=FALSE)
  sim <- eval(parse(text=paste('x$sims.list$',simulated,sep="")))
  if(is.null(sim))
    stop("MCMC chain ", deparse(substitute(simulated)), 
         " not found.", call.=FALSE)

    bpval <- mean(sim > obs)

  if(is.null(xlab)){
    xlab <- 'Observed Data'
  }
  if(is.null(ylab)){
    ylab <- 'Simulated Data'
  }
  if(is.null(main)){
    main <- paste('Posterior Predictive Check')
  }

  plotrange <- range(obs, sim)

  graphics::plot(x = obs, y = sim, xlab=xlab, ylab=ylab, main=main,
       xlim=plotrange,ylim=plotrange, ...)
  graphics::abline(0,1, lwd=2)
  where <- if(bpval < 0.5) 'topleft' else 'bottomright'
  graphics::legend(where, paste("P =", round(bpval,2)), bty='n', cex=1.5)

  return(bpval)
}

