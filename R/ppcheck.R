
pp.check <- function(x, observed, simulated, xlab=NULL, ylab=NULL, main=NULL){
  if(class(x)!="jagsUI"){stop('Requires jagsUI object as input')}
  devAskNewPage(ask=FALSE)
  observed <- eval(parse(text=paste('x$sims.list$',observed,sep="")))
  simulated <- eval(parse(text=paste('x$sims.list$',simulated,sep="")))
  
  bpval <- mean(simulated>observed)

  if(is.null(xlab)){
    xlab <- 'Observed Data'
  }
  if(is.null(ylab)){
    ylab <- 'Simulated Data'
  }
  if(is.null(main)){
    main <- paste('Posterior Predictive Check','\n',
                  'Bayesian P-value =',round(bpval,2))
  }

  minval <- min(c(observed,simulated))
  maxval <- max(c(observed,simulated))
  plotrange <- c(minval,maxval)
  
  plot(x = observed, y = simulated, xlab=xlab, ylab=ylab, main=main, 
       xlim=plotrange,ylim=plotrange)
  abline(1,1)
  

}
