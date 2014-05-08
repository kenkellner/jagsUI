
print.simplebugs <- function(x,digits=3){
  cat('Inference for Bugs model at \'',x$modfile,'\', fit using JAGS,','\n',sep="")
  cat(round(x$mcmc.info[[1]],0),'chains, each with',round(x$mcmc.info[[3]],0),'iterations (first ',round(x$mcmc.info[[4]],0),'discarded), n.thin =',round(x$mcmc.info[[5]],0)) 
  cat('\nn.sims = ',round(x$mcmc.info[[6]],0),'iterations saved','\n')

  y = data.frame(unlist(x$mean),unlist(x$sd),unlist(x$q2.5),unlist(x$q25),unlist(x$q50),unlist(x$q75),unlist(x$q97.5),
                 unlist(x$Rhat),unlist(x$n.eff)) 
  row.names(y) = colnames(x$samples[[1]])
  names(y) = c('mean','sd','2.5%','25%','50%','75%','97.5%','Rhat','n.eff')
  if(x$mcmc.info[[1]]==1){
  y = y[,-c(8,9)]
  }
  #get the rounding to look clean, move deviance to bottom
  z <-  as.data.frame(round(as.matrix(y),digits))
  print(z)
  
  if(x$mcmc.info[[1]]>1){
  cat('\nFor each parameter, n.eff is a crude measure of effective sample size,','\n')
  cat('and Rhat is the potential scale reduction factor (at convergence, Rhat=1).','\n')
  
  }
  
  if(!is.null(x$DIC)){
    
    cat('\nDIC info (using the rule, pD = var(deviance)/2)','\npD =',round(x$pD,1),'and DIC =',round(x$DIC,digits),'\n')
    cat('DIC is an estimate of expected predictive error (lower deviance is better).\n')
    
  }
}