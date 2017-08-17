
summary.jagsUI <- function(object,digits=3,...){
  
  cat('Summary for model \'',object$modfile,'\'','\n',sep="")
  cat('Saved parameters:',object$parameters,'\n')
  if(!object$parallel){cat('MCMC ran for ',object$mcmc.info$elapsed.mins,' minutes at time ',paste(object$run.date),'.\n','\n',sep="")
  } else{cat('MCMC ran in parallel for ',object$mcmc.info$elapsed.mins,' minutes at time ',paste(object$run.date),'.\n','\n',sep="")}
  
  cat('For each of',object$mcmc.info$n.chains,'chains:\n')
  if(all(object$mcmc.info$sufficient.adapt)){cat('Adaptation:           ',mean(object$mcmc.info$n.adapt),'iterations (sufficient)\n')
  }else{cat('Adaptation:           ',mean(object$mcmc.info$n.adapt),'iterations (possibly insufficient)\n')}
  cat('Burn-in:              ',object$mcmc.info$n.burnin,'iterations\n')
  cat('Thin rate:            ',object$mcmc.info$n.thin,'iterations\n')
  cat('Total chain length:   ',object$mcmc.info$n.iter+mean(object$mcmc.info$n.adapt),'iterations\n')
  cat('Posterior sample size:',object$mcmc.info$n.samples/object$mcmc.info$n.chains,'draws\n\n')
  
  if(object$mcmc.info$n.chains>1){
    if(max(unlist(object$Rhat),na.rm=TRUE)>1.1){cat('**WARNING** Rhat values indicate convergence failure.','\n')
      }else{cat('Successful convergence based on Rhat values (all < 1.1).','\n')}
  }
  
  if(object$DIC){    
    cat('\nDIC info: (pD = var(deviance)/2)','\npD =',round(object$pD,1),'and DIC =',round(object$DIC,digits),'\n')
  }
  
}