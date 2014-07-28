
run.model <- function(model.file,data,inits,parameters.to.save,n.chains,n.iter,n.burnin,n.thin,n.adapt,verbose=TRUE){

if(verbose){pb="text"} else {pb="none"}  
  
#Compile model 
m <- jags.model(file=model.file,data=data,inits=inits,n.chains=n.chains,n.adapt=0)

#Adaptive phase using adapt()
if(n.adapt>0){
  if(verbose){
  cat('Adaptive phase,',n.adapt,'iterations x',n.chains,'chains','\n')
  cat('If no progress bar appears JAGS has decided not to adapt','\n','\n')
  }
  x <- adapt(object=m,n.iter=n.adapt,progress.bar=pb,end.adaptation=TRUE)
} else{if(verbose){cat('No adaptive period specified','\n','\n')}
       #If no adaptation period specified:
       #Force JAGS to not adapt (you have to allow it to adapt at least 1 iteration)
       x <- adapt(object=m,n.iter=1,end.adaptation=TRUE)
}

#Burn-in phase using update()  
if(n.burnin>0){
  if(verbose){cat('\n','Burn-in phase,',n.burnin,'iterations x',n.chains,'chains','\n','\n')}
  update(object=m,n.iter=n.burnin,progress.bar=pb)
  if(verbose){cat('\n')}
} else if(verbose){cat('No burn-in specified','\n','\n')}

#Sample from posterior using coda.samples() 
if(verbose){cat('Sampling from joint posterior,',(n.iter-n.burnin),'iterations x',n.chains,'chains','\n','\n')}
samples <- coda.samples(model=m,variable.names=parameters.to.save,n.iter=(n.iter-n.burnin),thin=n.thin,
                        progress.bar=pb)
if(verbose){cat('\n')}

return(list(m=m,samples=samples))
}