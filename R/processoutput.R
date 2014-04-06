
process.output <- function(x,n.chains=n.chains,n=n){
  
  params <- colnames(x[[1]])
  nparams <- length(params)
  m <- n.chains

  sims.list <- as.list(params)
  names(sims.list) <- params
  rhat <- vector(length=nparams)
  n.eff <- vector(length=nparams)
  names(rhat) <- params
  for (i in 1:nparams){
    sims.list[[i]] <- unlist(x[,i])
    if(n.chains>1){rhat[i] <- gelman.diag(x[,i])$psrf[1]
    } else{rhat[i] = NA}
    
    xp <- matrix(sims.list[[i]],nrow=n,ncol=m)
    xdot <- apply(xp,2,mean)
    s2 <- apply(xp,2,var)
    W <- mean(s2)
    
    #Non-degenerate case
    if ((W > 1.e-8) && (n.chains > 1)) {
      B <- n*var(xdot)
      sig2hat <- ((n-1)*W + B)/n      
      n.eff[i] <- m*n*min(sig2hat/B,1)
    } else {
      n.eff[i] <- 1
    }   
  }
  
  means <- unlist(lapply(sims.list,mean))
  se <- unlist(lapply(sims.list,sd))
  qs <- function(x,y){as.numeric(quantile(x,y))}
  q2.5 <- unlist(lapply(sims.list,qs,0.025))
  q50 <- unlist(lapply(sims.list,qs,0.5))
  q97.5 <- unlist(lapply(sims.list,qs,0.975))
  
  overlap0 <- vector(length=nparams)
  f <- vector(length=nparams)
  names(overlap0) <- params
  names(f) <- params
  for (i in 1:nparams){
    overlap0[i] <- findInterval(0,c(q2.5[i],q97.5[i]))==1
    if(means[i]>=0){f[i] <- mean(unlist(x[,i])>=0)
    }else{f[i] <- mean(unlist(x[,i])<0)}
  }

  return(list(sims.list=sims.list,means=means,se=se,q2.5=q2.5,q50=q50,q97.5=q97.5,overlap0=overlap0,f=f,Rhat=rhat,n.eff=n.eff))
}