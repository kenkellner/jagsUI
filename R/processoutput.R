
process.output <- function(x,n.chains=n.chains,n=n){
  
  #Full set of parameter names (separate indexed parameters)
  params <- colnames(x[[1]])
  #Strip indexes from parameter names
  expand <- sapply(strsplit(params, "\\["), "[", 1)
  #Collapse indexed parameters to single name
  params.simple <- unique(sapply(strsplit(params, "\\["), "[", 1))
  
  #Count indexes for use later
  nparams <- length(params.simple)
  #Determine number of iterations
  iter <- length(unlist(x[,1]))
  m <- n.chains
  n <- iter / m

  #Create empty named lists for statistics
  sims.list <- means <- rhat <- n.eff <- se <- as.list(params.simple)
  q2.5 <- q50 <- q97.5 <- overlap0 <- f <- as.list(params.simple)
  names(sims.list) <- names(means) <- names(rhat) <- names(n.eff) <- params.simple
  names(se) <- names(q2.5) <- names(q50) <- names(q97.5) <- params.simple
  names(overlap0) <- names(f) <- params.simple
  
  #Get dimensions of indexed parameters if any
  dims <- get.dim(params)

  
  #Make quantile function
  qs <- function(x,y){as.numeric(quantile(x,y))}
  ov <- function(x){findInterval(0,c(qs(x,0.025),qs(x,0.975)))==1}
  gf <- function(x){if(mean(x)>=0){f[i] <- mean(x>=0)}else{mean(x<0)}}
  calcneff <- function(x,n,m){
    xp <- matrix(x,nrow=n,ncol=m)
    xdot <- apply(xp,2,mean)
    s2 <- apply(xp,2,var)
    W <- mean(s2)
    
    #Non-degenerate case
    if ((W > 1.e-8) && (m > 1)) {
      B <- n*var(xdot)
      sig2hat <- ((n-1)*W + B)/n      
      n.eff <- m*n*min(sig2hat/B,1)
    } else {
      n.eff <- 1
    }
    n.eff
  }
  
  #Iterate through parameters and calculate stats
  for (i in 1:nparams){
    
    #If parameter is indexed
    if(!is.na(dims[[i]][1])){
      hold <- x[,expand==params.simple[i]]

      
      if(n.chains > 1){
      rhat[[i]] <- array(as.numeric(gelman.diag(hold)$psrf[,1]),
                         dim=dims[[i]])}
      
      combined <- do.call("rbind",hold)
      sims.list[[i]] <- array(combined,dim=c(iter,dims[[i]]))
      ld <- length(dim(sims.list[[i]]))
      means[[i]] <- colMeans(sims.list[[i]])
      se[[i]] <- apply(sims.list[[i]],c(2:ld),sd)
      q2.5[[i]] <- apply(sims.list[[i]],c(2:ld),qs,0.025)
      q50[[i]] <- apply(sims.list[[i]],c(2:ld),qs,0.5)
      q97.5[[i]] <- apply(sims.list[[i]],c(2:ld),qs,0.975)
      overlap0[[i]] <- apply(sims.list[[i]],c(2:ld),ov)
      f[[i]] <- apply(sims.list[[i]],c(2:ld),gf)
      n.eff[[i]] <- apply(sims.list[[i]],c(2:ld),calcneff,n,m)
       
    #If not
    } else {
      hold <- x[,params.simple[i]]
      if(n.chains > 1){rhat[i] <- gelman.diag(hold)$psrf[1]}
      sims.list[[i]] <- unlist(hold)
      means[[i]] <- mean(sims.list[[i]])
      se[[i]] <- sd(sims.list[[i]])
      q2.5[[i]] <- qs(sims.list[[i]],0.025)
      q50[[i]] <- qs(sims.list[[i]],0.5)
      q97.5[[i]] <- qs(sims.list[[i]],0.975)
      overlap0[[i]] <- ov(sims.list[[i]])
      f[[i]] <- gf(sims.list[[i]])
      n.eff[[i]] <- calcneff(sims.list[[i]],n,m)
    }
        
  }


  return(list(sims.list=sims.list,means=means,se=se,q2.5=q2.5,q50=q50,q97.5=q97.5,overlap0=overlap0,
              f=f,Rhat=rhat,n.eff=n.eff))
}