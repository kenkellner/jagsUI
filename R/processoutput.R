
process.output <- function(x,n.chains=n.chains,DIC=FALSE){
  
  #Full set of parameter names
  params <- colnames(x[[1]])
  #Strip index from non-scalar parameter names
  expand <- sapply(strsplit(params, "\\["), "[", 1)
  #Collapse non-scalar parameters into single name
  params.simple <- unique(sapply(strsplit(params, "\\["), "[", 1))
  
  #Count indexes for use later
  nparams <- length(params.simple)
  #Determine number of iterations per chain
  iter <- length(unlist(x[,1]))
  m <- n.chains
  n <- iter / m

  #Create empty named lists for statistics
  sims.list <- means <- rhat <- n.eff <- se <- as.list(params.simple)
  q2.5 <- q25 <- q50 <- q75 <- q97.5 <- overlap0 <- f <- as.list(params.simple)
  names(sims.list) <- names(means) <- names(rhat) <- names(n.eff) <- params.simple
  names(se) <- names(q2.5) <- names(q50) <- names(q97.5) <- params.simple
  names(overlap0) <- names(f) <- params.simple
  
  #Get dimensions of non-scalar parameters if any
  dims <- get.dim(params)
  
  #Quantile function
  qs <- function(x,y){as.numeric(quantile(x,y))}
  #Overlap 0 function
  ov <- function(x){findInterval(0,c(qs(x,0.025),qs(x,0.975)))==1}
  #f function (proportion of posterior with same sign as mean)
  gf <- function(x){if(mean(x)>=0){f[i] <- mean(x>=0)}else{mean(x<0)}}
  #n.eff function
  calcneff <- function(x,n,m){
    xp <- matrix(x,nrow=n,ncol=m)
    xdot <- apply(xp,2,mean)
    s2 <- apply(xp,2,var)
    W <- mean(s2)
    
    #Non-degenerate case
    if ((W > 1.e-8) && (m > 1)) {
      B <- n*var(xdot)
      sig2hat <- ((n-1)*W + B)/n      
      n.eff <- round(m*n*min(sig2hat/B,1),0)
    #Degenerate case
    } else {
      n.eff <- 1
    }
    n.eff
  }
  
  #Iterate through parameters and calculate stats
  for (i in 1:nparams){
    
    #If non-scalar parameter
    #Treated separately to compile statistics for each vector/matrix/array into
    #corresponding summary vector/matrix/array (rather than treating each entry in non-scalar
    #as a completely separate parameter)
    if(!is.na(dims[[i]][1])){
      #Compile all entries in vector/matrix/array together
      hold <- x[,expand==params.simple[i]]

      #Get Rhat values if more than 1 chain    
      if(n.chains > 1){
        rhat.temp <- vector(length=dim(hold[[1]])[2])
        #Iterate through all vector/matrix/array entries individually 
        #Necessary to avoid Rhat calculation errors
        for (j in 1:(dim(hold[[1]])[2])){
          rhat.temp[j] <- gelman.diag(hold[,j])$psrf[1]
        }
        rhat[[i]] <- array(rhat.temp,dim=dims[[i]])
      }
      #Combine posterior samples for each entry in non-scalar parameter
      #Into single matrix/array (nrows=samples)
      combined <- do.call("rbind",hold)
      sims.list[[i]] <- array(combined,dim=c(iter,dims[[i]]))
      #Calculate statistics for each entry in non-scalar and combine
      ld <- length(dim(sims.list[[i]]))
      means[[i]] <- colMeans(sims.list[[i]])
      se[[i]] <- apply(sims.list[[i]],c(2:ld),sd)
      q2.5[[i]] <- apply(sims.list[[i]],c(2:ld),qs,0.025)
      q25[[i]] <- apply(sims.list[[i]],c(2:ld),qs,0.25)
      q50[[i]] <- apply(sims.list[[i]],c(2:ld),qs,0.5)
      q75[[i]] <- apply(sims.list[[i]],c(2:ld),qs,0.75)
      q97.5[[i]] <- apply(sims.list[[i]],c(2:ld),qs,0.975)
      overlap0[[i]] <- apply(sims.list[[i]],c(2:ld),ov)
      f[[i]] <- apply(sims.list[[i]],c(2:ld),gf)
      n.eff[[i]] <- apply(sims.list[[i]],c(2:ld),calcneff,n,m)
       
    #If parameter is a scalar
    } else {
      #Get all samples
      hold <- x[,params.simple[i]]
      #Calculate statistics
      if(n.chains > 1){rhat[i] <- gelman.diag(hold)$psrf[1]}
      sims.list[[i]] <- unlist(hold)
      means[[i]] <- mean(sims.list[[i]])
      se[[i]] <- sd(sims.list[[i]])
      q2.5[[i]] <- qs(sims.list[[i]],0.025)
      q25[[i]] <- qs(sims.list[[i]],0.25)
      q50[[i]] <- qs(sims.list[[i]],0.5)
      q75[[i]] <- qs(sims.list[[i]],0.75)
      q97.5[[i]] <- qs(sims.list[[i]],0.975)
      overlap0[[i]] <- ov(sims.list[[i]])
      f[[i]] <- gf(sims.list[[i]])
      n.eff[[i]] <- calcneff(sims.list[[i]],n,m)
    }
        
  }
  
  #Do DIC/pD calculations if requested by user
  if(DIC){
    dev <- matrix(data=unlist(x[,"deviance"]),ncol=n.chains,nrow=n)   
    pd <- numeric(n.chains)
    dic <- numeric(n.chains)    
    for (i in 1:n.chains){
      pd[i] <- var(dev[,i])/2
      dic[i] <- mean(dev[,i]) + pd[i]
    }    
    pd <- mean(pd)
    dic <- mean(dic)
    
    #Return this list if DIC/pD requested
    return(list(sims.list=sims.list,means=means,se=se,q2.5=q2.5,q25=q25,q50=q50,q75=q75,q97.5=q97.5,overlap0=overlap0,
                f=f,Rhat=rhat,n.eff=n.eff,pD=pd,DIC=dic))
  } else {
      #Otherwise return list without pD/DIC
      return(list(sims.list=sims.list,means=means,se=se,q2.5=q2.5,q25=q25,q50=q50,q75=q75,q97.5=q97.5,overlap0=overlap0,
              f=f,Rhat=rhat,n.eff=n.eff))
  }
}