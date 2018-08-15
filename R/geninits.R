
gen.inits <- function(inits,n.chains,seed,parallel){
  
  if(!is.null(seed)){
    
    #Save old seed if it exists
    if(exists('.Random.seed')){
      old.seed <- .Random.seed
    }   
    #Generate seed for each chain
    set.seed(seed)

  }

  #Error check and run init function if necessary
  if(is.list(inits)){
    if(length(inits)!=n.chains){stop('Length of initial values list != number of chains')}
    init.values <- inits
  } else if(is.function(inits)){
    init.values <- list()
    for (i in 1:n.chains){
      init.values[[i]] <- inits()
    }
  } else if(is.null(inits)){
    init.values <- NULL
    
  } else {stop('Invalid initial values. Must be a function or a list with length=n.chains')}
  
  #Add random seed info if specified
  if(!is.null(seed)){
    
    init.rand <- floor(runif(n.chains,1,100000))
    
    #Restore old seed if it exists
    if(exists('old.seed')){
      assign(".Random.seed", old.seed, pos=1)
    }
    
    #Add random seeds to inits
    if(is.null(inits)){
      init.values <- vector("list",length=n.chains)
      for(i in 1:n.chains){
        init.values[[i]]$.RNG.name="base::Mersenne-Twister"
        init.values[[i]]$.RNG.seed=init.rand[i]
      }
      
    } else if(is.list(init.values)){
        for(i in 1:n.chains){
          init.values[[i]]$.RNG.name="base::Mersenne-Twister"
          init.values[[i]]$.RNG.seed=init.rand[i]
        }
      
    } else if (is.function(inits)){
        for (i in 1:n.chains){
          init.values[[i]]$.RNG.name="base::Mersenne-Twister"
          init.values[[i]]$.RNG.seed=init.rand[i]
        }
      
    } 
 
    
  #If seed is not set
  } else {
    
    other.RNG <- all(c(".RNG.name",".RNG.seed")%in%names(init.values[[1]]))
    
    needs.RNG <- is.null(init.values)|!other.RNG
    
    #If parallel and no custom RNG has been set, add one. Otherwise all chains will start with same seed.
    if(needs.RNG&parallel){
      
      init.rand <- floor(runif(n.chains,1,100000))
      
      if(is.null(init.values)){init.values <- vector("list",length=n.chains)}

      for(i in 1:n.chains){
        init.values[[i]]$.RNG.name="base::Mersenne-Twister"
        init.values[[i]]$.RNG.seed=init.rand[i]
      }
      
    }
  }

  return(init.values) 
}
