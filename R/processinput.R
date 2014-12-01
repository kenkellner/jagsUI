
process.input = function(x,y,inits,n.chains,n.iter,n.burnin,n.thin,DIC=FALSE){
  cat('\nProcessing function input.......','\n')
  
  #Quality control
  if(n.iter<=n.burnin){
    stop('Number of iterations must be larger than burn-in.\n')
  }
  
  if(n.thin>1&&(n.iter-n.burnin)<10){
    options(warn=1)
    warning('The number of iterations is very low; jagsUI may crash. Recommend reducing n.thin to 1 and/or increasing n.iter.')
    options(warn=0,error=NULL)  
  }
  
  final.chain.length <- (n.iter - n.burnin) / n.thin
  even.length <- floor(final.chain.length) == final.chain.length
  if(!even.length){
    options(warn=1)
    warning('Number of iterations saved after thinning is not an integer; JAGS will round it up.')
    options(warn=0,error=NULL)  
  }
  
  #Check if supplied parameter vector is the right format
  if((is.character(y)&is.vector(y))){
      } else{stop('The parameters to save must be a vector containing only character strings.\n')}
  
  #If DIC requested, add deviance to parameters (if not already there)
  if(DIC&&(!'deviance'%in%y)){
      params <- c(y,"deviance")
  } else {params <- y}    
  
  #Check if supplied data object is the proper format
  if(is.list(x)||(is.character(x)&is.vector(x))){
  } else{stop('Input data must be a list of data objects OR a vector of data object names (as strings)\n')}
  
  if(is.list(x)&&all(sapply(x,is.character))){
    x = unlist(x)
  }
  
  if((is.list(x)&&is.null(names(x)))||(is.list(x)&&any(names(x)==""))){
    stop('At least one of the elements in your data list does not have a name\n')
  }
  
  #Convert a supplied vector of characters to a list of data objects
  if((is.character(x)&is.vector(x))){    
    temp = lapply(x,get,envir = parent.frame(2))
    names(temp) = x
    x = temp  
  }
  
  #Check each component of data object for issues and fix if possible
  for (i in 1:length(x)){
  
    if(is.factor(x[[i]])){
           
      stop('\nElement \'',names(x[i]) ,'\' in the data list is a factor.','\n','Convert it to a series of dummy/indicator variables or a numeric vector as appropriate.\n')
            
    }
     
    process <- data.check(x[[i]],name = names(x[i]))
    if(!is.na(process[1])&&process[1]=="error"){stop('\nElement \'',names(x[i]) ,'\' in the data list cannot be coerced to one of the','\n','allowed formats (numeric scalar, vector, matrix, or array)\n')
    } else{x[[i]] <- process}

  }
  
  #Get initial values
  init.vals <- gen.inits(inits,n.chains)
 
  cat('\nDone.','\n','\n')
  return(list(data=x,params=params,inits=init.vals))
   
}