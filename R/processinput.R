data.check <- function(x,name){
  
  test = FALSE
    
  if(is.data.frame(x)){
    if(!is.null(dim(x))){
      cat('\nConverting data frame \'',name,'\' to matrix.\n',sep="")
      x = as.matrix(x)
    } else {
      cat('\nConverting data frame',name,'to vector.\n')
      x = as.vector(x)}
  }
  
  
  if (is.numeric(x)&&is.matrix(x)){
    if(1%in%dim(x)){
      cat('\nConverting 1-column matrix \'',name,'\' to vector\n',sep="")
      x = as.vector(x)
    }
    test = TRUE
  }
  
  if(is.numeric(x)&&is.array(x)&&!test){
    test = TRUE
  }
  
  if (is.numeric(x)&&is.vector(x)&&!test){
    test = TRUE
  }
  
  if(test){
    return(x)
  } else{return('error')}
  
}


process.input = function(x,y,DIC=FALSE){
  cat('\nProcessing function input.......','\n')
  
  #Check if supplied parameter vector is the right format
  if((is.character(y)&is.vector(y))){
      } else{stop('The parameters to save must be a vector containing only character strings.\n')}
  
  #If DIC requested, add deviance to parameters (if not already there)
  #and start JAGS DIC module
  if(DIC){
    load.module("dic",quiet=TRUE)
    if(!'deviance'%in%y){
      params <- c(y,"deviance")
    } else {params <- y}    
  } else {params <- y}
  
  #Check if supplied data object is the proper format
  if(is.list(x)||(is.character(x)&is.vector(x))){
  } else{stop('Input data must be a list of data objects OR a vector of data object names (as strings)\n')}
  
  options( warn = -1 )
  if(is.list(x)&&all(lapply(x,is.character))){
    x = unlist(x)
  }
  options(warn=0)
  
  
  if((is.list(x)&&is.null(names(x)))||(is.list(x)&&any(names(x)==""))){
    stop('At least one of the elements in your data list does not have a name\n')
  }
  
  #Convert a supplied vector of characters to a list of data objects
  if((is.character(x)&is.vector(x))){    
    temp = lapply(x,get)
    names(temp) = x
    x = temp  
  }
  
  #Check each component of data object for issues and fix if possible
  for (i in 1:length(x)){
  
    if(is.factor(x[[i]])){
           
      stop('\nElement \'',names(x[i]) ,'\' in the data list is a factor.','\n','Convert it to a series of dummy/indicator variables or a numeric vector as appropriate.\n')
            
    }
     
    process <- data.check(x[[i]],name = names(x[i]))
    if(process[1]=="error"){stop('\nElement \'',names(x[i]) ,'\' in the data list cannot be coerced to one of the','\n','allowed formats (numeric scalar, vector, matrix, or array)\n')
    } else{x[[i]] <- process}

  }
 
  cat('\nDone.','\n','\n')
  return(list(data=x,params=params))
   
}