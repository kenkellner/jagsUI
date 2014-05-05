
process.input = function(x,y,DIC=FALSE){
  cat('\nProcessing function input.......','\n')
  
  #Check if supplied parameter vector is the right format
  if((is.character(y)&is.vector(y))){
      } else{stop('The parameters to save must be a vector containing only character strings.')}
  
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
  } else{stop('Input data must be a list of data objects OR a vector of data object names (as strings)')}
  
  if((is.list(x)&&is.null(names(x)))||(is.list(x)&&any(names(x)==""))){
    stop('At least one of the elements in your data list does not have a name')
  }
  
  #Convert a supplied vector of characters to a list of data objects
  if((is.character(x)&is.vector(x))){    
    temp = lapply(x,get)
    names(temp) = x
    x = temp  
  }
  
  #Check each component of data object for issues and fix if possible
  #for (i in 1:length(x)){
    
    #if(is.list(x[[i]])&&length(x[[i]]==1)&&!is.null(dim(x[[i]]))){
    #  cat('Note: data component',names(x[i]),'is a 1-element list and is being converted to numeric.','\n')
    #  x[[i]] = x[[i]][[1]]
    #}
    
    #if(is.matrix(x[[i]])&&ncol(x[[i]])==1){
    #  cat('Note: data component',names(x[i]),'is a 1-column matrix and is being converted to vector.','\n')
    #  x[[i]] = x[[i]][,1]
    #}  
        
    #if(!is.numeric(x[[i]])){
    #  stop('Data component ',names(x[i]),' is not numeric. Try coercing it with as.numeric().')
    #}
                
    #}
 
  cat('Done.','\n','\n')
  return(list(data=x,params=params))
   
}