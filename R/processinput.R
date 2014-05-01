
process.input = function(x,y,DIC=FALSE){
  cat('\n','Processing function input.......','\n')
  
  if((is.character(y)&is.vector(y))){
      } else{stop('The parameters to save must be a vector containing only character strings.')}
  
  if(DIC){
    load.module("dic",quiet=TRUE)
    if(!'deviance'%in%y){
      params <- c(y,"deviance")
    } else {params <- y}    
  } else {params <- y}
  
  if(is.list(x)||(is.character(x)&is.vector(x))){
  } else{stop('Input data must be a list of data objects OR a vector of data object names (as strings)')}
  
  if((is.list(x)&&is.null(names(x)))||(is.list(x)&&any(names(x)==""))){
    stop('At least one of the elements in your data list does not have a name')
  }
  
  if((is.character(x)&is.vector(x))){
    
    temp = lapply(x,get)
    names(temp) = x
    x = temp
  
  }
  
  
  for (i in 1:length(x)){
    
    if(is.list(x[[i]])&&length(x[[i]]==1)){
      cat('Note: data component',names(x[i]),'is a 1-element list and is being converted to numeric.','\n')
      x[[i]] = x[[i]][[1]]
    }
    
    if(is.matrix(x[[i]])&&ncol(x[[i]])==1){
      cat('Note: data component',names(x[i]),'is a 1-column matrix and is being converted to vector.','\n')
      x[[i]] = x[[i]][,1]
    }  
        
    if(!is.numeric(x[[i]])){
      stop('Data component ',names(x[i]),' is not numeric. Try coercing it with as.numeric().')
    }
                
    }
 
  cat('Done.','\n','\n')
  return(list(data=data,params=params))
   
}