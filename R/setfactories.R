
set.factories <- function(factories){
  
  if(!is.null(factories)){
    for (i in 1:length(factories)){
    
      split <- strsplit(factories[i],'\\s')[[1]]
    
      #Check if requested factory is available
      faclist <- as.character(list.factories(split[2])[,1])
      if(split[1]%in%faclist){
        
        null <- set.factory(split[1],split[2],split[3])
      
      } else{stop(paste('Requested factory',split[1],'is not available. Check that appropriate modules are loaded.'))}
    
    }
  }
  
}