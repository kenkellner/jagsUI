#This function gets the dimensions of non-scalar parameters 
#for which the user has requested posterior distributions.

get.dim <- function(params){
  
  #Get all unique parameters (i.e., collapse indexed non-scalars)
  ps <- unique(sapply(strsplit(params, "\\["), "[", 1)) 
  #Slice indexes from non-scalar parameter entries
  test <- sapply(strsplit(params, "\\["), "[", 1)
  
  #Calculate dimension for each parameter i
  dim <- lapply(ps, function(i){
    
    #Extract indices from each element j of parameter i
    w <- params[test==i]
    getinds <- lapply(w,FUN=function(j){
      
      w2 <- strsplit(j,'\\[')[[1]][2]
      w3 <- strsplit(w2,"\\]")[[1]] 
      w4 <- as.numeric(unlist(strsplit(w3,",")))
      return(w4)
      
    })
    
    #Get max value from each dimension of i
    collapsedinds <- do.call(rbind,getinds)
    apply(collapsedinds,2,max)  
        
  })
  
  names(dim) = ps
  dim
  
}