#This function gets the dimensions of non-scalar parameters 
#for which the user has requested posterior distributions.
#Called only by process.output()

get.dim <- function(params){
  
  #Get all unique parameters (i.e., collapse indexed non-scalars)
  ps <- unique(sapply(strsplit(params, "\\["), "[", 1)) 
  #Slice indexes from non-scalar parameter entries
  test <- sapply(strsplit(params, "\\["), "[", 1)
  
  dim = list()
  
  #Get dimensions for each parameter
  #Scalar parameters have dimension 'NA'
  for (i in 1:length(ps)){
    
    w = params[test==ps[i]]
    l = length(w)
    
    w2  =strsplit(w,'\\[')[[l]][2]    
    w3 = strsplit(w2,"\\]")[[1]]    
    dim[[i]] = as.numeric(unlist(strsplit(w3,",")))    
  }  
  names(dim) = ps
  dim
  
}