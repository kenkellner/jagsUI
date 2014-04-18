get.dim <- function(params){
  
  ps <- unique(sapply(strsplit(params, "\\["), "[", 1)) 
  test <- sapply(strsplit(params, "\\["), "[", 1)
  
  dim = list()
  
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