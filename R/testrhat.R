
test.Rhat <- function(samples,cutoff){
  
  params <- colnames(samples[[1]])
  
  gd <- function(hold){
    r <- gelman.diag(hold,autoburnin=FALSE)$psrf[1]
    if(is.nan(r)){r <- NA}
    return(r)
  }
  
  failure <- FALSE
  index <- 1
  while (failure==FALSE && index <= length(params)){
    test <- gd(samples[,index])
    if(test>cutoff){failure=TRUE
    } else {index <- index + 1}
  }
  
  if(failure==TRUE){
    cat('.......Convergence check failed for parameter \'',params[index],'\'\n',sep="")
  }
  if(failure==FALSE){
    cat('.......All parameters converged.','\n\n')
  }
  
  return(failure)
  
}