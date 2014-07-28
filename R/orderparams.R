
order.params <- function(samples,parameters.to.save,DIC){
  
  params <- colnames(samples[[1]])
  params <- params[order(match(sapply(strsplit(params, "\\["), "[", 1),
                               sapply(strsplit(parameters.to.save, "\\["), "[", 1)))]
  
  if(DIC&&('deviance'%in%params)){
    params <- c(params[params!='deviance'],'deviance')
  } else if (DIC&&!('deviance'%in%params)){
    warning('JAGS did not monitor deviance.')
    DIC <- FALSE
  } 
  
  samples <- samples[,params]
  
  return(samples)
  
}