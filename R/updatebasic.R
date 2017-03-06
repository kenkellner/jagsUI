
update.jagsUIbasic <- function(object, parameters.to.save=NULL, n.adapt=NULL, n.iter, n.thin=NULL, 
                               modules=c('glm'), factories=NULL, DIC=NULL, seed=as.integer(Sys.time()), verbose=TRUE, ...){
  
  mod <- object$model
  n.chains <- length(object$samples)
  n.cores <- object$n.cores
 
  if(is.null(parameters.to.save)){
    params.temp <- colnames(object$samples[[1]])
    parameters <- unique(sapply(strsplit(params.temp, "\\["), "[", 1))
  } else {parameters <- parameters.to.save}
  
  #Set up DIC monitoring
  if(is.null(DIC)){
    if('deviance'%in%parameters){
      DIC=TRUE
    } else {DIC=FALSE}
  } else{
    if(DIC&!'deviance'%in%parameters){parameters <- c(parameters,'deviance')
    } else if(!DIC&'deviance'%in%parameters){parameters <- parameters[parameters!='deviance']}
  }
  
  if(is.null(n.thin)){n.thin <- thin(object$samples)}
  
  start.time <- Sys.time()
  
  if(names(object$model[1])=='cluster1'){
    
    par <- run.parallel(data=NULL,inits=NULL,parameters.to.save=parameters,model.file=NULL,n.chains=n.chains
                        ,n.adapt=n.adapt,n.iter=n.iter,n.burnin=0,n.thin=n.thin,modules=modules,factories=factories,
                        seed=seed,DIC=DIC,model.object=mod,update=TRUE,verbose=verbose,n.cores=n.cores) 
    samples <- par$samples
    m <- par$model
       
    } else {
    
    #Set modules
    set.modules(modules,DIC)
    set.factories(factories)
    
    rjags.output <- run.model(model.file=NULL,data=NULL,inits=NULL,parameters.to.save=parameters,
                              n.chains=object$mcmc.info$n.chains,n.iter,n.burnin=0,n.thin,n.adapt,
                              model.object=mod,update=TRUE,verbose=verbose)
    samples <- rjags.output$samples
    m <- rjags.output$m    
  }
  
  samples <- order.params(samples,parameters,DIC)
  
  end.time <- Sys.time() 
  time <- round(as.numeric(end.time-start.time,units="mins"),digits=3)
  if(verbose){cat('MCMC took',time,'minutes.\n')}
  
  output <- list(samples=samples,model=m,n.cores=n.cores)
  
  class(output) <- 'jagsUIbasic'
  
  return(output)
  
}