
update.jagsUIbasic <- function(object, parameters.to.save=NULL, n.adapt=NULL, 
                               n.iter, n.thin=NULL, modules=c('glm'), 
                               factories=NULL, DIC=NULL, verbose=TRUE, ...){
  
  #Convert to jagsUI class object to pass thru to update method
  object$parallel <- FALSE
  object$mcmc.info <- list()
  if(class(object$model) != 'jags') object$parallel <- TRUE
 
  parameters <- parameters.to.save
  if(is.null(parameters)){
    parameters <- param_names(object$samples, simplify=TRUE)
  }

  if(is.null(DIC)) DIC <- 'deviance' %in% parameters
  parameters <- check_params(parameters, DIC) 

  #Update object
  #No stats will be calculated since codaOnly = all parameters
  jags_out <- update.jagsUI(object, parameters, n.adapt, n.iter, n.thin, 
                            modules, factories, DIC, codaOnly = parameters, 
                            verbose=verbose)

  #Cleanup
  jags_out$n.cores <- jags_out$mcmc.info$n.cores
  to_remove <- c('sims.list','pD','DIC','summary','modfile','parameters',
                 'mcmc.info','run.date','parallel','bugs.format','calc.DIC',
                 'update.count')
  jags_out[to_remove] <- NULL
  class(jags_out) <- 'jagsUIbasic'
  
  jags_out
}
