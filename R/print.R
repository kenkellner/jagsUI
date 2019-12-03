#Print method for jagsUI output
print.jagsUI <- function(x,digits=3, tests=FALSE, bugs=FALSE, ...){
  
  if(bugs) return(bugs_print(x, digits))

  out <- as.data.frame(round(x$summary[,-c(4,6)], digits))

  #Diagnostics
  rh <- out$Rhat <= 1.05
  ef_lim <- 100*x$mcmc.info$n.chains
  ef <- out$n.eff >= ef_lim 
  rc <- ifelse(mean(rh)==1, ' \u2713','')
  ec <- ifelse(mean(ef)==1, ' \u2713','') 
  
  #Adaption check
  ad <- all(x$mcmc.info$sufficient.adapt)
  if(!ad) warning("Adaption period was too short")
  
  #Include tests if requested
  if(!tests){
    out$overlap0 <- out$f <- NULL
  } else {
    out$overlap0 <- as.logical(out$overlap0)
  }

  #Print info
  message(paste0("Inference for BUGS model: ",x$modfile), " (", 
                  x$mcmc.info$n.samples, " samples)")
  message(paste0('Rhat: ',sum(rh),'/',length(rh),' parameters (',
                 round(mean(rh)*100),'%) had split Rhat < 1.05',rc))
  message(paste0('ESS:  ',sum(ef),'/',length(ef),' parameters (',
                 round(mean(ef)*100),'%) had bulk n.eff > ',ef_lim,ec))
  message(paste0('DIC = ',round(x$DIC,digits),'; pD = ',round(x$pD,digits)))
  message()

  #Print posterior stats
  out$n.eff <- round(out$n.eff)
  names(out)[3:5] <- c('2.5%','50%','97.5%')
  print(out)
  invisible(out)
}

#Print format emulating old-school R2WinBUGS output
bugs_print <- function(x, digits=3){

  m <- x$mcmc.info
  out <- as.data.frame(round(x$summary,digits))

  message(paste0("Inference for Bugs model at '",x$modfile,
                 "', fit using JAGS,"))
  message(paste0(m$n.chains,' chains, each with ',m$n.iter,
                 ' iterations (first ',m$n.burnin,
                 ' discarded), n.thin = ',m$n.thin))
  message(paste0('n.sims = ',m$n.samples,' iterations saved'))

  out$overlap0 <- out$f <- NULL
  print(out)

  message()
  message(paste('For each parameter, n.eff is a crude measure of',
                'effective sample size,'))

  message(paste('and Rhat is the potential scale reduction factor', 
                '(at convergence, Rhat=1).'))
  message()
  message('DIC info (using the rule, pD = var(deviance/2)')
  message(paste0('pd = ',round(x$pD,digits),' and DIC = ',
                 round(x$DIC,digits)))
  message(paste0('DIC is an estimate of expected predictive error',
                 '(lower deviance is better).'))

  invisible(out)
}
